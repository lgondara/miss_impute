

mult_impute <- function(xmis, maxiter = 2, ntree = 100, variablewise = FALSE,
                        decreasing = FALSE, verbose = FALSE,
                        mtry = floor(sqrt(ncol(xmis))), replace = TRUE,
                        classwt = NULL, cutoff = NULL, strata = NULL,
                        sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                        xtrue = NA, modelUse="RF")
{ 
  require(randomForest)
  require(e1071)
  require(caret)
  n <- nrow(xmis)
  p <- ncol(xmis)
  if (!is.null(classwt))
    stopifnot(length(classwt) == p, typeof(classwt) == 'list')
  if (!is.null(cutoff))
    stopifnot(length(cutoff) == p, typeof(cutoff) == 'list')
  if (!is.null(strata))
    stopifnot(length(strata) == p, typeof(strata) == 'list')
  if (!is.null(nodesize))
    stopifnot(length(nodesize) == 2)
  
  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == n)){
    indCmis <- which(apply(is.na(xmis), 2, sum) == n)
    xmis <- xmis[,-indCmis]
    p <- ncol(xmis)
    cat('  removed variable(s)', indCmis,
        'due to the missingness of all entries\n')
  } 
  
  
  ## perform initial S.W.A.G. on xmis (mean imputation)
  ximp <- xmis
  xAttrib <- lapply(xmis, attributes)
  varType <- character(p)
  for (t.co in 1:p){
    if (is.null(xAttrib[[t.co]])){
      varType[t.co] <- 'numeric'
      ximp[is.na(xmis[,t.co]),t.co] <- mean(xmis[,t.co], na.rm = TRUE)
    } else {
      varType[t.co] <- 'factor'
      ## take the level which is more 'likely' (majority vote)
      max.level <- max(table(ximp[,t.co]))
      ## if there are several classes which are major, sample one at random
      class.assign <- sample(names(which(max.level == summary(ximp[,t.co]))), 1)
      ## it shouldn't be the NA class
      if (class.assign != "NA's"){
        ximp[is.na(xmis[,t.co]),t.co] <- class.assign
      } else {
        while (class.assign == "NA's"){
          class.assign <- sample(names(which(max.level ==
                                               summary(ximp[,t.co]))), 1)
        }
        ximp[is.na(xmis[,t.co]),t.co] <- class.assign
      }
    }
  }
  
  ## extract missingness pattern
  NAloc <- is.na(xmis)            # where are missings
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  sort.j <- order(noNAvar)        # indices of increasing amount of NA in vars
  if (decreasing)
    sort.j <- rev(sort.j)
  sort.noNAvar <- noNAvar[sort.j]
  
  
  ## output
  Ximp <- vector('list', maxiter)
  
  ## initialize parameters of interest
  iter <- 0
  k <- length(unique(varType))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  OOBerror <- numeric(p)
  names(OOBerror) <- varType
  
  ## setup convergence variables w.r.t. variable types
  if (k == 1){
    if (unique(varType) == 'numeric'){
      names(convNew) <- c('numeric')
    } else {
      names(convNew) <- c('factor')
    }
    convergence <- c()
    OOBerr <- numeric(1)
  } else {
    names(convNew) <- c('numeric', 'factor')
    convergence <- matrix(NA, ncol = 2)
    OOBerr <- numeric(2)
  }
  
  ## function to yield the stopping criterion in the following 'while' loop
  stopCriterion <- function(varType, convNew, convOld, iter, maxiter){
    k <- length(unique(varType))
    if (k == 1){
      (convNew < convOld) & (iter < maxiter)
    } else {
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
    }
  }
  
  ## iterate missForest
  while (stopCriterion(varType, convNew, convOld, iter, maxiter)){
    if (iter != 0){
      convOld <- convNew
      OOBerrOld <- OOBerr
    }
    cat("  missForest iteration", iter+1, "in progress...")
    t.start <- proc.time()
    ximp.old <- ximp
    
    for (s in 1:p) {
      varInd <- sort.j[s]
      if (noNAvar[[varInd]] != 0) {
        obsi <- !NAloc[, varInd]
        misi <- NAloc[, varInd]
        obsY <- ximp[obsi, varInd]
        obsX <- ximp[obsi, seq(1, p)[-varInd]]
        misX <- ximp[misi, seq(1, p)[-varInd]]
        typeY <- varType[varInd]
        if (typeY == "numeric") {
          if (modelUse == "RF"){
            
            RF <- randomForest( x = obsX,
                                y = obsY,
                                ntree = ntree,
                                mtry = mtry,
                                replace = replace,
                                sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                  if (replace) nrow(obsX) else ceiling(0.632*nrow(obsX)),
                                nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
                                maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
            ## record out-of-bag error
            OOBerror[varInd] <- RF$mse[ntree]
            misY <- predict(RF, misX)
          }
          if (modelUse == "svm"){
            RF <- svm(obsX,as.matrix(obsY), scale = F)
            misY=predict(RF,misX)
          }
          if (modelUse=="dn"){
            darch <- darch(data.matrix(obsX),obsY,
                           preProc.params = list(method = c("center", "scale")),
                           preProc.targets = T,
                           layers = c(ncol(obsX),20,50,20,1),
                           darch.batchSize =  round(nrow(obsX)/10),
                           bp.learnRate = .01,
                           darch.isClass = F,
                           darch.numEpochs = 100,
                           darch.unitFunction = linearUnit)
            misY=predict(darch,newdata=misX,type="raw")
          }
          
        } else {
          obsY <- factor(obsY)
          summarY <- summary(obsY)
          if (length(summarY) == 1) {
            misY <- factor(rep(names(summarY), sum(misi)))
          } else {
            if (modelUse=="RF"){
              
              RF <- randomForest(x = obsX, 
                                 y = obsY, 
                                 ntree = ntree, 
                                 mtry = mtry, 
                                 replace = replace, 
                                 classwt = if (!is.null(classwt)) classwt[[varInd]] else 
                                   rep(1, nlevels(obsY)),
                                 cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else 
                                   rep(1/nlevels(obsY), nlevels(obsY)),
                                 strata = if (!is.null(strata)) strata[[varInd]] else obsY, 
                                 sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else 
                                   if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)), 
                                 nodesize = if (!is.null(nodesize)) nodesize[2] else 5, 
                                 maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
              ## record out-of-bag error
              OOBerror[varInd] <- RF$err.rate[[ntree, 1]]
              misY <- predict(RF, misX)
            }
            
            if (modelUse == "svm"){
              RF <- svm(obsX,as.matrix(obsY), type="C", scale = F)
              misY=predict(RF,misX)
            }
            
            if(modelUse=="dn"){
              darch <- darch(obsX,obsY,
                             preProc.params = list(method = c("center", "scale")),
                             layers = c(ncol(obsX),200,300,200,nlevels(obsY)),
                             darch.batchSize =  nlevels(obsY),
                             darch.dropout = .05,
                             darch.dropout.oneMaskPerEpoch = T,
                             bp.learnRate = .01,
                             darch.isClass = T,
                             darch.fineTuneFunction = "backpropagation",
                             darch.unitFunction = c(maxoutUnit,maxoutUnit,maxoutUnit,softmaxUnit),
                             darch.numEpochs = 200)
              misY=predict(darch,newdata=misX, type="class")
              
            }
            
            ## predict missing parts of Y
            
          }
        }
        ximp[misi, varInd] <- misY
      }
    }
    cat('done!\n')
    
    iter <- iter+1
    Ximp[[iter]] <- ximp
    
    t.co2 <- 1
    ## check the difference between iteration steps
    for (t.type in names(convNew)){
      t.ind <- which(varType == t.type)
      if (t.type == "numeric"){
        convNew[t.co2] <- sum((ximp[,t.ind]-ximp.old[,t.ind])^2)/sum(ximp[,t.ind]^2)
      } else {
        dist <- sum(as.character(as.matrix(ximp[,t.ind])) != as.character(as.matrix(ximp.old[,t.ind])))
        convNew[t.co2] <- dist / (n * sum(varType == 'factor'))
      }
      t.co2 <- t.co2 + 1
    }
    
    ## compute estimated imputation error
    if (!variablewise){
      NRMSE <- sqrt(mean(OOBerror[varType=='numeric'])/
                      var(as.vector(as.matrix(xmis[,varType=='numeric'])),
                          na.rm = TRUE))
      PFC <- mean(OOBerror[varType=='factor'])
      if (k==1){
        if (unique(varType)=='numeric'){
          OOBerr <- NRMSE
          names(OOBerr) <- 'NRMSE'
        } else {
          OOBerr <- PFC
          names(OOBerr) <- 'PFC'
        }
      } else {
        OOBerr <- c(NRMSE, PFC)
        names(OOBerr) <- c('NRMSE', 'PFC')
      }
    } else {
      OOBerr <- OOBerror
      names(OOBerr)[varType=='numeric'] <- 'MSE'
      names(OOBerr)[varType=='factor'] <- 'PFC'
    }
    
    if (any(!is.na(xtrue))){
      err <- suppressWarnings(mixError(ximp, xmis, xtrue))
    }
    
    ## return status output, if desired
    if (verbose){
      delta.start <- proc.time() - t.start
      if (any(!is.na(xtrue))){
        cat("    error(s):", err, "\n")
      }
      cat("    estimated error(s):", OOBerr, "\n")
      cat("    difference(s):", convNew, "\n")
      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }#end while((convNew<convOld)&(iter<maxiter)){
  
  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr)
    } else {
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr, error = err)
    }
  } else {
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter-1]], OOBerror = OOBerrOld)
    } else {
      out <- list(ximp = Ximp[[iter-1]], OOBerror = OOBerrOld,
                  error = suppressWarnings(mixError(Ximp[[iter-1]], xmis, xtrue)))
    }
  }
  class(out) <- 'mult_impute'
  return(out)
}

data(nhanes)
data("nhanes2")
imp.rf=mult_impute(nhanesnew,modelUse="RF", verbose = TRUE)
imp.svm=mult_impute(nhanesnew,modelUse = "svm", verbose = TRUE)
imp.dl=mult_impute(nhanesnew,modelUse = "dn", verbose = TRUE)
imp.data=imp.svm$ximp

require(missForest)
b=data(BreastCancer)
newb=prodNA(Glass,0.05)
buse=newb[,-1]
imp.rf=mult_impute(newb,modelUse="svm", verbose = TRUE)
missForest(newb)
