

reg_impute <- function(xmis, maxiter = 1, ntree = 100, verbose = FALSE,xtrue=NA)
{ 
  require(e1071)
  require(caret)
  n <- nrow(xmis)
  p <- ncol(xmis)
  ##regression
  
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
          print(s)
          
          RF <- cv.glmnet(data.matrix(obsX),obsY)
          misY <- predict(RF, data.matrix(misX), s = "lambda.min")
          
          
        } else {
          obsY <- factor(obsY)
          summarY <- summary(obsY)
          if (length(summarY) == 1) {
            misY <- factor(rep(names(summarY), sum(misi)))
          } else {
            
            
            RF <- svm(data.matrix(obsX),obsY, type="C", scale = F)
            misY <- predict(RF,data.matrix(misX))
            
            
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
  class(out) <- 'reg_impute'
  return(out)
}
