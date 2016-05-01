
dl_impute <- function(xmis, maxiter = 5, ntree = 100, variablewise = FALSE,
                        decreasing = FALSE, verbose = FALSE)
{ 
  require(randomForest)
  require(caret)
  require(darch)
  n <- nrow(xmis)
  p <- ncol(xmis)
  
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
  
  
  
    
    for (s in 1:p) {
      varInd <- sort.j[2]
      if (noNAvar[[varInd]] != 0) {
        obsi <- !NAloc[, varInd]
        misi <- NAloc[, varInd]
        obsY <- ximp[obsi, varInd]
        obsX <- ximp[obsi, seq(1, p)[-varInd]]
        misX <- ximp[misi, seq(1, p)[-varInd]]
        typeY <- varType[varInd]
        if (typeY == "numeric") {
        
            darch <- darch(data.matrix(obsX),obsY,
                           preProc.params = list(method = c("center", "scale")),
                           preProc.targets = T,
                           layers = c(ncol(obsX),20,50,20,1),
                           darch.batchSize =10,
                           bp.learnRate = .001,
                           darch.isClass = F,
                           darch.numEpochs = 100,
                           darch.unitFunction = linearUnit)
            misY=predict(darch,newdata=data.matrix(misX),type="raw")

          
        } else {
          obsY <- factor(obsY)
          summarY <- summary(obsY)
          if (length(summarY) == 1) {
            misY <- factor(rep(names(summarY), sum(misi)))
          } else {
           

              darch <- darch(obsX,obsY,
                             layers = c(ncol(obsX),200,300,200,nlevels(obsY)),
                             darch.batchSize =  nlevels(obsY),
                             darch.dropout = .05,
                             darch.dropout.oneMaskPerEpoch = T,
                             bp.learnRate = .001,
                             darch.isClass = T,
                             darch.fineTuneFunction = "backpropagation",
                             darch.unitFunction = c(maxoutUnit,maxoutUnit,maxoutUnit,softmaxUnit),
                             darch.numEpochs = 200)
              misY=predict(darch,newdata=misX, type="class")
              
            
            
            ## predict missing parts of Y
            
          }
        }
        ximp[misi, varInd] <- misY
      }
    }
    cat('done!\n')
}

data("BreastCancer")
missbc=prodNA(BreastCancer[,-1],0.02)
d=dl_impute(missbc)
dldata=ximp


f=missForest(missbc)
rfdata=f$ximp
