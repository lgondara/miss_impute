####darch
require(devtools)
install.packages("lme4")
require(lme4)
install_github("maddin79/darch")
require(darch)
require(mlbench)
require(randomForest)
require(bartMachine)

example.regression <- function(...)
{
  library(MASS)
  library(caret)
  data(cats)
  
  darch <- darch(Hwt ~ Bwt,
                 cats[1:100,],
                 preProc.params = list(method = c("center", "scale")),
                 preProc.targets = T,
                 layers = c(1,20,50,20,1),
                 darch.batchSize =  10,
                 bp.learnRate = .01,
                 darch.isClass = F,
                 darch.numEpochs = 100,
                 darch.unitFunction = linearUnit,
                 ...)
  
  print(darchTest(darch, newdata = cats[101:144,]))
  
  darch
}
example.regression()

##cat dataset

darch <- darch(obsX,obsY,
               preProc.params = list(method = c("center", "scale")),
               preProc.targets = T,
               layers = c(ncol(obsX),20,50,20,1),
               darch.batchSize =  10,
               bp.learnRate = .01,
               darch.isClass = F,
               darch.numEpochs = 100,
               darch.unitFunction = linearUnit)

pred=predict(darch,misX, type="raw")
sqrt(mean(cats[101:144,3]-pred)^2)

lmodel=lm(Hwt~Bwt,data=cats[1:100,])
pred.lm=predict.lm(lmodel,cats[101:144,])
sqrt(mean(cats[101:144,3]-pred.lm)^2)

model.rf <- randomForest(Hwt~Bwt,data=cats[1:100,])
model.rf.pred=predict(model.rf,cats[101:144,])
sqrt(mean((model.rf.pred-cats[101:144,3])^2))

cat.train=as.data.frame(cats[1:100,2])
colnames(cat.train)=c("bwt")
cat.test=as.data.frame(cats[101:144,2])
colnames(cat.test)=c("bwt")
model.bm=bartMachine(cat.train,cats[1:100,3])
predict.bm=predict(model.bm, cat.test)
sqrt(mean((predict.bm-cats[101:144,3])^2))


##glass dataset
data("Glass")
glass_shuffle=Glass[sample(nrow(Glass)),]
glassX=as.matrix(glass_shuffle[1:150,1:9])
glassY=glass_shuffle[1:150,10]
darch <- darch(glassX,glassY,
               preProc.params = list(method = c("center", "scale")),
               layers = c(9,200,300,100,6),
               darch.batchSize =  6,
               darch.dropout = .1,
               darch.dropout.oneMaskPerEpoch = T,
               bp.learnRate = .007,
               darch.isClass = T,
               darch.fineTuneFunction = "backpropagation",
               darch.unitFunction = c(maxoutUnit,maxoutUnit,maxoutUnit,softmaxUnit),
               darch.numEpochs = 500)


pred=predict(darch,newdata=glass_shuffle[151:214,1:9], type="class")
postResample(pred,glass_shuffle[151:214,10])

model.rf <- randomForest(glassX,glassY)
model.rf.pred=predict(model.rf,glass_shuffle[151:214,1:9])
postResample(model.rf.pred,glass_shuffle[151:214,10])


##MNIST try
# MNIST example with pre-training
example.mnist <- function(dataFolder = "data/", downloadMNIST = T, ...)
{
  # Make sure to prove the correct folder if you have already downloaded the
  # MNIST data somewhere, or otherwise set downloadMNIST to TRUE
  provideMNIST(dataFolder, downloadMNIST)
  
  # Load MNIST data
  load(paste0(dataFolder, "train.RData")) # trainData, trainLabels
  load(paste0(dataFolder, "test.RData")) # testData, testLabels
  
  # only take 1000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  darch  <- darch(trainDataSmall, trainLabelsSmall,
                  rbm.numEpochs = 5,
                  rbm.consecutive = F, # each RBM is trained one epoch at a time
                  rbm.batchSize = 100,
                  rbm.lastLayer = -1, # don't train output layer
                  rbm.allData = T, # use bootstrap validation data as well for training
                  rbm.errorFunction = rmseError,
                  rbm.initialMomentum = .5,
                  rbm.finalMomentum = .7,
                  rbm.learnRate = .1,
                  rbm.learnRateScale = .98,
                  rbm.momentumRampLength = .8,
                  rbm.numCD = 2,
                  rbm.unitFunction = sigmoidUnitRbm,
                  rbm.weightDecay = .001,
                  layers = c(784,100,10),
                  darch.batchSize = 100,
                  darch.dither = T,
                  darch.initialMomentum = .4,
                  darch.finalMomentum = .9,
                  darch.momentumRampLength = .75,
                  bp.learnRate = 1,
                  bp.learnRateScale = .99,
                  darch.unitFunction = c(tanhUnit, softmaxUnit),
                  bootstrap = T,
                  darch.numEpochs = 20,
                  gputools = T, # try to use gputools
                  gputools.deviceId = 0,
                  ...
  )
  
  predictions <- predict(darch, newdata=testData, type="class")
  
  labels <- cbind(predictions, testLabels)
  numIncorrect <- sum(apply(labels, 1, function(i) { any(i[1:10] != i[11:20]) }))
  cat(paste0("Incorrect classifications on test data: ", numIncorrect,
             " (", round(numIncorrect/nrow(testLabels)*100, 2), "%)\n"))
  
  darch
}
example.mnist()
