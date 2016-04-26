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


darch <- darch(Hwt ~ Bwt,
               cats[1:100,],
               preProc.params = list(method = c("center", "scale")),
               preProc.targets = T,
               layers = c(1,20,50,20,1),
               darch.batchSize =  10,
               bp.learnRate = .01,
               darch.isClass = F,
               darch.numEpochs = 100,
               darch.unitFunction = linearUnit)

pred=predict(darch,newdata=cats[101:144,], type="raw")
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
