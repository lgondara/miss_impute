install.packages("ripa")
install.packages("randomForest")
install.packages("bartMachine")
require(EBImage)
require(randomForest)
require(bartMachine)
require(mxnet)
require(ripa)
Image <- readImage('~/sop.JPG')
display(Image)
dim(Image)
g.Image=Image(Image, c(506,316), "Grayscale")
display(g.Image)
g.Image.d=as.data.frame(g.Image)
I=as.Image(g.Image.d)
display(g.Image.d)
# MCAR
prop.m = .07  
mcar   = runif(nrow(g.Image.d), min=0, max=1)
g.Image.d[,3]= ifelse(mcar<prop.m, 0, g.Image.d[,3]) 
display(as.Image(g.Image.d))
data(logo)
logo2=logo
logo2[,33]= ifelse(mcar<prop.m, 0, logo2[,33]) # unrelated to anything
plot(logo2)
logo2.matrix=matrix(logo2,nrow=nrow(logo2),ncol=ncol(logo2))
logo2.matrix2=cbind(1:nrow(logo2.matrix),logo2.matrix)

logo.rf <- randomForest(logo2.matrix[,3] ~ ., data=logo2.matrix, mtry=3,importance=TRUE, na.action=na.omit)
rf.pred=predict(logo.rf)
pred.value=matrix(rf.pred,nrow=nrow(logo2.matrix),ncol=1)
for(i in 1:nrow(logo2.matrix))
{
  if(logo2.matrix[,3][i]==0)
  {
    logo2.matrix[,3][i]=pred.value[i]
  }
}

logo.imp1=imagematrix(logo2.matrix)
plot(logo.imp1)

x=as.data.frame(cbind(logo2.matrix[,1:2],logo2.matrix[,4:101]))
y=logo2.matrix[,3]
bart_machine = bartMachine(x,y)
y_hat = predict(bart_machine, x)

pred.value=matrix(y_hat,nrow=nrow(logo2.matrix),ncol=1)
for(i in 1:nrow(logo2.matrix))
{
  if(logo2.matrix[,3][i]==0)
  {
    logo2.matrix[,3][i]=pred.value[i]
  }
}

logo.imp1=imagematrix(logo2.matrix)
plot(logo.imp1)


###deep learning
logo2.matrix=as.data.frame(matrix(logo2,nrow=nrow(logo2),ncol=ncol(logo2)))
logo2.matrix2=cbind(1:nrow(logo2.matrix),logo2.matrix)

train= subset(logo2,V3!=0)
train.y=as.data.frame((train[,3]))
names(train.y) <- gsub("(train[, 3])", "Y", names(train.y))
train.x=(cbind(train[,1:2],train[,4:101]))
names(train.x) <- gsub("V", "X", names(train.x))

test= subset(logo2.matrix,V3==0)
test.y=test[,3]
test.x=cbind(test[,1:2],test[,4:101])
mx.set.seed(0)
model <- mx.mlp(train.x, train.y, hidden_node=10, out_node=2, out_activation="softmax",
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                eval.metric=mx.metric.accuracy)


###plyt with simple  datasets first 
data(BostonHousing, package="mlbench")

train.ind = seq(1, 506, 3)
train=data.frame(BostonHousing[train.ind,])
test=data.frame(BostonHousing[-train.ind,])
train.x = data.matrix(BostonHousing[train.ind, -14])
train.y = BostonHousing[train.ind, 14]
test.x = data.matrix(BostonHousing[-train.ind, -14])
test.y = BostonHousing[-train.ind, 14]

# Define the input data
data <- mx.symbol.Variable("data")
# A fully connected hidden layer
# data: input source
# num_hidden: number of neurons in this hidden layer
fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)


# Use linear regression for the output layer
lro <- mx.symbol.LinearRegressionOutput(fc1)

mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, X=train.x, y=train.y,
                                     ctx=mx.cpu(), num.round=50, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9, eval.metric=mx.metric.rmse)

model <- mx.mlp(train.x, train.y,
                hidden_node=10, out_node=2,
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                out_activation = "relu", 
                , eval.metric=mx.metric.rmse)

preds = t(predict(model, test.x))
sqrt(mean((preds-test.y)^2))
graph.viz(model$symbol$as.json())

# random forest
model.rf <- randomForest(train$medv ~ .,data=train, mtry=3,importance=TRUE, na.action=na.omit)
model.rf.pred=predict(model.rf,test)
sqrt(mean((model.rf.pred-test$medv)^2))



####
logo.matrix=as.matrix(logo)
plot(logo.matrix)
misslogo=prodNA(logo.matrix,0.05)
plot(misslogo)

display(logo)


###try ebimage
require(EBImage)
f = system.file("images", "sample.png", package="EBImage")
Image <- readImage('C:/Users/ldpc/Downloads/panda.jpg')
display(Image)
dim(img)
img.data=imageData(Image)
display(img.data)

img.data.miss=prodNA(img.data,0.5)
display(img.data.miss)

img.dat=as.data.frame(img.data.miss)

missf.img1=missForest(img.dat,maxiter=1)

misssvm.img1=svm_impute(img.dat)
comp.data=misssvm.img1$ximp
display(as.matrix(comp.data))


comp.data=missf.img1$ximp
display(as.matrix(comp.data))

missf.img2=dl_impute(img.data.miss)
comp.data.2=missf.img2
display(as.matrix(comp.data.2))

img.data.miss=prodNA(img.data,0.2)
display(img.data.miss)

img.dat=as.data.frame(img.data.miss)

missf.img1=missForest(img.dat,maxiter=1)
comp.data.3=missf.img1$ximp
display(as.matrix(img.data.miss))


lasso.imp=reg_impute(img.dat)
comp.data.4=lasso.imp$ximp
display(as.matrix(comp.data.4))


###image missing a chunk ###
img.data[200:250,200:250]=NA
display(as.matrix(img.data))

missf.img1=missForest(img.data,maxiter=5)

imp.data=missf.img1$ximp
display(as.matrix(imp.data))

lasso.imp=reg_impute(img.data,maxiter = 5)
imp.data2=lasso.imp$ximp

display(as.matrix(imp.data2))


dl.imp=dl_impute(img.data)
imp.data3=dl.imp$ximp
display(as.matrix(imp.data3))
