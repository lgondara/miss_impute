install.packages("ripa")
install.packages("randomForest")
require(ripa)
require(randomForest)
data(logo)
plot(logo)
dim(logo)
head(logo)
logo2=logo
sample.logo=imagematrix(logo[1:70,])

# MCAR
prop.m = .07  
mcar   = runif(77, min=0, max=1)
logo2[,3]= ifelse(mcar<prop.m, 0, logo2[,3]) 
logo2[,33]= ifelse(mcar<prop.m, 0, logo2[,33]) # unrelated to anything
plot(logo2)
logo2.matrix=matrix(logo2,nrow=nrow(logo2),ncol=ncol(logo2))

ozone.rf <- randomForest(logo2.matrix[,3] ~ ., data=logo2.matrix, mtry=3,importance=TRUE, na.action=na.omit)
rf.pred=predict(ozone.rf)
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

