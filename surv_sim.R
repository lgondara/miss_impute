require(survsim)
require(missForest)
dist.ev <- "weibull"
anc.ev <- 1
beta0.ev <- 5.268
dist.cens <- "weibull"
anc.cens <- 1
beta0.cens <- 5.368
x <- list(c("bern", 0.3), c("bern", 0.4))
beta <- list(-0.4, -0.25)
simple.dat <- simple.surv.sim(500, 365, dist.ev, anc.ev, beta0.ev,dist.cens, anc.cens, beta0.cens, , beta, x)

survreg(Surv(stop, status) ~ 1 + x + x.1, data=simple.dat, dist="weibull")

miss.surv1=prodNA(simple.dat,0.5)
survreg(Surv(stop, status) ~ 1 + x + x.1, data=miss.surv1, dist="weibull")

#data set 1
uis<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/uis.csv", sep=",", header = TRUE)
uis=uis[complete.cases(uis),]
coxph( Surv(time, censor)~as.factor(ivhx)+age, method="breslow", data=uis)

N  <-  50                                   
inds <- round(runif(N,1,length(uis$censor)))   
uis$censor[inds] <- 0 
coxph( Surv(time, censor)~as.factor(ivhx)+age, method="breslow", data=uis)


uis$censor[(uis$age)>32 & (uis$ivhx)==3] <- 0
coxph( Surv(time, censor)~as.factor(ivhx)+age, method="breslow", data=uis)


uis$censor[(uis$age)>32 & (uis$ivhx)==3] <- NA
uis.imp=missForest(uis)

uis.imp.data=uis.imp$ximp
coxph( Surv(time, round(censor))~as.factor(round(ivhx))+age, method="breslow", data=uis.imp.data)

##Competing risk
##MAR
N  <-  50                                   
inds <- round(runif(N,1,length(uis$censor)))   
uis$censor[inds] <- 2 
x=model.matrix(~as.factor(ivhx)+age,data=uis)
x2=x[,-1]
crr(uis$time,uis$censor,x2,failcode=1, cencode=0)

##MNAR
uis$censor[(uis$age)>32 & (uis$ivhx)==3] <- 2
x=model.matrix(~as.factor(ivhx)+age,data=uis)
x2=x[,-1]
crr(uis$time,uis$censor,x2,failcode=1, cencode=0)
