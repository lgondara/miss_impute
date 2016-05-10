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

uis<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/uis.csv", sep=",", header = TRUE)
mod.ph <- coxph( Surv(time, censor)~as.factor(ivhx)+age, method="breslow", data=uis)
mod.ph

miss.surv2=prodNA(uis,0.1)
mod.ph <- coxph( Surv(time, censor)~as.factor(ivhx)+age, method="breslow", data=miss.surv2)
mod.ph
