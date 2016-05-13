##data as a plot
uis<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/uis.csv", sep=",", header = TRUE)
uis=uis[complete.cases(uis),]
im1=subset(uis,uis$censor==1)
im.sample=im1[sample(nrow(im1), 11), ]
im.sample2=im.sample[,-1]
display(as.matrix(im.sample2))

##missing time and event
N  <-  round(nrow(uis)*0.1)                                
inds <- round(runif(N,1,length(uis$censor)))   
uis$censor[inds] <- 0
time_real=uis$time[inds]
uis$time[inds]<-median(uis$time)
time_imp=uis$time[inds]

bardata=as.data.frame(cbind(1:length(time_real),time_real))
p=ggplot(bardata, aes(x=V1, y=time_real)) +
  geom_bar(stat='identity') 
p+geom_hline(aes(yintercept = 170))+coord_flip()

coxph( Surv(time, censor)~as.factor(ivhx)+age, method="breslow", data=uis)

uis$censor[inds] <- NA
uis$time[inds]<-NA

uis$censor=as.factor(uis$censor)
rf.impute=missForest(uis)

rf.data=rf.impute$ximp
time_real=rf.data$time[inds]
rfbardata=as.data.frame(cbind(1:length(time_real),time_real))
coxph( Surv(time, as.numeric(censor))~as.factor(ivhx)+age, method="breslow", data=rf.data)

mice.imp=mice(uis,method="fastpmm")

mice.data=complete(mice.imp,1)
time_real=mice.data$time[inds]
micebardata=as.data.frame(cbind(1:length(time_real),time_real))
coxph( Surv(time, as.numeric(censor))~as.factor(ivhx)+age, method="breslow", data=mice.data)

##plot
p=ggplot(NULL,aes(x=V1,y=time_real))+
  geom_bar(stat="identity",aes(fill="real"),data=bardata)+
  geom_bar(stat="identity",aes(fill="MICE"),data=micebardata,alpha=0.8)+
  geom_point(stat="identity",data=rfbardata)
p+geom_hline(aes(yintercept = 170))+coord_flip()
postResample(rfbardata$time_real,time_real)
postResample(micebardata$time_real,time_real)
postResample(time_imp,time_real)


