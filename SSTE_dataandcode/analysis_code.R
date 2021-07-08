library(R2WinBUGS)
library(spdep)
library(maptools)
library(car)
library(RColorBrewer)
dat<-readShapePoly("T:/users/ozd504/Work/Papers_Manuscripts/SACrime/datafinal6911_joinwroad_abc.shp", 
proj4string=CRS("+proj=utm zone=14"))
saol<-readShapeLines( "T:/users/ozd504/Work/Papers_Manuscripts/SACrime/sa_l.shp", 
proj4string=CRS("+proj=utm zone=14"))
bexol<-readShapeLines( "T:/users/ozd504/Work/Grants/MissionsNSF/Missions/BexarCnty_Project.shp", 
proj4string=CRS("+proj=utm zone=14"))

setwd("C:/Workspace/sacrimework/")
#setwd("T:/users/ozd504/Work/Papers_Manuscripts/SACrime/")


#dont neeed to do this*
#NE_bad
dat$zblack<-as.numeric(scale(dat$pblack_1, center=T, scale=T))
dat$z_femkid<-as.numeric(scale(dat$pfemhhwkid, center=T, scale=T))
dat$z_pubass<-as.numeric(scale(dat$phhpubassi, center=T, scale=T))

dat$z_pov<-as.numeric(scale(dat$ppersonspo, center=T, scale=T))
dat$z_unemp<-as.numeric(scale(dat$punemp, center=T, scale=T))

#NE_good
dat$z_hiedu<-as.numeric(scale(dat$p25pluspro, center=T, scale=T))
dat$z_inc<-as.numeric(scale(dat$medhhinc_1, center=T, scale=T))
dat$z_hiwork<-as.numeric(scale(dat$pmgmtprofo, center=T, scale=T))

dat$ne_bad<-dat$z_pov+dat$z_unemp+dat$z_femkid+dat$z_pubass+(-1*dat$z_hiedu)+(-1*dat$z_inc)+(-1*dat$z_hiwork)
library(epicalc)
alpha(c("z_pov", "z_unemp", "z_femkid", "z_pubass", "z_hiedu", "z_inc", "z_hiwork"), dat@data)

dat$vcrimert<-dat$viol3yr
dat$pcrimert<-dat$prop3yr
dat$expvcrime<-dat$POPSIZE*(sum(dat$vcrimert)/sum(dat$POPSIZE))
dat$exppcrime<-dat$POPSIZE*(sum(dat$pcrimert)/sum(dat$POPSIZE))

dat$ziso<-as.numeric(scale(dat$isol, center=T, scale=T))
dat$zdis<-as.numeric(scale(dat$dissim, center=T, scale=T))
dat$zforn<-as.numeric(scale(dat$pforborn_1, center=T, scale=T))
dat$zresins<-as.numeric(scale(dat$p5yrinmig, center=T, scale=T))
names(dat)


dat$hwy<-ifelse(dat$NEAR_DIST>200,0,1)
dat$inten<-dat$MEAN
dat$landmix<-dat$MEAN_1
dat$abcpc<-1000*(dat$Count_2/dat$POPSIZE)

#writePolyShape(dat, "T:/users/ozd504/Work/Papers_Manuscripts/SACrime/datafile69.shp")

dat$zabc<-as.numeric(scale(dat$abcpc, center=T, scale=T))
dat$zinten<-as.numeric(scale(dat$inten, center=T, scale=T))
dat$logvcrime<-log(dat$vcrimert/dat$POPSIZE)
dat$logpcrime<-log(dat$pcrimert/dat$POPSIZE)
sanb<-poly2nb(dat, queen=F)
salw<-nb2listw(sanb, style="W")
sawt<-nb2WB(sanb)

library(MASS)
fit0<-glm(vcrimert~offset(log(expvcrime))+zabc+ziso+zforn+ne_bad+zinten+zresins,
 data=dat, family=poisson)
summary(fit0);vif(fit0)



library(lme4)
fit<-lmer(vcrimert~offset(log(expvcrime))+zabc+ziso+ne_bad+zinten+zresins+(1|FIPS), data=dat, family=poisson)

fit1<-lmer(vcrimert~offset(log(expvcrime))+zabc+ziso+zforn+ne_bad+zinten+zresins+(1|FIPS), data=dat, family=poisson)
summary(fit); summary(fit1)


fit2<-lmer(cbind(vcrimert, POPSIZE)~zabc+ziso+zdis+zforn+ne_bad+zinten+zresins+(1|FIPS), data=dat, family=binomial)
summary(fit2)

anova(fit, fit2)

fitsp<-lagsarlm(logvcrime~zabc+ziso+zdis+zforn+ne_bad+zinten+zresins, listw=salw, dat)
summary(fitsp)

N<-length(dat$vcrimert)

inits1<-list(alpha=1,
beta=c(0,0,0,0,0,0,0),
u=runif(N, -1,1), v=rep(0,N),
  stdu=1, stdv=1 )

inits2<-list(alpha=-1,
beta=c(1,1,1,1,1,1,1),
u=runif(N, -10,10), v=rep(0,N),
  stdu=5, stdv=5)



#propoerty
#d<-list(y=dat$pcrimert, n=dat$POPSIZE,E=dat$exppcrime,
d<-list(y=dat$vcrimert, E=dat$expvcrime,
N=N,abc=dat$zabc, 
iso=dat$ziso, dis=dat$zdis,
        forn=dat$zforn, resin=dat$zresins,
inten=dat$zinten, bad=dat$ne_bad,
adj=sawt$adj, weights=sawt$weights, num=sawt$num)


#d<-c(list(crimert=dat$crimert, expcrime=dat$expcrime,N=N, min=dat$z_min, unemp=dat$z_unemp))
#setwd("C:/workspace/crime/")


setwd("C:/WorkSpace/sacrimework/spatial/revis/vcrime")

model1<-"
model
{
for (i in 1:N)
{

#likelihood
y[i] ~ dpois(mu[i])

mu[i]<- theta[i]*E[i]
#theta[i]<-exp(alpha + beta[1]*abc[i]+beta[2]*iso[i]+beta[3]*dis[i]+beta[4]*forn[i]+
#beta[5]*resin[i]+beta[6]*inten[i]+beta[7]*bad[i]+
theta[i]<-exp(alpha + beta[1]*abc[i]+beta[2]*iso[i]+beta[3]*dis[i]+beta[4]*forn[i]+
beta[5]*resin[i]+beta[6]*inten[i]+beta[7]*bad[i]+
u[i] +
v[i])
trash1[i]<-iso[i]

u[i]~dnorm(0, precu)

}
v[1:N]~car.normal(adj[], weights[], num[], precv)

alpha~dflat()

precv<-pow(stdv, -2)
precu<-pow(stdu, -2)

stdv~dunif(0,100)
stdu~dunif(0,100)

for (j in 1:7){beta[j]~dnorm(0,.001)}
#for (j in 1:4){beta[j]~dnorm(0,.001)}

}
"

write(model1, "sppois.txt")
tdir<-getwd()
mfile<-paste(getwd(),"/sppois.txt",sep="", collapse="")
bugs.data(d, data.file="data.txt")
bugs.data(inits1, data.file="inits1.txt")
bugs.data(inits2, data.file="inits2.txt")

resprop<- bugs(data = "data.txt", inits = list(inits1, inits2),
 parameters.to.save = c( "alpha", "beta","theta","v", "u", "stdu", "stdv"),
 model.file = mfile, working.directory = tdir,
 n.chains = 2,  n.iter =100000, n.burnin = 50000, debug=F,
 bugs.directory="C:/WorkSpace/WinBUGS14/")


dat$vmean<-resviol$mean$v
dat$umean<-resviol$mean$u
dat$phat<-resviol$mean$p

spplot(dat, "vmean", pretty=T, at = c(-4,-2,-1,0,1,2,4), col.regions=brewer.pal(6, "RdBu"))

spplot(dat, "umean", pretty=T, at = c(-1,-.5,-.1,0,.1,.5,1), col.regions=brewer.pal(6, "RdBu"))

spplot(dat, "phat", pretty=T,  at=c(0,.01, .05,.1,.3,.5, .6),col.regions=brewer.pal(6, "RdBu"))

#alpha = 1 beta1 through 11 2:12

#apply(abs(resprop$sims.matrix[,2:12]), 2,function (x) ifelse(mean(x)<0, mean(x<0), mean(x>0))
resmc$summary[1:15,]

mean(resprop$sims.matrix[,2]>0) #abc
mean(resprop$sims.matrix[,3]>0) #iso
mean(resprop$sims.matrix[,4]>0) #dis
mean(resprop$sims.matrix[,5]<0) #inten
mean(resprop$sims.matrix[,6]<0) #male
mean(resprop$sims.matrix[,7]>0) #imm
mean(resprop$sims.matrix[,8]<0) #bad ne
mean(resprop$sims.matrix[,9]<0) #good ne
mean(resprop$sims.matrix[,10]>0) #hwy
mean(resprop$sims.matrix[,11]>0) #school
mean(resprop$sims.matrix[,12]>0) #nb assoc


#risk ratios > 1.25, 1.5, 2
dat$prtheta125<-apply(resprop$sims.matrix[,1738:2598],2,function(x) mean (x>=1.25))
dat$prtheta15<-apply(resprop$sims.matrix[,1738:2598],2,function(x) mean (x>=1.5))
dat$prtheta2<-apply(resprop$sims.matrix[,1738:2598],2,function(x) mean (x>=2))

#Figures showing risk ratio of varying levels
library(classInt)
tiff("T:/users/ozd504/Work/Papers_Manuscripts/SACrime/Fig1.tif", width=2400, height=1800, res=150)


par(mfrow=c(1,3))
brks.pr125<-classIntervals(dat$prtheta125, n=5,style="fixed", fixedBreaks = c(0,.25,.5,.75,.99,1))
cols<-brewer.pal(5, "Greys")
plot(bexol, col=grey(.1))
plot(dat, col=cols[findInterval(dat$prtheta125, brks.pr125$brks, all.inside=T)], border=0,add=T)
lines(saol, col=grey(.4))
legend("topright", legend=leglabs(round(brks.pr125$brks,3), under="<", over=">", between="-"), 
fill=cols, title= expression(paste("Pr ",theta, "> 1.25")), cex=1.25)

brks.pr15<-classIntervals(dat$prtheta15, n=5,style="fixed", fixedBreaks = c(0,.25,.5,.75,.99,1))
plot(bexol, col=grey(.1))
plot(dat, col=cols[findInterval(dat$prtheta15, brks.pr15$brks, all.inside=T)], border=0,add=T)
lines(saol, col=grey(.4))
legend("topright", legend=leglabs(round(brks.pr125$brks,3), under="<", over=">", between="-"), 
fill=cols, title= expression(paste("Pr ",theta, "> 1.5")), cex=1.25)
title(main="Posterior Probabilities for Property Crime Risk Ratios")

brks.pr2<-classIntervals(dat$prtheta2, n=5,style="fixed", fixedBreaks = c(0,.25,.5,.75,.99,1))
plot(bexol, col=grey(.1))
plot(dat, col=cols[findInterval(dat$prtheta2, brks.pr2$brks, all.inside=T)], border=0,add=T)
lines(saol, col=grey(.4))
legend("topright", legend=leglabs(round(brks.pr2$brks,3), under="<", over=">", between="-"), 
fill=cols, title= expression(paste("Pr ",theta, "> 2")), cex=1.25)
dev.off()


par(mfrow=c(1,2))
brks.iso<-classIntervals(dat$isol, n=5,style="quantile")
cols<-brewer.pal(5, "Greys")
plot(bexol, col=grey(.1))
plot(dat, col=cols[findInterval(dat$isol, brks.iso$brks, all.inside=T)], border=0,add=T)
lines(saol, col=grey(.4))
legend("topright", legend=leglabs(round(brks.iso$brks,3), under="<", over=">", between="-"), 
fill=cols, title= expression(paste("Pr ",theta, "> 1.25")), cex=1.25)

brks.pr125<-classIntervals(dat$prtheta125, n=5,style="fixed", fixedBreaks = c(0,.25,.5,.75,.99,1))
cols<-brewer.pal(5, "Greys")
plot(bexol, col=grey(.1))
plot(dat, col=cols[findInterval(dat$prtheta125, brks.pr125$brks, all.inside=T)], border=0,add=T)
lines(saol, col=grey(.4))
legend("topright", legend=leglabs(round(brks.pr125$brks,3), under="<", over=">", between="-"), 
fill=cols, title= expression(paste("Pr ",theta, "> 1.25")), cex=1.25)

brks.pr2<-classIntervals(dat$prtheta2, n=5,style="fixed", fixedBreaks = c(0,.25,.5,.75,.99,1))
plot(bexol, col=grey(.1))
plot(dat, col=cols[findInterval(dat$prtheta2, brks.pr2$brks, all.inside=T)], border=0,add=T)
lines(saol, col=grey(.4))
legend("topright", legend=leglabs(round(brks.pr2$brks,3), under="<", over=">", between="-"), 
fill=cols, title= expression(paste("Pr ",theta, "> 2")), cex=1.25)



jpeg("T:/users/ozd504/Work/Presentations/SDA2010/sacrime/Crimerates.jpg", width=600, height=600,quality=100)

