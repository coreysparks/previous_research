setwd("~/Dropbox/Sparks Work/infantmortality/")
library(maptools)
library(spdep)
library(classInt)
library(RColorBrewer)
library(R2WinBUGS)

us.state.outline<-readShapeLines("T:/users/ozd504/Work/Papers_Manuscripts/InfantMortManuscript/USStateOutline.shp",  proj4string=CRS("+proj=lcc"))
#this is original data, all counties
#usdat<-readShapePoly("/Volumes/demography/users/ozd504/Work/Papers_Manuscripts/InfantMortManuscript/datafile530final.shp",  proj4string=CRS("+proj=lcc"))


#us.nb<-poly2nb(usdat, queen=T)
#usdat<-usdat[card(us.nb)!=0,] #get rid of zero connectivity counties
#us.nb<-poly2nb(usdat, queen=T)

#writePolyShape(usdat, "/Volumes/demography/users/ozd504/Work/Papers_Manuscripts/InfantMortManuscript/bayesuscounty.shp")
usdat<-readShapePoly("bayesuscounty.shp",  proj4string=CRS("+proj=lcc"))
us.nb<-poly2nb(usdat, queen=T)
names(usdat)
us.wtw<-nb2listw(us.nb, zero.policy=T)
us.wb<-nb2WB(us.nb)

names(usdat)

#make variables
usdat$infd03<-usdat$f1194803
usdat$infd98<-usdat$f1194898

usdat$births03<-usdat$f1254603
usdat$births98<-usdat$f1254698

usdat$unempz<-as.numeric(scale(usdat$UNEMP, center=T, scale=T))
usdat$zblack<-as.numeric(scale(usdat$PBLACK, center=T, scale=T))
usdat$z_femkid<-as.numeric(scale(usdat$pfemhhwkid, center=T, scale=T))
usdat$z_pubass<-as.numeric(scale(usdat$phhpubas_1, center=T, scale=T))
usdat$ne_bad<-usdat$povz+usdat$unempz+usdat$z_femkid+usdat$z_pubass

#NE_good
usdat$z_hiwork<-as.numeric(scale(usdat$pmgmtprofo, center=T, scale=T))
usdat$z_hiedu<-as.numeric(scale(usdat$p25pluspro, center=T, scale=T))
usdat$z_inc<-as.numeric(scale(usdat$MEDHHINC, center=T, scale=T))


usdat$ne_good<-usdat$z_hiedu+usdat$z_inc+usdat$z_hiwork

usdat$ne_bad<-(usdat$povz+usdat$unempz+usdat$z_femkid+usdat$z_pubass+usdat$z_inc+usdat$z_hiedu+usdat$z_hiwork)/7


#Form poisson expected
usdat$E<-usdat$births03*(sum(usdat$infd03)/sum(usdat$births03))
usdat$smr03<-usdat$infd03/usdat$E


#descriptive spatial
tid<-moran.mc(usdat$infd03, listw=us.wtw, zero.policy=T,  nsim=999 )
tsmr<-moran.mc(usdat$smr, listw=us.wtw, zero.policy=T,  nsim=999 )

t1<-moran.mc(usdat$PRURAL2K, listw=us.wtw, zero.policy=T,  nsim=999 )
t2<-moran.mc(usdat$giniz, listw=us.wtw, zero.policy=T,  nsim=999 )
t3<-moran.mc(usdat$ne_bad, listw=us.wtw, zero.policy=T,  nsim=999 )
t4<-moran.mc(usdat$DISSWB, listw=us.wtw, zero.policy=T, nsim=999 )
t5<-moran.mc(usdat$DISSPR, listw=us.wtw, zero.policy=T, nsim=999 )
t6<-moran.mc(usdat$ISOBW, listw=us.wtw, zero.policy=T, nsim=999 )
t7<-moran.mc(usdat$ISOPR, listw=us.wtw, zero.policy=T, nsim=999 )
t8<-moran.mc(usdat$SPSEGR, listw=us.wtw, zero.policy=T, nsim=999 )
t9<-moran.mc(usdat$SPSEGP, listw=us.wtw, zero.policy=T, nsim=999 )


mean(usdat@data[,c("infd03", "E", "smr03","GINI00", "PRURAL2K", "ne_bad")], na.rm=T);sd(usdat@data[,c("infd03", "E","smr03", "GINI00", "PRURAL2K", "ne_bad")], na.rm=T)

mean(usdat@data[,c("DISSWB", "DISSPR", "ISOBW", "ISOPR", "SPSEGR", "SPSEGP")]);sd(usdat@data[,c("DISSWB", "DISSPR", "ISOBW", "ISOPR", "SPSEGR", "SPSEGP")])

desdatdf<-list(controls=data.frame(meanscontrols=mean(usdat@data[,c("infd03", "E", "smr03","GINI00", "PRURAL2K", "ne_bad")], na.rm=T),
sdcontrols=sd(usdat@data[,c("infd03", "E","smr03", "GINI00", "PRURAL2K", "ne_bad")], na.rm=T)),
seg=data.frame(meanseg=mean(usdat@data[,c("DISSWB", "DISSPR", "ISOBW", "ISOPR", "SPSEGR", "SPSEGP")]),
sdseg=sd(usdat@data[,c("DISSWB", "DISSPR", "ISOBW", "ISOPR", "SPSEGR", "SPSEGP")])),
moran.stats<-data.frame(infd=tid$statistic, smr=tsmr$statistic,rural=t1$statistic,gini=t2$statistic,depr=t3$statistic,
disbw=t4$statistic,dispr=t5$statistic,isobw=t6$statistic,isopr=t7$statistic,
spbw=t8$statistic,sppr=t9$statistic))

desdatdf

usdat$gt1d<-ifelse(usdat$infd03>=1,1,0)
usdat$gt200bl<-ifelse((usdat$PBLACK*usdat$f0453000)>=200,1,0 )
usdat$bgt10<-ifelse(usdat$births03>=10,1,0)

usdat$lcismr<-(qchisq(p=.95,df=2*usdat$infd03,log.p=F, lower.tail=F)*.5 )/usdat$E
usdat$ucismr<-(qchisq(p=.05,df=2*usdat$infd03+1,log.p=F, lower.tail=F)*.5 )/usdat$E
usdat$smrgt1<-ifelse(usdat$ucismr>1&usdat$lcismr>1,1,0)
spplot(usdat, "smrgt1")

usdat$disswbz<-as.numeric(scale(usdat$DISSWB, center=T, scale=T))
usdat$disspz<-as.numeric(scale(usdat$DISSPR, center=T, scale=T))
usdat$isowbz<-as.numeric(scale(usdat$ISOBW, center=T, scale=T))
usdat$isoprz<-as.numeric(scale(usdat$ISOPR, center=T, scale=T))
usdat$spbwz<-as.numeric(scale(usdat$SPSEGR, center=T, scale=T))
usdat$spprz<-as.numeric(scale(usdat$SPSEGP, center=T, scale=T))

#poisson regressions
fit.0<-glm(infd03~offset(log(E+.000001))+giniz+PRURAL2K+DISSWB+ne_bad, family=quasipoisson, data=usdat)
fit.1<-glm(infd03~offset(log(E+.000001))+giniz+PRURAL2K+DISSPR+ne_bad, family=quasipoisson, data=usdat)
fit.2<-glm(infd03~offset(log(E+.000001))+giniz+PRURAL2K+ISOBW+ne_bad, family=quasipoisson, data=usdat)
fit.3<-glm(infd03~offset(log(E+.000001))+giniz+PRURAL2K+ISOPR+ne_bad, family=quasipoisson, data=usdat)
fit.4<-glm(infd03~offset(log(E+.000001))+giniz+PRURAL2K+SPSEGR+ne_bad, family=quasipoisson, data=usdat, subset=SPSEGR>0)
fit.5<-glm(infd03~offset(log(E+.000001))+giniz+PRURAL2K+SPSEGP+ne_bad, family=quasipoisson, data=usdat)


fit0<-glm(infd03~offset(log(E+.000001))+DISSWB, family=poisson, data=usdat)
fit.0<-glm(infd03~offset(log(E+.000001))+DISSPR, family=poisson, data=usdat)
fit.1<-glm(infd03~offset(log(E+.000001))+ISOBW, family=poisson, data=usdat)
fit.2<-glm(infd03~offset(log(E+.000001))+ISOPR, family=poisson, data=usdat)
fit.3<-glm(infd03~offset(log(E+.000001))+SPSEGR, family=poisson, data=usdat)
fit.4<-glm(infd03~offset(log(E+.000001))+SPSEGP, family=poisson, data=usdat)

AIC(fit.0);AIC(fit.1); AIC(fit.2); AIC(fit.3); AIC(fit.4); AIC (fit.5)

library(MCMCglmm)
prior1<-list(G=list(G1=list(V=diag(9), nu=9), R=list(V=diag(9), nu=9)))
prior.flatvar<-list(R=list(V=1e-16, nu=-2),G=list(G1=list(V=1e-16,
nu=-2)))
prior2 = list(B= list (mu = matrix(c(0,1,0,0,0,0),6),V = diag(6)*1e+6))
diag(prior2$B$V)[2]<-(1e-6)

dat<-usdat@data

fit.b1<-MCMCglmm(infd03~log(E+.000001)+ giniz+PRURAL2K+DISSWB+ne_bad,data=dat, prior=prior2,family="poisson", verbose=F, burnin=50000, nitt=100000, thin=50)
fit.b2<-MCMCglmm(infd03~log(E+.000001)+ giniz+PRURAL2K+DISSPR+ne_bad, data=dat, prior=prior2,family="poisson", verbose=F, burnin=50000, nitt=100000, thin=50)

fit.b3<-MCMCglmm(infd03~log(E+.000001)+ giniz+PRURAL2K+ISOBW+ne_bad, data=dat,prior=prior2,family="poisson", verbose=F, burnin=50000, nitt=100000, thin=50)
fit.b4<-MCMCglmm(infd03~log(E+.000001)+ giniz+PRURAL2K+ISOPR+ne_bad, data=dat, prior=prior2,family="poisson", verbose=F, burnin=50000, nitt=100000, thin=50)
fit.b5<-MCMCglmm(infd03~log(E+.000001)+ giniz+PRURAL2K+SPSEGR+ne_bad, data=dat, prior=prior2,family="poisson", verbose=F, burnin=50000, nitt=100000, thin=50)
fit.b6<-MCMCglmm(infd03~log(E+.000001)+ giniz+PRURAL2K+SPSEGP+ne_bad, data=dat, prior=prior2,family="poisson", verbose=F, burnin=50000, nitt=100000, thin=50)
summary(fit.b1);summary(fit.b2);summary(fit.b3);summary(fit.b4);summary(fit.b5);summary(fit.b6)

library(spdep)
fit.sp1<-errorsarlm(smr03~giniz+PRURAL2K+DISSWB+ne_bad, data=dat,listw=us.wtw,method="Matrix", na.action="na.omit")
fit.sp2<-errorsarlm(smr03~giniz+PRURAL2K+DISSPR+ne_bad, data=dat,listw=us.wtw,method="Matrix", na.action="na.omit")
fit.sp3<-errorsarlm(smr03~giniz+PRURAL2K+ISOBW+ne_bad, data=dat,listw=us.wtw,method="Matrix", na.action="na.omit")
fit.sp4<-errorsarlm(smr03~giniz+PRURAL2K+ISOPR+ne_bad, data=dat,listw=us.wtw,method="Matrix", na.action="na.omit")
fit.sp5<-errorsarlm(smr03~giniz+PRURAL2K+SPSEGR+ne_bad, data=dat,listw=us.wtw,method="Matrix", na.action="na.omit")
fit.sp6<-errorsarlm(smr03~giniz+PRURAL2K+SPSEGP+ne_bad, data=dat,listw=us.wtw,method="Matrix", na.action="na.omit")

summary(fit.sp1);summary(fit.sp2);summary(fit.sp3);summary(fit.sp4);summary(fit.sp5);summary(fit.sp6)


cols<-brewer.pal(n=5,"Blues")
brks.smr<-classIntervals(usdat$smr, n=4, style="quantile")
postscript(file="/Volumes/Macintosh HD/Users/utsamacuser/Figure1.eps",width=1000, height=800)
#layout(mat=matrix(c(1,2,2,2), nrow=1,ncol=4),respect=F)
#hist(usdat$smr, xlab="SMR",ylim=c(0,2000), ylab="Frequency", main="Distribution of SMR")
plot(usdat,  col=cols[findInterval(usdat$smr, brks.smr$brks, all.inside=T )], border=0)
title(main=c("Spatial Distribution of the Standardized Mortality Ratio"), cex.main=1)
labs<-leglabs(round(brks.smr$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=1.2,title="SMR", fill=brewer.pal(5,"Blues"))
lines(us.state.outline, lwd=.5)
dev.off()

cols<-brewer.pal(n=5,"RdBu")
brks.bad<-classIntervals(usdat$ne_bad, n=5, style="quantile")
postscript(file="/Volumes/Macintosh HD/Users/utsamacuser/Figure2.eps",width=1000, height=800)
#layout(mat=matrix(c(1,2,2,2), nrow=1,ncol=4),respect=F)
#hist(usdat$smr, xlab="SMR",ylim=c(0,2000), ylab="Frequency", main="Distribution of SMR")
plot(usdat,  col=rev(cols[findInterval(usdat$ne_bad, brks.bad$brks, all.inside=T )]), border=0)
title(main=c("Spatial Distribution of the Deprivation Index"), cex.main=1)
labs<-leglabs(round(brks.bad$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=1.2,title="Index", fill=cols)
lines(us.state.outline, lwd=.5)
dev.off()



#prep for Winbugs
N<-length(usdat$infd03)
dat<-usdat@data

#attach(dat)
d<-list(y=dat$infd03,n=dat$births03, E=dat$E, N= N, gini=dat$giniz, rural=dat$PRURAL2K, bad=as.numeric(dat$ne_bad), 
        seg1=dat$DISSWB, seg2=dat$DISSPR,seg3=dat$ISOBW, seg4=dat$ISOPR, seg5=dat$SPSEGR, seg6=dat$SPSEGP,        
adj=us.wb$adj, num=us.wb$num, sumNumNeigh=sum(us.wb$num))
#these inits are only for seg only models
in1<-list(alpha=1, beta=0, u=rep(0,N),
 v = rep(0, N),
stdu = 1,
stdv = 1)

in2<-list(alpha=0, beta=1, u=rep(0,N),
 v = rep(0, N),
stdu = 5,
stdv = 5)

#these inits are for all beta models
inits1<-list(alpha = 1,
 beta = c(fit.3$coef[2], fit.3$coef[3],fit.3$coef[4],fit.3$coef[5]),
 u = rep(0,N),
 v = rep(0, N),
stdu = 1,
stdv = 1)

inits2 <- list(alpha = -1,
 beta = c(1,-1,1,-1),
 u = rep(0, N),
 v = rep(0,N),
 stdu = 3,
stdv = 3)



setwd("C:/Workspace/infmort")


modelstring3<-"
model
{
for (i in 1:N)
{
#likelihood
y[i]~dpois(mu1[i])
mu1[i]<-theta1[i]*E[i]
#mu2[i]<-theta2[i]*E[i]
#mu3[i]<-theta3[i]*E[i]
#mu4[i]<-theta4[i]*E[i]
#mu5[i]<-theta5[i]*E[i]
#mu6[i]<-theta6[i]*E[i]

theta1[i]<-exp(alpha+beta*seg1[i]+u[i]+v[i])
#theta2[i]<-exp(alpha+beta*seg2[i]+u[i]+v[i])
#theta3[i]<-exp(alpha+beta*seg3[i]+u[i]+v[i])
#theta4[i]<-exp(alpha+beta*seg4[i]+u[i]+v[i])
#theta5[i]<-exp(alpha+beta*seg5[i]+u[i]+v[i])
#theta6[i]<-exp(alpha+beta*seg6[i]+u[i]+v[i])

#theta2[i]<-exp(alpha+beta[1]*gini[i]+beta[2]*rural[i]+beta[3]*seg2[i]+beta[4]*bad[i]+u[i]+v[i])
#theta3[i]<-exp(alpha+beta[1]*gini[i]+beta[2]*rural[i]+beta[3]*seg3[i]+beta[4]*bad[i]+u[i]+v[i])
#theta4[i]<-exp(alpha+beta[1]*gini[i]+beta[2]*rural[i]+beta[3]*seg4[i]+beta[4]*bad[i]+u[i]+v[i])
#theta5[i]<-exp(alpha+beta[1]*gini[i]+beta[2]*rural[i]+beta[3]*seg5[i]+beta[4]*bad[i]+u[i]+v[i])
#theta6[i]<-exp(alpha+beta[1]*gini[i]+beta[2]*rural[i]+beta[3]*seg6[i]+beta[4]*bad[i]+u[i]+v[i])

trash1[i]<-seg2[i]
trash2[i]<-seg3[i]
trash3[i]<-seg4[i]
trash4[i]<-seg5[i]
trash5[i]<-seg6[i]
trash6[i]<-gini[i]; trash7[i]<-rural[i];trash8[i]<-bad[i]

u[i]~dnorm(0, precu)
trash[i]<-n[i]  
}
v[1:N]~car.normal(adj[], weights[], num[], precv)
alpha~dflat()

for (j in 1:sumNumNeigh){weights[j]<-1}

precv<-pow(stdv, -2)
precu<-pow(stdu, -2)
stdv~dunif(0,100)
stdu~dunif(0,100)
#for (j in 1:4){beta[j]~dnorm(0,.001)}
beta~dnorm(0,.0001)
}
"


write(modelstring3, "spois.txt")



#mfile <- paste(getwd(), "/spbinom.txt", sep = "", collapse = "")
#tdir <- paste(getwd(), "/spbinom", sep = "", collapse = "")

mfile <- paste(getwd(), "/spois.txt", sep = "", collapse = "")
tdir <- paste(getwd(), "/spois", sep = "", collapse = "")

#dir.create(tdir)
library(R2WinBUGS)

bugs.data(d, data.file="data.txt")
#bugs.data(inits1, data.file="inits1.txt")
#bugs.data(inits2, data.file="inits2.txt")

bugs.data(in1, data.file="in1.txt")
bugs.data(in2, data.file="in2.txt")

res <- bugs(data = d, inits = list(in1,in2), 
parameters.to.save = c(  "beta","stdv" , "stdu"),
 model.file = mfile, working.directory = tdir,save.history=F,
 n.chains = 2,  n.iter=60000,n.burnin=30000,n.sims=1000,debug=F,
           bugs.directory="C:/WorkSpace/WinBUGS14/")

# res<-read.bugs(codafiles=c( "coda1.txt", "coda2.txt"))
setwd("~/Dropbox/Sparks Work/infantmortality/")
res<-read.coda.interactive()

sums<-summary(res)
#dat<-usdat[card(us.nb)!=0,]

usdat$theta4<-as.numeric(sums$statistics[,1])
writePolyShape(usdat, "output12711.shp")

apply(usdat@data, 2, class)
nams<-data.frame(dimnames(sums$statistics)[1])
#change letter for variable, t=theta, v=v, m=mu
index<-which(substr(nams$c..alpha....beta.1.....beta.2.....beta.3.....beta.4.....theta5.1....,1,1)=="t")

us$prtheta1<-apply(as.matrix(res[,6:3069]), 2,function(x) mean(x>1))
spplot(us, "prtheta1")


mean(res[,2]>0) #seg
mean(res[,3]>0) #black
mean(res[,4]>0) #hisp
mean(res[,5]<0) #rural
mean(res[,6]<0) #metro



usdat2<-
usdat2$muv<-test$median$v
usdat$muu<-test$median$u
usdat$mutheta<-test$mean$theta



#########
#plots
##########

brks.rur.d <-classIntervals(usdat$PRURAL, n=5, style="quantile")
brks.his.d <-classIntervals(usdat$PHISP, n=5, style="quantile")
brks.bla.d <-classIntervals(usdat$PBLACK, n=5, style="quantile")
brks.dens.d <-classIntervals(usdat$PDENSITY, n=5, style="quantile")
brks.pov.d <-classIntervals(usdat$PPERSONPO, n=5, style="quantile")
brks.poviso.d<-classIntervals(usdat$ISOBW,n=5, style="quantile")
brks.poviso.i<-classIntervals(usdat$ISOPR,n=5, style="quantile")
brks.spsegr<-classIntervals(usdat$SPSEGR,n=5, style="quantile")
brks.spsegp<-classIntervals(usdat$SPSEGP,n=5, style="quantile")

cols<-brewer.pal(5, "RdBu")
 
postscript("T:/projects/Mexico_Spatial/GWRManuscript/Fig1.eps",width=8000, height=8000)
par(mfrow=c(2,2))
 cols<-brewer.pal(5,"Reds")


plot(usdat,  col=cols[findInterval(usdat$PRURAL, brks.rur.d$brks, all.inside=T )], border=0)
title(main=c("Spatial Distribution of the Rural Population %"), cex.main=1)
labs<-leglabs(round(brks.rur.d$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="% Rural", fill=brewer.pal(5,"Reds"))
lines(us.state.outline, lwd=.5)

plot(usdat,  col=cols[findInterval(usdat$PHISP, brks.his.d$brks, all.inside=T )], border=0)
title(main=c("Spatial Distribution of the Hispanic Population % "), cex.main=1)
labs<-leglabs(round(brks.his.d$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="% Hispanic", fill=brewer.pal(5,"Reds"))
lines(us.state.outline, lwd=.5)

plot(usdat,  col=cols[findInterval(usdat$PBLACK, brks.bla.d$brks, all.inside=T )], border=0)
title(main=c("Spatial Distribution of the Black Population % "), cex.main=1)
labs<-leglabs(round(brks.bla.d$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="% Black", fill=brewer.pal(5,"Reds"))
lines(us.state.outline, lwd=.5)

 dev.off()


brks.theta <-classIntervals(usdat$mutheta, n=5, style="quantile")
cols<-rev(brewer.pal(5,"RdBu"))


plot(usdat,  col=cols[findInterval(usdat$mutheta, brks.theta$brks, all.inside=T )], border=0)
title(main=c("Spatial Distribution of the Bayesian SMR"), cex.main=1)
labs<-leglabs(round(brks.theta$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="theta", fill=rev(brewer.pal(5,"RdBu")))
lines(us.state.outline, lwd=.5)

plot(usdat,  col=cols[findInterval(usdat$smr, brks.theta$brks, all.inside=T )], border=0)
title(main=c("Spatial Distribution of the SMR"), cex.main=1)
labs<-leglabs(round(brks.theta$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="theta", fill=rev(brewer.pal(5,"RdBu")))
lines(us.state.outline, lwd=.5)

usdat$muv<-test$statistics[6149:9212,1]
brks.v <-classIntervals(usdat$muv, n=5, style="quantile")
cols<-brewer.pal(5,"RdBu")
plot(usdat,  col=cols[findInterval(usdat$muv, brks.v$brks, all.inside=T )], border=0)

title(main=c("Spatial Distribution of the V"), cex.main=1)
labs<-leglabs(round(brks.v$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="v", fill=brewer.pal(5,"RdBu"))
lines(us.state.outline, lwd=.5)


brks.theta <-classIntervals(usdat$mutheta, n=5, style="quantile")
cols<-brewer.pal(5,"RdBu")
plot(usdat,  col=cols[findInterval(usdat$mutheta, brks.theta$brks, all.inside=T )], border=0)

plot(usdat,  col=cols[findInterval(usdat$smr, brks.theta$brks, all.inside=T )], border=0)
title(main=c("Spatial Distribution of the Bayesian SMR"), cex.main=1)
labs<-leglabs(round(brks.theta$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="theta", fill=brewer.pal(5,"RdBu"))
lines(us.state.outline, lwd=.5)

usdat$muv<-test$statistics[6149:9212,1]
brks.v <-classIntervals(usdat$muv, n=5, style="quantile")
cols<-brewer.pal(5,"RdBu")
plot(usdat,  col=cols[findInterval(usdat$muv, brks.v$brks, all.inside=T )], border=0)

title(main=c("Spatial Distribution of the V"), cex.main=1)
labs<-leglabs(round(brks.v$brks,3), under="Under", over="Over", between="to")
legend("bottomleft",legend=labs, cex=.8,title="v", fill=brewer.pal(5,"RdBu"))
lines(us.state.outline, lwd=.5)









