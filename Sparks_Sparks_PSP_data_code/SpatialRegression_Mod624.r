library(maptools)
library(spdep) 
library(sp)
library(sf)

  us.clus<-st_read("~/Documents/GitHub/previous_research/Sparks_Sparks_PSP_data_code/PSP_Data_CL.shp")

  us.outline<-readShapeLines("/Volumes/demography/users/ozd504/Work/Papers_Manuscripts/AppliedStatPaper/USMortality/US_State_line.shp")


library(classInt)
library(RColorBrewer)
#Figure 1
quartz( type="tiff", file="/Volumes/demography/users/ozd504/Work/PSP_Paper/FIG1.tif", dpi=300, bg="white")
brks.mort<-classIntervals(us.clus$ARSMORT, n=5, style="fisher")
cols<-brewer.pal(6, "Greys")
#Plot mortality rates
brks.mort.leg<-leglabs(round(brks.mort$brks, 2))
layout(matrix(c(1,1,2,3),2,2,byrow=T),respect=T)
plot(us.clus, col=cols[findInterval(us.clus$ARSMORT, brks.mort$brks,all.inside=F)], border=0, lwd=.0005)
title(main=c("Figure 1.  Geographic Distribution and Comparison to Gaussian Distribution", "of US Age, Sex and Race Standardized Mortality Rate, 1998 - 2002"), cex=.7)
legend("bottomright", title="Mortality Rate", legend=brks.mort.leg, fill=brewer.pal(6,"Greys"),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.5)

plot(density(us.clus$ARSMORT), lwd=1, main=c("Distribution of Mortality Rates", "1998-2002"))
lines(density(rnorm(10000, mean=mean(us.clus$ARSMORT), sd=sd(us.clus$ARSMORT))), col=grey(.5), lwd=2, lty=2)
legend("topright", legend=c("Observed", "Gaussian"), col=c("black", grey(.5)), lwd=2, cex=.75, lty=c(1,2))

qqnorm(us.clus$ARSMORT, main=c("Normal Q-Q Plot for Standardized", "Mortality Rates"))
qqline(us.clus$ARSMORT)
dev.off()

#Figure 2a-2e
quartz( type="tiff", file="/Volumes/demography/users/ozd504/Work/PSP_Paper/FIG2A.tif", dpi=300, bg="white")
par(mfrow=c(2,1))
#Mortality
cols<-rep(1,length(us.clus$CL_ARSTOT00))
cols[us.clus$CL_ARSTOT00==0]<-1
cols[us.clus$CL_ARSTOT00==1]<-2
cols[us.clus$CL_ARSTOT00==2]<-3
cols[us.clus$CL_ARSTOT00==3]<-4
cols[us.clus$CL_ARSTOT00==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0 , bg=0, lwd=.0005)
title(main=c("Figure 2a. Local Autocorrelation Cluster Map for U.S. County","Mortality Rates and Rural Population"),sub=c("Local Autocorrelation Clusters Standardized Mortality Rate"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)

#Rural
cols<-rep(1,length(us.clus$CL_PRURAL2K))
cols[us.clus$CL_PRURAL2K==0]<-1
cols[us.clus$CL_PRURAL2K==1]<-2
cols[us.clus$CL_PRURAL2K==2]<-3
cols[us.clus$CL_PRURAL2K==3]<-4
cols[us.clus$CL_PRURAL2K==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, bg=0, lwd=.0005)
title(sub=c("Local Autocorrelation Clusters Percent Rural Population"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)
dev.off()



quartz( type="tiff", file="/Volumes/demography/users/ozd504/Work/PSP_Paper/FIG2B.tif", dpi=300, bg="white")
par(mfrow=c(2,1))
# Black
cols<-rep(1,length(us.clus$CL_PBLACK))
cols[us.clus$CL_PBLACK==0]<-1
cols[us.clus$CL_PBLACK==1]<-2
cols[us.clus$CL_PBLACK==2]<-3
cols[us.clus$CL_PBLACK==3]<-4
cols[us.clus$CL_PBLACK==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, lwd=.0005)
title(main=c("Figure 2b. Local Autocorrelation Cluster Map for Percentages", "of U.S. County Populations that are Black and Hispanic"),sub=("Local Autocorrelation Clusters Percent Black Population"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)

# Hispanic
cols<-rep(1,length(us.clus$CL_PHISP))
cols[us.clus$CL_PHISP==0]<-1
cols[us.clus$CL_PHISP==1]<-2
cols[us.clus$CL_PHISP==2]<-3
cols[us.clus$CL_PHISP==3]<-4
cols[us.clus$CL_PHISP==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, lwd=.0005)
title(sub=c("Local Autocorrelation Clusters Percent Hispanic Population"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)
dev.off()


      
quartz( type="tiff", file="/Volumes/demography/users/ozd504/Work/PSP_Paper/FIG2C.tif", dpi=300, bg="white")
par(mfrow=c(2,1))
                                        # FEMALE HH
cols<-rep(1,length(us.clus$CL_FEMHH))
cols[us.clus$CL_FEMHH==0]<-1
cols[us.clus$CL_FEMHH==1]<-2
cols[us.clus$CL_FEMHH==2]<-3
cols[us.clus$CL_FEMHH==3]<-4
cols[us.clus$CL_FEMHH==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, lwd=.0005)
title(main =c("Figure 2c. Local Autocorrelation Cluster Map", "for Percentages of U.S. County Households that are", "Female Headed and County Unemployment Rates"), sub=c("Local Autocorrelation Clusters Percent Female Headed Households"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)

# Unemployment
cols<-rep(1,length(us.clus$CL_UNEMP))
cols[us.clus$CL_UNEMP==0]<-1
cols[us.clus$CL_UNEMP==1]<-2
cols[us.clus$CL_UNEMP==2]<-3
cols[us.clus$CL_UNEMP==3]<-4
cols[us.clus$CL_UNEMP==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, lwd=.0005)
title(sub=c("Local Autocorrelation Clusters Unemployment Rate"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)
dev.off()


quartz( type="tiff", file="/Volumes/demography/users/ozd504/Work/PSP_Paper/FIG2D.tif", dpi=300, bg="white")
par(mfrow=c(2,1))
                                        # Housing value
cols<-rep(1,length(us.clus$CL_MEDHVAL))
cols[us.clus$CL_MEDHVAL==0]<-1
cols[us.clus$CL_MEDHVAL==1]<-2
cols[us.clus$CL_MEDHVAL==2]<-3
cols[us.clus$CL_MEDHVAL==3]<-4
cols[us.clus$CL_MEDHVAL==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, lwd=.0005)
title(main=c("Figure 2d. Local Autocorrelation Cluster Map for Median", "Housing Value and County Population Density"),sub=c("Local Autocorrelation Clusters Median Home Value"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c"white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)
      
# Population Density
cols<-rep(1,length(us.clus$CL_PDENSITY))
cols[us.clus$CL_PDENSITY==0]<-1
cols[us.clus$CL_PDENSITY==1]<-2
cols[us.clus$CL_PDENSITY==2]<-3
cols[us.clus$CL_PDENSITY==3]<-4
cols[us.clus$CL_PDENSITY==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, lwd=.0005)
title(sub=c("Local Autocorrelation Clusters Population Density"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)
dev.off()

      
quartz( type="tiff", file="/Volumes/demography/users/ozd504/Work/PSP_Paper/FIG2E.tif", dpi=300, bg="white")
#Gini
cols<-rep(1,length(us.clus$CL_GINI001))
cols[us.clus$CL_GINI001==0]<-1
cols[us.clus$CL_GINI001==1]<-2
cols[us.clus$CL_GINI001==2]<-3
cols[us.clus$CL_GINI001==3]<-4
cols[us.clus$CL_GINI001==4]<-5
plot(us.clus, col=c("white", grey(.1), grey(.35), grey(.75), grey(.9))[cols], border=0, lwd=.0005)
title(main=c("Figure 2e. Local Autocorrelation Cluster Map for", "U.S. Counties Gini Coefficient for Income"),sub=c("Local Autocorrelation Clusters Gini Coefficient (Income)"))
legend("bottomright", title="Cluster Type", legend=c("Unclustered", "HH", "LL","LH", "HL"), fill=c("white", grey(.1), grey(.35), grey(.75), grey(.9)),cex=.8, y.intersp=0.8) 
lines(us.outline, lwd=.2)
dev.off()


#writeSpatialShape(us.clus, "/Volumes/demography/users/ozd504/Work/PSP_Paper/PSP_Data.shp")

#us.clus<-readShapeSpatial("/Volumes/demography/users/ozd504/Work/PSP_Paper/PSP_Data_CL.shp")
us.clus$radrvar00.new<-ifelse(us.clus$RADRVAR00!=0, us.clus$RADRVAR00, .0001)
us.clus$wt.ninv<-1/((us.clus$F1198401 +us.clus$F0453000 +us.clus$F1198499)/3)

us.clus<-us.clus[us.clus$STATE==48,]
us.clus$struct<-1:dim(us.clus)[1]
us.nb<-poly2nb(us.clus, queen=T, row.names=us.clus$SP_ID)
us.nb<-make.sym.nb(us.nb)
us.wtw<-nb2listw(us.nb,style="W", zero.policy=T)
M<-nb2mat(us.nb, style = "B")

library(brms)

fit<-brm(ARSMORT ~ PRURAL+PBLACK+PHISP+FEMHH+car(M, type = "bym2"), data=us.clus, data2 = list(M=M), 
         cores = 2, chains = 2, iter = 10000, warmup = 3000, thin = 10)
summary(fit)

samps<-as.data.frame(fit$fit)

library(INLA)
colnames(M) <- rownames(M) 
M <- as.matrix(M[1:dim(M)[1], 1:dim(M)[1]])
nb2INLA("cl_graph",us.nb)
am_adj <-paste(getwd(),"/cl_graph",sep="")
H<-inla.read.graph(filename="cl_graph")

mod1<-inla(formula = ARSMORT ~ PRURAL+PBLACK+PHISP+FEMHH+f(struct, model="bym2",  graph = H),data = us.clus, #linear predictor - fixed effects
           family = "gaussian",  #marginal distribution for the outcome, expected count
           #control.compute = list(waic=T), # compute DIC or not?
          # control.predictor = list(link=1), #estimate predicted values & their marginals or not?
           num.threads = 3, 
           verbose = F)
summary(mod1)

fiti<-inla(ARSMORT ~ PRURAL+PBLACK+PHISP+FEMHH+f(SPID, model="besag"), data=us.clus)
#us.outline<-readShapeLines(fn="/Volumes/demography/users/ozd504/Work/AppliedStatPaper/USMortality/usline.shp")


#Global Moran statistics
mor.mort<-moran.test(us.clus$ARSMORT, us.wtw, zero.policy=T)
mor.rur<-moran.test(us.clus$PRURAL, us.wtw, zero.policy=T)
mor.bla<-moran.test(us.clus$PBLACK, us.wtw, zero.policy=T)
mor.his<-moran.test(us.clus$PHISP, us.wtw, zero.policy=T)
mor.fem<-moran.test(us.clus$FEMHH, us.wtw, zero.policy=T)
mor.unem<-moran.test(us.clus$UNEMP, us.wtw, zero.policy=T)
mor.pov<-moran.test(us.clus$PPERSONPO, us.wtw, zero.policy=T)
mor.dens<-moran.test(us.clus$PDENSITY, us.wtw, zero.policy=T)
mor.inc<-moran.test(us.clus$MEDHHINC, us.wtw, zero.policy=T)
mor.house<-moran.test(us.clus$MEDHVAL, us.wtw, zero.policy=T)
mor.gin<-moran.test(us.clus$GINI00.1, us.wtw, zero.policy=T)

print(list(mor.mort, mor.rur, mor.bla, mor.his, mor.fem, mor.unem, mor.pov, mor.dens, mor.inc, mor.house, mor.gin))

loc.mort<-localmoran(us.clus$ARSMORT, us.wtw, zero.policy=T)
loc.rur<-localmoran(us.clus$PRURAL, us.wtw, zero.policy=T)
loc.bla<-localmoran(us.clus$PBLACK, us.wtw, zero.policy=T)
loc.his<-localmoran(us.clus$PHISP, us.wtw, zero.policy=T)
loc.fem<-localmoran(us.clus$FEMHH, us.wtw, zero.policy=T)
loc.unem<-localmoran(us.clus$UNEMP, us.wtw, zero.policy=T)
loc.pov<-localmoran(us.clus$PPERSONPO, us.wtw, zero.policy=T)
loc.dens<-localmoran(us.clus$PDENSITY, us.wtw, zero.policy=T)
loc.inc<-localmoran(us.clus$MEDHHINC, us.wtw, zero.policy=T)
loc.house<-localmoran(us.clus$MEDHVAL, us.wtw, zero.policy=T)
loc.gini<-localmoran(us.clus$GINI001, us.wtw, zero.policy=T)

us.clus$mortz<-scale(us.clus$ARSMORT, center=T, scale=T)
us.clus$densz<-scale(us.clus$PDENSITY, center=T, scale=T)
us.clus$rurz<-scale(us.clus$PRURAL, center=T, scale=T)
us.clus$blacz<-scale(us.clus$PBLACK, center=T, scale=T)
us.clus$hisz<-scale(us.clus$PHISP, center=T, scale=T)
us.clus$femz<-scale(us.clus$FEMHH, center=T, scale=T)
us.clus$unemz<-scale(us.clus$UNEMP, center=T, scale=T)
us.clus$povz<-scale(us.clus$PPERSONPO, center=T, scale=T)
us.clus$incz<-scale(us.clus$MEDHHINC, center=T, scale=T)
us.clus$hvalz<-scale(us.clus$MEDHVAL, center=T, scale=T)
us.clus$giniz<-scale(us.clus$GINI001, center=T, scale=T)


 us.clus$expmort<-us.clus$popsize*sum(us.clus$F1194100)/sum(us.clus$popsize)
us.clus$SMR<-us.clus$F1194100/us.clus$expmort
#################################
#This portion of the program does the analysis presented
#in the results section
#################################

#Do the bootstrap of the Shapiro-Wilk test
results<-numeric(0)
for (i in 1:9999)
    {
	results[i]<-shapiro.test(sample(us.clus$ARSMORT, 1000))$statistic
	
}    
plot(density(results))
abline(v=shapiro.test(us.clus$ARSMORT)$statistic)
quantile(results)
sum(ifelse(results>=shapiro.test(us.clus$ARSMORT)$statistic,1,0))/10000

#Get the distribution of population sizes
us.clus$popsize<-(us.clus$F1198401+us.clus$F0453000+us.clus$F1198499)/3
summary(popsize)


#These are the regression models

fit.ols<-lm(mortz~rurz+blacz+hisz+femz+unemz+hvalz+densz+giniz,data=us.clus)
fit.ols.wt<-lm(mortz~rurz+blacz+hisz+femz+unemz+hvalz+densz+giniz,data=us.clus, weights=popsize)


fit.ols.res.mor<-lm.morantest(fit.ols,us.wtw,zero.policy=T)
fit.ols.res.mor
fit.ols.wt.res.mor<-lm.morantest(fit.ols.wt,us.wtw,zero.policy=T)
fit.ols.wt.res.mor

#Spatial lag and error models
fit.err<-errorsarlm(mortz~rurz+blacz+hisz+femz+unemz+hvalz+densz+giniz,data=us.clus, listw=us.wtw,zero.policy=T, tol.solve=1.0e-20,  method="Matrix")

fit.lag<-lagsarlm(SMR~rurz+blacz+hisz+femz+unemz+hvalz+densz+giniz,data=us.clus, listw=us.wtw,zero.policy=T, tol.solve=1.0e-20)

#this will calculate the smoothed mortality rate for each county
uswt.mat<-listw2mat(us.wtw)
smooth.mort<-uswt.mat%*%us.clus$ARSMORT
us.clus$sm.mort.z<-scale(smooth.mort, center=T, scale=T)


#Fitting the weighted SAR model
fit.spauto.w<-spautolm(mortz~rurz+blacz+hisz+femz+unemz+hvalz+densz+giniz,data=us.clus, listw=us.wtw,zero.policy=T, tol.solve=1.0e-20, family="SAR", method="Matrix", weights=1/us.clus$popsize)

summary(fit.ols)
summary(fit.ols.wt)
summary(fit.err)
summary(fit.lag)
summary(fit.spauto.w)

AIC(fit.ols)
AIC(fit.ols.wt)
AIC(fit.err)
AIC(fit.lag)
AIC(fit.spauto.w)

lm.LMtests(fit.ols, listw=us.wtw, zero.policy=T, "all")
lm.LMtests(fit.ols, listw=us.wtw, zero.policy=T, "LMlag")
lm.LMtests(fit.ols, listw=us.wtw, zero.policy=T, "RLMerr")
lm.LMtests(fit.ols, listw=us.wtw, zero.policy=T, "RLMlag")

bptest.sarlm(fit.err)
bptest.sarlm(fit.lag)

#Add the values of the local morans I to the data and run a re
us.clus$locrur<-loc.rur[,1]
us.clus$locmor<-loc.mort[,1]
us.clus$locbla<-loc.bla[,1]
us.clus$lochis<-loc.his[,1]
us.clus$locfem<-loc.fem[,1]
us.clus$locdens<-loc.dens[,1]
us.clus$lochouse<-loc.house[,1]
us.clus$locgini<-loc.gini[,1]

fit.ols.cl<-lm(mortz~rurz+blacz+hisz+femz+unemz+hvalz+densz+giniz+locrur+locbla+lochis+locfem+locdens+lochouse +locgini,data=us.clus)


#GEt sandwich estimates of model standard errors, corrected for heterosckedasticity
library(sandwich)
sqrt(diag(round(sandwich(fit.err$lm.target),5)))
sqrt(diag(round(sandwich(fit.lag$lm.target),5)))
sqrt(diag(round(sandwich((fit.ols.wt),5)))

     #This gets the heteroskedasticity corrected se's for the spautolm fit
us<-as.data.frame(us.clus)
     us$ones<-rep(1, 3071)
X<-as.matrix(us[,c("ones","rurz", "blacz","hisz", "femz", "unemz", "hvalz", "densz", "giniz")])
fit.res<-resid(fit.spauto.w)
fit.res<-as.matrix(fit.res)
fit.res.mat<-matrix(0, nrow=length(fit.res), ncol=length(fit.res))
spauto.fit.se<-diag(fit.res.mat)<-fit.res

test<-solve(t(X)%*%X)
test2<-t(X)%*%(fit.res.mat^2)%*%X
result<-test%*%test2%*%test
sqrt(diag(result))


     
us.filt<-SpatialFiltering(mortz~rurz+blacz+hisz+femz+unemz+hvalz+densz+giniz, data=us.clus, nb=us.nb, style="W", ExactEV=T, zero.policy=T)



#This made the original plots
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T),respect=T)
par(mfrow=c(3,1))
cols<-c("white", "black", grey(.5), "white", "white")
plot(usdat,border=0, col=cols[findInterval(us.clus$CL_MORT_RAT,vec=c(0,1,2,3,4))])
plot(us.outline, add=T)
legend(x=-140,y=39,c("High-High ", "Low-Low", "Unclustered"),fill=c("black", grey(.5),"white"),bty="n", cex=1)
title(main="Mortality Cluster Map")
plot(usdat,border=0, col=cols[findInterval(us.clus$CL_PRURAL,vec=c(0,1,2,3,4))])
plot(us.outline, add=T)
title( main="Rural Population Cluster Map")
plot(usdat,border=0, col=cols[findInterval(us.clus$CL_BLAC,vec=c(0,1,2,3,4))],forcefill=F)
plot(us.outline, add=T)
title( main="Black Population Cluster Map")
plot(usdat,border=0, col=cols[findInterval(us.clus$CL_HISP,vec=c(0,1,2,3,4))],forcefill=F)
plot(us.outline, add=T)
title(main="Hispanic Population Cluster Map")
plot(usdat,border=0, col=cols[findInterval(us.clus$CL_POV,vec=c(0,1,2,3,4))],forcefill=F)
plot(us.outline,add=T)
title(main="Poverty Cluster Map")
plot(usdat,border=0, col=cols[findInterval(us.clus$CL_DENS,vec=c(0,1,2,3,4))],forcefill=F)
plot(us.outline,add=T)
title(main="Population Density Cluster Map")
legend(x=-125,y=33,c("High-High ", "Low-Low", "Unclustered"),fill=c("black", grey(.5),"white"),bty="n")

#This made the original figures
par(mfrow=c(2,3))
hist(loc.mort[,5],breaks=seq(0,1,.1),ylim=c(0,2500),xlab="P > z",ylab="Number of Counties",main="Mortality Rate z" )
hist(loc.rur[,5],breaks=seq(0,1,.1),ylim=c(0,2500),xlab="P > z",ylab="Number of Counties", main="% Rural z")
hist(loc.bla[,5],breaks=seq(0,1,.1),ylim=c(0,2500),xlab="P > z",ylab="Number of Counties",main="%Black z")
hist(loc.his[,5],breaks=seq(0,1,.1),ylim=c(0,2500),xlab="P > z",ylab="Number of Counties",main="% Hispanic z")
hist(loc.pov[,5],breaks=seq(0,1,.1),ylim=c(0,2500),xlab="P > z",ylab="Number of Counties", main="%Poverty z")
hist(loc.hosp[,5],breaks=seq(0,1,.1),ylim=c(0,2500),xlab="P > z",ylab="Number of Counties",main="Hospital z")
