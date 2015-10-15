##############################################################
##                                                          ##
## 1. Prepare data for comparisons                          ##
## 2. Between-reader otolith comparisons (TAL v. DHO ages)  ##
## 3. Otolith-scales comparisons (only TAL ages)            ##
##                                                          ##
##############################################################

##############################################################
## 1. Prepare data for comparisons                          ##
##############################################################
# Get raw data and load packages
rm(list=ls()); cat("\014")
source("DataInit.R")
# Reduce data for otolith-otolith comparisons
kiyiOO <- kiyiAge[complete.cases(kiyiAge[,c("otoAge_TAL","otoAge_DHO")]),]
# Reduce data for otolith-scale comparisons
kiyiOS <- kiyiAge[complete.cases(kiyiAge[,c("scaleAge","otoAge_TAL")]),]



##############################################################
## 2. Between-reader otolith comparisons (TAL v. DHO ages)  ##
##############################################################
# Bias analysis
ab.tA2 <- ageBias(otoAge_TAL~otoAge_DHO,data=kiyiOO,ref.lab="Ager 1",nref.lab="Ager 2")
plot(ab.tA2,show.pts=TRUE)
summary(ab.tA2,what="bias")
summary(ab.tA2,what="EvansHoenig")
summary(ab.tA2,what="symmetry")
# Precision analysis
ap.B <- agePrecision(otoAge_TAL~otoAge_DHO,data=kiyiOO)
summary(ap.B,what="absolute difference")
# within one year.
sum(ap.B$absdiff[c("0","1")])/sum(ap.B$absdiff)*100
summary(ap.B,what="precision")

# Publication quality graphic
png("manuscript/Figs/FigureX_OtoOtoComp.PNG",width=4.5,height=4.5,units="in",pointsize=14,family="sans",res=600)
par(mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2,las=1)
plot(ab.tA2,show.n=TRUE,nYpos=0.025,cex.n=0.6,lwd.CI=2,col.CIsig="gray",yaxt="n",
     lwd.agree=1,xlim=c(3,20),ylim=c(-3.4,2),difference=TRUE,
     show.pts=TRUE,transparency=1/15,ylab="Second Reader",xlab="First Reader")
axis(2,seq(-3,2,1))
dev.off()



##############################################################
## 3. Otolith-scales comparisons (only TAL ages)            ##
##############################################################
ab.tA1 <- ageBias(scaleAge~otoAge_TAL,data=kiyiOS,
                  ref.lab="Otolith Age",nref.lab="Scale Age")
plot(ab.tA1,show.pts=TRUE)
summary(ab.tA1,what="bias")
summary(ab.tA1,what="table")
summary(ab.tA1,what="symmetry")

# Publication quality graphic
png("manuscript/Figs/FigureX_ScaleOtoComp.PNG",width=4.5,height=4.5,units="in",pointsize=14,family="sans",res=600)
par(mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2,las=1)
plot(ab.tA1,show.n=TRUE,nYpos=0.025,cex.n=0.6,lwd.CI=2,col.CIsig="gray",
     lwd.agree=1,xlim=c(3,16),ylim=c(-10.5,0),difference=TRUE,yaxt="n",
     show.pts=TRUE,transparency=1/5)
axis(2,seq(-10,0,1))
axis(1,seq(4,16,2))
dev.off()


