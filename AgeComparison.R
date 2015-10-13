##############################################################
##                                                          ##
## 1. Prepare data for comparisons                          ##
## 2. Otolith-scales comparisons (only TAL ages)            ##
## 3. Between-reader otolith comparisons (TAL v. DHO ages)  ##
##                                                          ##
##############################################################

##############################################################
## 1. Prepare data for comparisons                          ##
##############################################################
# Get raw data and load packages
source("DataInit.R")
# Reduce data for otolith-scale comparisons
kiyiOS <- kiyial[complete.cases(kiyial[,c("scaleAge","otoAge_TAL")]),]
# Reduce data for otolith-otolith comparisons
kiyiOO <- kiyial[complete.cases(kiyial[,c("otoAge_TAL","otoAge_DHO")]),]



##############################################################
## 2. Otolith-scales comparisons (only TAL ages)            ##
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
plot(ab.tA1,show.n=FALSE,lwd.CI=1,lwd.agree=1,xlim=c(3,16),ylim=c(3,16),
     show.pts=TRUE,transparency=1/5)
axis(2,seq(4,16,2))
axis(1,seq(4,16,2))
dev.off()


##############################################################
## 3. Between-reader otolith comparisons (TAL v. DHO ages)  ##
##############################################################
# Bias analysis
ab.tA2 <- ageBias(otoAge_TAL~otoAge_DHO,data=kiyiOO,ref.lab="Ager 1",nref.lab="Ager 2")
plot(ab.tA2,show.pts=TRUE)
summary(ab.tA2,what="bias")
summary(ab.tA2,what="table")
summary(ab.tA2,what="symmetry")
# Precision analysis
ap.B <- agePrecision(otoAge_TAL~otoAge_DHO,data=kiyiOO)
summary(ap.B,what="absolute difference")
summary(ap.B,what="precision")

# Publication quality graphic
png("manuscript/Figs/FigureX_OtoOtoComp.PNG",width=4.5,height=4.5,units="in",pointsize=14,family="sans",res=600)
par(mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2,las=1)
plot(ab.tA2,show.n=FALSE,lwd.CI=2,lwd.agree=2,xlim=c(3,16),ylim=c(3,16),
     show.pts=TRUE,transparency=1/15)
axis(1,seq(4,16,2))
dev.off()
