##############################################################
##                                                          ##
## 1. Load data                                             ##
## 2. Construct and apply age-length key                    ##
## 3. Summarize assigned ages                               ##
## 4. Age frequency overall                                 ##
## 5. Age frequency by region                               ##
##                                                          ##
##############################################################

##############################################################
## 1. Load data
##############################################################
# Get raw data and load packages
source("DataInit.R")


##############################################################
## 2. Construct and apply age-length key                    ##
##############################################################
# Set random seed ... all randomization same on each run
set.seed(34783478)
# Exclude fish <140 mm (only 4 fish)
tmp <- filter(kiyial,tl>=140)

## Make one ALK for all fish (see ALKComparison.R)
# Isolate aged fish
kiyial.aged <- tmp %>% filterD(!is.na(otoAge))
all(!is.na(kiyial.aged$otoAge))  # confirm TRUE
# make the key
alk.freq <- xtabs(~lcat10+otoAge,data=kiyial.aged)
alk <- prop.table(alk.freq,margin=1)
round(alk,3)

## Apply ALK to all unaged fish
# Isolate unaged fish
kiyial.unaged <- tmp %>% filterD(is.na(otoAge)) %>% as.data.frame()
all(is.na(kiyial.unaged$otoAge))  # confirm TRUE
# Apply the ALK
kiyial.unaged.mod <- alkIndivAge(alk,otoAge~tl,data=kiyial.unaged)
# Combine back with the otolith aged fish
kiyial.fnl <- rbind(kiyial.aged,kiyial.unaged.mod)
any(is.na(kiyial.fnl$otoAge))     # confirm FALSE





##############################################################
## 3. Summarize assigned ages                               ##
##############################################################
# Consensus ages histogram
hist(~OtoAge,data=kiyial.fnl,breaks=2:20,xlab="Age",col="gray50")
#Summary of Consensus Ages
(fnl.summary <- xtabs(~lcat10+otoAge,data=kiyial.fnl))

#-------------------------------------------------------------
# Age frequency histograms by regions
#-------------------------------------------------------------
brks <- seq(2,20,1)
hist(otoAge~Reg,data=kiyial.fnl,breaks=brks,xlab="Age",nrow=5,ncol=1)


#-------------------------------------------------------------
# Age frequency tables by region (sexes pooled)
#   with corresponding chi-square tests
#-------------------------------------------------------------
(kiyi.age1<-xtabs(~Reg+otoAge,data=kiyial.fnl))
(kiyi.age2<-cbind(rowSums(kiyi.age1[,1:2]),kiyi.age1[,3:8],rowSums(kiyi.age1[,9:14])))
#Made ages <6,6,7,8,9,10,11,>11 in groups
(OA.chi<-chisq.test(kiyi.age2))
round(prop.table(kiyi.age2,margin=1)*100,0)


#=============================================================
# PRESENTATION-QUALITY GRAPHICS
#=============================================================
#-------------------------------------------------------------
# Overall Age Frequency
#-------------------------------------------------------------
windows(7,6)
par(mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2,cex=1.75,lwd=2,las=1)
hist(~otoAge,data=kiyial.fnl,breaks=4:20,ylim=c(0,100),xlab="Consensus Age",col="gray50")
axis(2,lwd=3)
axis(1,4:20,lwd=3)
axis(1,seq(7,19,2))

#-------------------------------------------------------------
# Age Frequency by Region
#-------------------------------------------------------------
windows(3.5,2)
ylim <- c(0,20)
par(mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2,cex=1,lwd=2,las=1)
hist(~otoAge,data=filter(kiyial.fnl,Reg=="East"),breaks=4:20,ylim=ylim,xlab="Consensus Age",col="gray50",xaxt="n")
axis(2,lwd=3)
axis(1,seq(4,20,2),lwd=3)
legend("topright","East",bty="n",cex=1.25)

hist(~otoAge,data=filter(kiyial.fnl,Reg=="West"),breaks=4:20,ylim=ylim,xlab="Consensus Age",col="gray50",xaxt="n")
axis(2,lwd=3)
axis(1,seq(4,20,2),lwd=3)
legend("topright","West",bty="n",cex=1.25)

hist(~otoAge,data=filter(kiyial.fnl,Reg=="North"),breaks=4:20,ylim=ylim,xlab="Consensus Age",col="gray50",xaxt="n")
axis(2,lwd=3)
axis(1,seq(4,20,2),lwd=3)
legend("topright","North",bty="n",cex=1.25)

hist(~otoAge,data=filter(kiyial.fnl,Reg=="South"),breaks=4:20,ylim=ylim,xlab="Consensus Age",col="gray50",xaxt="n")
axis(2,lwd=3)
axis(1,seq(4,20,2),lwd=3)
legend("topright","South",bty="n",cex=1.25)

hist(~otoAge,data=filter(kiyial.fnl,Reg=="Isle"),breaks=4:20,ylim=ylim,xlab="Consensus Age",col="gray50",xaxt="n")
axis(2,lwd=3)
axis(1,seq(4,20,2),lwd=3)
legend("topright","Isle",bty="n",cex=1.25)
