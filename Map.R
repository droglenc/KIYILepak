## Source the initialization script
source("aaaInit.R")

## Get Some Other Required packages
library(GISTools)
library(mapproj)
library(rgdal)

## Function to convert coords from USGS to true lat/long digits
cnvrt <- function(x) floor(x/100) + (x/100-floor(x/100))*100/60

## Get station locations, convert to true lat/long digits,
##   make longs negative, setup regions, remove location==2
sta <- read_excel("data/KIYILepak_2014.xlsx",sheet="Stations") %>%
  setNames(tolower(names(.))) %>%
  mutate(latitude=cnvrt(latitude),
         longitude=-1*cnvrt(longitude),
         region=factor(mapvalues(region,regSold,regS),levels=regS),
         regionL=factor(mapvalues(region,regS,regL),levels=regL)) %>%
  filterD(location!=2)
xtabs(~region,data=sta)


## Get map layer and futz with some projections
gl <- readOGR("./data","glgis_gl_shore_noaa_70k")
xlm <- -c(92.2,84.35); ylm <- c(46.1,49.5)
xd <- diff(xlm); yd <- diff(ylm)
ydc <- yd * 1/cos((mean(ylm) * pi)/180)
x_y <- xd/ydc
wid <- 6; ( hig <- wid/x_y )

## Make the map
png("manuscript/Figs/Figure1_SamplesMap.png",width=wid,height=hig,
    units="in",pointsize=14,family="sans",res=600)
par(xaxs="i", yaxs="i", mar=c(2,3,0.5,0.5),mgp=c(2,0.05,0),las=1,tcl=-0.2)
# Lake Superior
plot(gl,xlim=xlm,ylim=ylm,axes=TRUE,lwd=0.6,las=1,yaxt="n",xaxt="n")
degAxis(2,46:49,cex.axis=0.6,hadj=1.2)
degAxis(1,seq(-92,-84,1),cex.axis=0.6,padj=-0.3)
# Put on station points
points(sta$longitude,sta$latitude,cex=0.6,pch=pts[sta$region])
#with(sta,text(longitude,latitude,location,cex=0.4))
# Add scale, north arrow, and legend
maps::map.scale(-92.1,46.33,ratio=FALSE,relwidth=0.2,cex=0.5,pos=3,offset=0.1)
north.arrow(-84.65,49.2,len=0.05,lab="N")
legend("topleft",regL,pch=pts,bty="n",cex=0.7,inset=0.01)
dev.off()


## Setup Regions
# Get region boundary locations
# reg <- read_excel("data/KIYILepak_2014.xlsx",sheet="Regions")
# Put on region boundaries
#tmp <- c("Nipigon","Sault")
#with(reg,lines(longitude[shortloc %in% tmp],latitude[shortloc %in% tmp],col="gray50"))
#tmp <- c("Copper Harbor","Pukaskwa")
#with(reg,lines(longitude[shortloc %in% tmp],latitude[shortloc %in% tmp],col="gray50"))
#tmp <- c("Grand Marais","Houghton")
#with(reg,lines(longitude[shortloc %in% tmp],latitude[shortloc %in% tmp],col="gray50"))

