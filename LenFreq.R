##############################################################
##  LENGTH FREQUENCY ANALYSIS                               ##
##                                                          ##
##  1. Preliminaries                                        ##
##  2. Explorations (note used in manuscript)               ##
##  3. Presentation-quality graphics                        ##
##                                                          ##
##############################################################

##############################################################
##  1. Preliminaries                                        ##
##############################################################
source("DataInit.R")


##############################################################
##  2. Explorations (note used in manuscript)               ##
##############################################################
# Examine sample size by month and year
xtabs(~fyear+mon,data=kiyiLF)

# Examine LF by month to get a feel for within-year growth
tmp <- filterD(kiyiLF,year %in% c(2005,2006,2011),mon != "Apr")
h <- ggplot(tmp,aes(x=tl)) +
  theme_bw() + theme_hist +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,300),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..,),binwidth=5,fill="gray50",color="black") +
  facet_grid(mon~fyear) +
  labs(x="Total Length (mm)",ylab="Relative Frequency")
h


##############################################################
##  3. Presentation-quality graphics                        ##
##############################################################
# 2014 (Jun-Jul) Length Frequency
png("manuscript/Figs/FigureX_LF2014.PNG",width=4.5,height=3,units="in",pointsize=24,family="sans",res=600)
tmp <- filterD(kiyiLF,year==2014,mon %in% c("Jun","Jul"))
h <- ggplot(tmp,aes(x=tl)) +
  theme_bw() + theme_hist +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,300),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  labs(x="Total Length (mm)",ylab="Relative Frequency")
h
dev.off()

# Progression ... left May in because small n for some Junes
png("manuscript/Figs/FigureX_LFProgression.PNG",width=6.5,height=9,units="in",pointsize=24,family="sans",res=600)
tmp <- filterD(kiyiLF,year>=2001,mon %in% c("May","Jun","Jul"))
lvls <- levels(tmp$fyear)
h <- ggplot(tmp,aes(x=tl)) +
  theme_bw() + theme_hist + theme(axis.text.y=element_blank())
  scale_x_continuous(expand=c(0.02,0),limits=c(40,305),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  facet_wrap(~fyear,nrow=2,dir="v") +
  labs(x="Total Length (mm)",y="Relative Frequency") +
  geom_text(aes(y=0.9),data=data.frame(tl=110,fyear=factor(2004,levels=lvls)),label="11",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=95,fyear=factor(2006,levels=lvls)),label="9",size=5) +
#  geom_text(aes(y=0.45),data=data.frame(tl=80,fyear=factor(2009,levels=lvls)),label="6",size=5) +
#  geom_text(aes(y=0.25),data=data.frame(tl=120,fyear=factor(2010,levels=lvls)),label="6",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=90,fyear=factor(2010,levels=lvls)),label="5",size=5)
h
dev.off()
