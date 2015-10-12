##############################################################
##############################################################
##  Kiyi (Taylor Lepak et al.) manuscript work
##
##  LENGTH FREQUENCY ANALYSIS SETUP SCRIPT
##
##  1. Preliminaries
##  2. Make the data.frame of lengths
##  3. Explorations (note used in manuscript)
##  4. Presentation-quality graphics
##
##############################################################
##############################################################

##############################################################
##  1. Preliminaries
##############################################################
# Clear the environment first
rm(list = ls(all.names=TRUE))
# Load the helper files
source("zzzHelpers.R")
# Load packages
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data
library(lubridate) # to handle dates
library(ggplot2)


##############################################################
##  2. Make the data.frame of lengths
##############################################################
# Load the raw LF data that came from USGS and clean ...
#   Use only those records with use==1 (TRUE),
#   Handle dates (create year and month variables),
#   Rename some variables so not all caps
#   Create an average depth
#   Remove unused variables
kiyiLF <- read_excel("data/KIYILepak_2014.xlsx",sheet="LenFreq") %>%
  filterD(Use==1) %>%
  mutate(op_date=ymd(OP_DATE),
         year=year(op_date),
         year=ifelse(year>2020,year-100,year),
         fyear=factor(year),
         mon=month(op_date,label=TRUE),
         tl=LENGTH,
         beg_depth=BEG_DEPTH,
         end_depth=END_DEPTH,
         avg_depth=(beg_depth+end_depth)/2) %>%
  select(-c(OP_DATE,BEG_DEPTH,END_DEPTH,LENGTH,TARGET,TR_DESIGN))

# Expend the raw LF data ... the raw LF data are recorded as the
#   frequency of fish for each length by year.  These data
#   must be expanded to individual lengths for each year.
#     Repeat row index as many times as TotalCount
#     Make a new data.frame with those row indices (will repeat as necessary)
#     Drop the "ActualCount" and "TotalCount" columns
# Removed fish captured prior to 1992 because small sample sizes.
# Added a 5-mm length category variale
reprows <- rep(1:nrow(kiyiLF),kiyiLF$TotalCount)
kiyiLens <- kiyiLF[reprows,] %>%
  select(-c(ActualCount,TotalCount)) %>%
  filterD(year>=1992) %>%
  mutate(lcat5=lencat(tl,w=5))


##############################################################
##  3. Explorations (not used in manuscript)
##############################################################
# Examine sample size by month and year
xtabs(~fyear+mon,data=kiyiLens)

# Set some generalities for the histograms below
Summarize(tl~fyear,data=kiyiLens)
Summarize(~tl,data=kiyiLens)
xlim <- c(20,380)
len.ticks <- brks <- seq(0,500,5)
ylim <- c(0,15)
freq.ticks <- seq(ylim[1],ylim[2],5)

# Examine LF by month to get a feel for within-year growth
#   Use 2005, 2006, 2011 as representative years
windows(5,10,record=TRUE)
par(mfrow=c(7,1),mar=c(3,3,0.25,0.25),mgp=c(1.7,0.4,0),tcl=-0.25,las=1,xaxs="i",yaxs="i")
yrs <- c(2005,2006,2011)
mons <- c("May","Jun","Jul","Aug","Sep","Oct","Nov")
for (i in yrs) {
  tmp <- filter(kiyiLens,year==i,mon %in% mons)
  for (j in mons) {
    kiyiHist(filter(tmp,mon==j),breaks=brks,xlim=c(50,300),ylim=ylim,
             clr="gray90",len.ticks=len.ticks,freq.ticks=freq.ticks,lbl=j)
  }
}


# Examine LF by year
# Reduce length frequency sample to only those fish caught in
##   May, June, or July ... to be consistent with Taylor's samples.
##   Actually May is too early but need to have adequate sample
##   sizes in some years.
## -----------------------------------------------------------
kiyiLens %<>% filterD(mon %in% c("May","Jun","Jul"))

# Goto the graphics windows and use PgUp and PgDn to move
#   through the histograms
windows(7,6,record=TRUE)
par(mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2)
for (i in 1992:2014) {
  kiyiHist(filter(kiyiLens,year==i),breaks=brks,xlim=xlim,ylim=ylim,clr="gray90",
           len.ticks=len.ticks,freq.ticks=freq.ticks,xlab="tl",ylab="Frequency",lbl=i)
}



##############################################################
##  4. Presentation-quality graphics
##############################################################
# 2014 (May-Jul) Length Frequency
windows(7,6)
par(mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2,cex=1.75,lwd=2,las=1)
xlim <- c(25,300)
kiyiHist(filter(LF,YEAR==2014),breaks=brks,xlim=xlim,ylim=ylim,clr="gray50",len.ticks=len.ticks,freq.ticks=freq.ticks,xlab="Total Length (mm)",ylab="Percentage",show.axes=FALSE)
axis(1,seq(25,300,25),lwd=3,pos=0)
axis(2,seq(0,15,5),lwd=3)
abline(h=0,lwd=3)

#-------------------------------------------------------------
# Progression
#-------------------------------------------------------------
png("manuscript/Figs/Figure3.PNG",width=6.5,height=9,units="in",pointsize=24,family="sans",res=600)
tmp <- filterD(kiyiLens,year>=2001)
lvls <- levels(tmp$fyear))
h <- ggplot(tmp,aes(x=tl))
h <- h + theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(color="black",size=1.25),
        strip.background = element_rect(color="black",size=1.25),
        strip.text=element_text(face="bold"),
        axis.text.y=element_blank()) +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,300),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..,),binwidth=5,fill="gray50",color="black")
h <- h + facet_wrap(~fyear,nrow=2,dir="v") +
  labs(x="Total Length (mm)",ylab="Relative Frequency")
h <- h +
  geom_text(aes(y=0.9),data=data.frame(tl=110,fyear=factor(2004,levels=lvls),label="11") +
  geom_text(aes(y=0.35),data=data.frame(tl=95,fyear=factor(2006,levels=lvls),label="9") +
  geom_text(aes(y=0.45),data=data.frame(tl=80,fyear=factor(2009,levels=lvls),label="6") +
  geom_text(aes(y=0.25),data=data.frame(tl=120,fyear=factor(2010,levels=lvls),label="6") +
  geom_text(aes(y=0.4),data=data.frame(tl=90,fyear=factor(2010,levels=lvls),label="5")
dev.off()
