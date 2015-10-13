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
# Load packages
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data
library(lubridate) # to handle dates
library(ggplot2)
# make a histogram theme
theme_hist <- theme(panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.border = element_rect(color="black",size=1.25),
                    strip.background = element_rect(color="black",size=1.25),
                    strip.text=element_text(face="bold",size=11),
                    axis.text.y=element_blank(),
                    axis.text=element_text(size=14),
                    axis.title=element_text(size=18),
                    axis.title.x=element_text(margin=margin(8,0,0,0)),
                    axis.title.y=element_text(margin=margin(0,16,0,0)))

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
kiyiLF <- kiyiLF[reprows,] %>%
  select(-c(ActualCount,TotalCount)) %>%
  filterD(year>=1992) %>%
  mutate(lcat5=lencat(tl,w=5))


##############################################################
##  3. Explorations (not used in manuscript)
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
##  4. Presentation-quality graphics
##############################################################
# 2014 (May-Jul) Length Frequency
png("manuscript/Figs/FigureX_LF2014.PNG",width=4.5,height=3,units="in",pointsize=24,family="sans",res=600)
tmp <- filterD(kiyiLF,year==2014,mon %in% c("May","Jun","Jul"))
h <- ggplot(tmp,aes(x=tl)) +
  theme_bw() + theme_hist +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,300),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  labs(x="Total Length (mm)",ylab="Relative Frequency")
h
dev.off()

# Progression
png("manuscript/Figs/FigureX_LFProgression.PNG",width=6.5,height=9,units="in",pointsize=24,family="sans",res=600)
tmp <- filterD(kiyiLF,year>=2001,mon %in% c("May","Jun","Jul"))
lvls <- levels(tmp$fyear)
h <- ggplot(tmp,aes(x=tl)) +
  theme_bw() + theme_hist +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,305),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..,),binwidth=5,fill="gray50",color="black") +
  facet_wrap(~fyear,nrow=2,dir="v") +
  labs(x="Total Length (mm)",y="Relative Frequency") +
  geom_text(aes(y=0.9),data=data.frame(tl=110,fyear=factor(2004,levels=lvls)),label="11",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=95,fyear=factor(2006,levels=lvls)),label="9",size=5) +
#  geom_text(aes(y=0.45),data=data.frame(tl=80,fyear=factor(2009,levels=lvls)),label="6",size=5) +
#  geom_text(aes(y=0.25),data=data.frame(tl=120,fyear=factor(2010,levels=lvls)),label="6",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=90,fyear=factor(2010,levels=lvls)),label="5",size=5)
h
dev.off()
