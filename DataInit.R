##############################################################
##                                                          ##
## Initial data manipulation for Kiyi Age & Growth Project  ##
##  This is sourced by the other scripts.                   ##
##                                                          ##
##  1. Load packages needed here and in other scripts       ##
##  2. Set up colors and names for the regions              ##
##  3. Read and manipulate ageing data.frame                ##
##  4. Read and manipulate length frequency data.frame      ##
##  5. Read and manipulate station information data.frame   ##
##  6. Basic summaries                                      ##
##  7. Make a histogram ggplot2 theme                       ##
##                                                          ##
##############################################################

# clear workspace and console
rm(list=ls()); cat("\014")

##############################################################
##  1. Load packages needed here and in other scripts       ##
##############################################################
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD
library(lubridate) # to handle dates
library(ggplot2)
library(nnet)      # multinom
library(Matching)  # ks.boot
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data


##############################################################
##  2. Set up colors and names for the regions              ##
##############################################################
regS <- c("West","Isle","North","South","East")
regL <- c("Western Arm","Isle Royale","Northern Ontario",
          "Southern Ontario","Eastern Michigan")
clrs <- c("black","blue","green","orange","red")
names(clrs) <- regS


##############################################################
##  3. Read and manipulate main data file                   ##
##############################################################
# add 10-mm length categories
# add log10 of weight and length
# make Reg factor variable
# make a Sex factor variable (after mappling to words)
# make a long region variable
kiyiAge <- read_excel("data/KIYILepak_2014.xlsx",sheet="Ageing") %>%
  mutate(lcat10=lencat(tl,w=10),
         logW=log10(wt),logL=log10(tl),
         region=factor(region,levels=regS),
         regionL=factor(mapvalues(region,regS,regL),levels=regL),
         sex=factor(mapvalues(sex,from=c(0,1,2),to=c("juvenile","male","female"))))



##############################################################
##  4. Read and manipulate length frequency data.frame      ##
##############################################################
# Load the raw LF data that came from USGS and clean ...
#   Use only those records with use==1 (TRUE),
#   Handle dates (create year and month variables),
#   Rename some variables so not all caps
#   make region a factor variable
#   Create an average depth
#   Remove unused variables
kiyiLF <- read_excel("data/KIYILepak_2014.xlsx",sheet="LenFreq") %>%
  filterD(Use==1) %>%
  setNames(tolower(names(.))) %>%
  mutate(op_date=ymd(op_date),
         year=year(op_date),
         year=ifelse(year>2020,year-100,year),
         fyear=factor(year),
         mon=month(op_date,label=TRUE),
         tl=length,
         region=factor(region,levels=regS),
         regionL=factor(mapvalues(region,regS,regL),levels=regL)) %>%
  select(-c(op_id,op_date,serial,cruise,target,tr_design,
            beg_depth,end_depth,use,usenotes))

# Expend the raw LF data ... the raw LF data are recorded as the
#   frequency of fish for each length by year.  These data
#   must be expanded to individual lengths for each year.
#     Repeat row index as many times as TotalCount
#     Make a new data.frame with those row indices (will repeat as necessary)
#     Drop the "ActualCount" and "TotalCount" columns
# Removed fish captured prior to 1992 because small sample sizes.
# Added a 5-mm length category variale
reprows <- rep(1:nrow(kiyiLF),kiyiLF$totalcount)
kiyiLF <- kiyiLF[reprows,] %>%
  select(-c(actualcount,totalcount)) %>%
  filterD(year>=1992) %>%
  mutate(lcat5=lencat(tl,w=5)) %>%
  as.data.frame()

# Get the LF data for only 2014 ... Restrict to Jun-Jul
kiyiLF14 <- kiyiLF %>%
  filterD(year==2014,mon %in% c("Jun","Jul")) %>%
  as.data.frame()



##############################################################
##  5. Read stations information data.frame                 ##
##############################################################
# renamed to lower_case
# handled dates and regions
# removed several variables that were no longer needed
# removed location==2 (May and different region than others)
kiyiStations <- read_excel("data/KIYILepak_2014.xlsx",sheet="Stations") %>%
  setNames(tolower(names(.))) %>%
  mutate(op_date=ymd(op_date),
         year=year(op_date),
         year=ifelse(year>2020,year-100,year),
         mon=month(op_date,label=TRUE),
         region=factor(region,levels=regS),
         regionL=factor(mapvalues(region,regS,regL),levels=regL),
         type=factor(type)) %>%
  select(-op_date) %>%
  filterD(location!=2) %>%
  as.data.frame()



##############################################################
##  6. Basic Summaries                                      ##
##############################################################
# Number of locations where Kiyi were captured
nrow(kiyiStations)
# Summarize depths & distances for nearshore cross-contour tows (82 & 84)
tmp <- filterD(kiyiStations,type=="nearshore")
Summarize(~beg_depth,data=tmp,digits=0)
Summarize(~end_depth,data=tmp,digits=0)
Summarize(~distance,data=tmp,digits=2)
# Summarize depths for offshore along-contour tows (not 82 & 84)
tmp <- filterD(kiyiStations,type=="offshore")
Summarize(~avg_depth,data=tmp,digits=0)
Summarize(~distance,data=tmp,digits=2)

# basic length summary of the entire sample (for the manuscript)
Summarize(~tl,data=kiyiLF14,digits=2)

# basic length summary of the subsample (not in the manuscript)
Summarize(~tl,data=kiyiAge,digits=2)

# sex ratio summary of the subsample for the manuscript
xtabs(~region+sex,data=kiyiAge)
prop.table(xtabs(~sex,data=filterD(kiyiAge,sex!="juvenile")))*100
sextbl <- xtabs(~region+sex,data=filterD(kiyiAge,sex!="juvenile"))
chisq.test(sextbl)

# Examine the subsampling scheme (in theory there should be 5
# fish per 10-mm length bin for each sex within each region ...
# except that all fish <160 mm and greater than 280 mm were
# collected.)
xtabs(~lcat10+sex+region,data=kiyiAge)
# For all fish that received a consensus otolith age
xtabs(~lcat10+sex+region,data=filterD(kiyiAge,!is.na(otoAge)))

# summary of the otolith characteristics
( octbl <- xtabs(~otoChar,data=kiyiAge) )
# find proportion unuseable
prop.table(octbl)*100
# find proportion of useable that were unreadable
prop.table(octbl[-2])*100


##############################################################
##  7. Make some ggplot2 themes                             ##
##############################################################
theme_kiyi <- function (base_size = 12, base_family = "") {
  theme_bw(base_size=base_size,base_family=base_family) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(color="black",size=1.25),
        strip.background = element_rect(color="black",size=1.25),
        strip.text=element_text(face="bold",size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        axis.title.x=element_text(margin=margin(8,0,0,0)),
        axis.title.y=element_text(margin=margin(0,16,0,0)),
        legend.title=element_blank(),
        legend.text=element_text(size=14))
}

theme_mhist <- function (base_size = 12, base_family = "") {
  theme_kiyi(base_size=base_size,base_family=base_family) +
  theme(axis.text.y=element_blank())
}
