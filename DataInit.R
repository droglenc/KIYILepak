##############################################################
##                                                          ##
## Initial data manipulation for Kiyi Age & Growth Project  ##
##  This is sourced by the other scripts.                   ##
##                                                          ##
##  1. Read and manipulate ageing data.frame                ##
##  2. Read and manipulate station information data.frame   ##
##  3. Load the length frequency analysis script            ##
##  4. Basic summaries                                      ##
##                                                          ##
##############################################################

# clear workspace and console
rm(list=ls()); cat("\014")
# load the helpers
source("aaaInit.R")

##############################################################
##  1. Read and manipulate main data file                   ##
##############################################################
# add 10-mm length categories
# add log10 of weight and length
# make Reg factor variable
# make a Sex factor variable (after mappling to words)
# make a long region variable
kiyiAge <- read_excel("data/KIYILepak_2014.xlsx",sheet="Ageing") %>%
  mutate(lcat10=lencat(tl,w=10),
         logW=log10(wt),logL=log10(tl),
         region=factor(mapvalues(region,regSold,regS),levels=regS),
         regL=factor(mapvalues(region,regS,regL),levels=regL),
         sex=factor(mapvalues(sex,from=c(0,1,2),to=c("juvenile","male","female"))))


##############################################################
##  2. Read stations information data.frame                 ##
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
         region=factor(mapvalues(region,regSold,regS),levels=regS),
         regionL=factor(mapvalues(region,regS,regL),levels=regL),
         type=factor(type)) %>%
  select(-op_date) %>%
  filterD(location!=2) %>%
  as.data.frame()



##############################################################
##  3. Read the length frequency analysis script            ##
##     brings in kiyiLF14 data.frame                        ##
##############################################################
# Read the raw LF data from
kiyiLF <- read_excel("data/LF_2001_14_forMark_corrected.xlsx") %>%
  filter(use=="yes") %>%
  mutate(region=mapvalues(region,regSold,regS),
         region=factor(region,levels=regS),
         regL=mapvalues(region,regS,regL),
         fyear=factor(year),
         region=factor(region),
         mon=factor(mon)) %>%
  select(-c(op_id,op_date,serial,cruise,target,use,tr_design,
            beg_depth,end_depth,year))

# Expend the raw LF data ... the raw LF data are recorded as the
#   frequency of fish for each length by year.  These data
#   must be expanded to individual lengths for each year.
#     Repeat row index as many times as TotalCount
#     Make a new data.frame with those row indices (will repeat as necessary)
#     Drop the "ActualCount" and "TotalCount" columns
# Added a 5-mm length category variale
reprows <- rep(1:nrow(kiyiLF),kiyiLF$totalcount)
kiyiLF <- kiyiLF[reprows,] %>%
  select(-c(actualcount,totalcount)) %>%
  mutate(lcat5=lencat(tl,w=5)) %>%
  as.data.frame()

# Get just 2014 (also remove fish from location==2 (only May sample))
kiyiLF14 <- kiyiLF %>%
  filterD(fyear==2014,location!=2)



##############################################################
##  4. Basic Summaries                                      ##
##############################################################
# Number of locations where Kiyi were captured
nrow(kiyiStations)
xtabs(~region,data=kiyiStations)    ## in Table 2

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
Summarize(tl~region,data=kiyiLF14,digits=2)   ## in Table 2

# basic length summary of the subsample (not in the manuscript)
Summarize(~tl,data=kiyiAge,digits=2)

# sex ratio summary of the subsample
xtabs(~region+sex,data=kiyiAge)


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
