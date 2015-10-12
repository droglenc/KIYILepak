## ===========================================================
## Load Packages -- used here and in other scripts
##   other packages loaded as needed in the individual scripts
## ===========================================================
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data
library(lubridate) # to handle dates



## ===========================================================
## Set the random seed for reproducibility (i.e., randomization
##   is used in the "new" sex variable below and in application
##   of the age-length-key.
## ===========================================================
set.seed(84621684)



## ===========================================================
## Load and Initial Manipulations of Length Frequency Data
## ===========================================================
## -----------------------------------------------------------
## Load the LF data
## -----------------------------------------------------------
kiyiLF <- read_excel("data/KIYILepak_2014.xlsx",sheet="LenFreq")

## -----------------------------------------------------------
## Clean up a little
##   handle dates (create year and month variables),
##   recode vessel codes
##   rename some variables so not all caps
##   create an average depth
##   remove unused variables
## -----------------------------------------------------------
kiyiLF %<>%
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


## -----------------------------------------------------------
## Expend the LF data ... the LF data are recorded as the
##   frequency of fish for each length by year.  These data
##   must be expanded to individual lengths for each year.
##
## repeat row index as many times as TotalCount
## make a new data.frame with those row indices (will repeat as necessary)
## drop the "ActualCount" and "TotalCount" columns
## Reduce length frequency sample to only those fish caught in
##   May, June, or July ... to be consistent with Taylor's samples.
##   Actually May is too early but need to have adequate sample
##   sizes in some years.
## Removed fish captured prior to 1992 because small sample sizes.
## -----------------------------------------------------------
reprows <- rep(1:nrow(kiyiLF),kiyiLF$TotalCount)
kiyiLens <- kiyiLF[reprows,] %>%
  select(-c(ActualCount,TotalCount)) %>%
  filterD(mon %in% c("May","Jun","Jul"),year>=1992)


## -----------------------------------------------------------
## Write out to a CSV file
## -----------------------------------------------------------
write.csv(kiyiLens,"data/kiyiLens.csv",row.names=FALSE)


## -----------------------------------------------------------
## Clean objects from memory
## -----------------------------------------------------------
rm(kiyiLens,kiyiLF,reprows)
