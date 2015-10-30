##############################################################
##  LENGTH FREQUENCY DATA PRELIMINARIES                     ##
##                                                          ##
##  This was used to create a data.frame that was           ##
##  restricted to May-Jul of 2001-2014.  This was then sent ##
##  to Mark Vinson as a CSV vile where he added a column    ##
##  called use that indicates whether we could use that     ##
##  record or not based on USGS criteria.  This modified    ##
##  CSV file is loaded in LenFreq.R and analyzed for the   ##
##  manuscript.  THIS SCRIPT SHOULD NOT BE RUN AGAIN!!      ##
##                                                          ##
##############################################################

# clear workspace and console
rm(list=ls()); cat("\014")
# load required packages
library(readxl)
library(dplyr)
library(magrittr)
library(FSA)
# read in the LF data
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
  filterD(year>=2001,mon %in% c("May","Jun","Jul"))

#write.csv(kiyiLF,"LF_2001_14_forMark2.csv",row.names=FALSE,quote=FALSE)
