##############################################################
##                                                          ##
## Initial data manipulation for Kiyi Age & Growth Project  ##
##  This is sourced by the other scripts.                   ##
##                                                          ##
##  1. Load packages needed here and in other scripts       ##
##  2. Set up colors and names for the regions              ##
##  3. Read and manipulate main data file                   ##
##                                                          ##
##############################################################

##############################################################
##  1. Load packages needed here and in other scripts       ##
##############################################################
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data
library(NCStats)


##############################################################
##  2. Set up colors and names for the regions              ##
##############################################################
regS <- c("East","Isle","North","South","West")
regL <- c("East Keweenaw","Isle Royale","North Ontario","South Ontario","Western Arm")
clrs <- c("black","blue","green","orange","red")
names(clrs) <- regS


##############################################################
##  3. Read and manipulate main data file                   ##
##############################################################
# add 10-mm length categories
# add log10 of weight and length
# make Reg factor variable
# make a Sex factor variable (after mappling to words)
kiyial <- read_excel("data/KIYILepak_2014.xlsx",sheet="Ageing") %>%
  mutate(lcat10=lencat(tl,w=10),
         logW=log10(wt),logL=log10(tl),
         region=factor(region,levels=regS),
         sex=factor(mapvalues(sex,from=c(0,1,2),to=c("juvenile","male","female"))))
headtail(kiyial)
