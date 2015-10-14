##############################################################
##                                                          ##
## Basic Summaries                                          ##
##                                                          ##
## 1. Prepare data                                          ##
##                                                          ##
##############################################################

##############################################################
## 1. Prepare data                                          ##
##############################################################
# Get raw data and load packages
source("DataInit.R")


##############################################################
##  2. Sample sizes                                         ##
##############################################################
# For all fish in the sample (in theory there should be 5 fish
# per 10-mm length bin for each sex within each region ...
# except that all fish <160 mm and greater than 280 mm were
# collected.)
xtabs(~lcat10+sex+region,data=kiyiAge)
# For all fish that received a consensus otolith age
xtabs(~lcat10+sex+region,data=filterD(kiyiAge,!is.na(otoAge)))


