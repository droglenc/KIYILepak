##############################################################
##                                                          ##
## Initial data manipulation for Kiyi Age & Growth Project  ##
##  This is sourced by the other scripts.                   ##
##                                                          ##
##  1. Load packages needed here and in other scripts       ##
##  2. Set up colors and names for the regions              ##
##  3. Read and manipulate ageing data.frame                ##
##  4. Read and manipulate length frequency data.frame      ##
##  5. Make a histogram ggplot2 theme                       ##
##                                                          ##
##############################################################

##############################################################
##  1. Load packages needed here and in other scripts       ##
##############################################################
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data
library(lubridate) # to handle dates
library(ggplot2)
library(nnet)      # multinom


##############################################################
##  2. Set up colors and names for the regions              ##
##############################################################
regS <- c("West","Isle","North","South","East")
regL <- c("Western Arm","Isle Royale","North Ontario","South Ontario","East Keweenaw")
clrs <- c("black","blue","green","orange","red")
names(clrs) <- regS


##############################################################
##  3. Read and manipulate main data file                   ##
##############################################################
# add 10-mm length categories
# add log10 of weight and length
# make Reg factor variable
# make a Sex factor variable (after mappling to words)
kiyiAge <- read_excel("data/KIYILepak_2014.xlsx",sheet="Ageing") %>%
  mutate(lcat10=lencat(tl,w=10),
         logW=log10(wt),logL=log10(tl),
         region=factor(region,levels=regS),
         sex=factor(mapvalues(sex,from=c(0,1,2),to=c("juvenile","male","female"))))
headtail(kiyiAge)


##############################################################
##  4. Read and manipulate length frequency data.frame      ##
##############################################################
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
##  5. Make a histogram ggplot2 theme                       ##
##############################################################
theme_hist <- theme(panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.border = element_rect(color="black",size=1.25),
                    strip.background = element_rect(color="black",size=1.25),
                    strip.text=element_text(face="bold",size=11),
                    axis.text=element_text(size=14),
                    axis.title=element_text(size=18),
                    axis.title.x=element_text(margin=margin(8,0,0,0)),
                    axis.title.y=element_text(margin=margin(0,16,0,0)))
