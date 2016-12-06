##############################################################
##                                                          ##
## Create helper items for other scripts in Taylor Lepak's  ##
##  Kiyi Age & Growth Project.  This is sourced by the      ##
##  other scripts.                                          ##
##                                                          ##
##  1. Load packages needed here and in other scripts       ##
##  2. Set up colors and names for the regions              ##
##  3. Make ggplot2 themes                                  ##
##                                                          ##
##############################################################

# clear workspace and console
rm(list=ls()); cat("\014")

##############################################################
##  1. Load packages needed here and in other scripts       ##
##############################################################
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD, dunnTest
library(lubridate) # to handle dates
library(ggplot2)
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data

library(knitr)
opts_chunk$set(prompt=TRUE,comment='')

##############################################################
##  2. Make some ggplot2 themes                             ##
##############################################################
theme_kiyi <- function (base_size = 12, base_family = "") {
  theme_bw(base_size=base_size,base_family=base_family) +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border = element_rect(color="black",size=1.25),
          strip.background = element_rect(color="black",size=1.25),
          strip.text=element_text(face="bold",size=11),
          axis.text=element_text(size=14,color="black"),
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
