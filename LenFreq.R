##############################################################
##  LENGTH FREQUENCY ANALYSIS                               ##
##                                                          ##
##  1. Preliminaries                                        ##
##  2. Explorations (not used in manuscript)                ##
##  3. Examine Length Frequency by Region                   ##
##  4. Length-Frequency for 2014                            ##
##  5. Length Frequency for 2001-2014                       ##
##                                                          ##
##############################################################

##############################################################
##  1. Preliminaries                                        ##
##############################################################
# Get the main LF data.frame ... in kiyiLF
rm(list=ls()); cat("\014")
source("DataInit.R")
# Restrict to fish >-140 mm in Jun-Jul of 2014
kiyiLF14 <- kiyiLF %>%
  filterD(year==2014,mon %in% c("Jun","Jul"),tl>=140)
# Split by region for comparisons below
westLF  <- filterD(kiyiLF14,region=="West") %>% as.data.frame()
isleLF  <- filterD(kiyiLF14,region=="Isle") %>% as.data.frame()
northLF <- filterD(kiyiLF14,region=="North") %>% as.data.frame()
southLF <- filterD(kiyiLF14,region=="South") %>% as.data.frame()
eastLF  <- filterD(kiyiLF14,region=="East") %>% as.data.frame()


##############################################################
##  2. Explorations (not used in manuscript)                ##
##                                                          ##
##  Youngest fish do not exhibit growth any earlier than    ##
##    August.  Thus, if sample is reduced to Jun & Jul      ##
##    then there should not be substantial plus growth.     ##
##                                                          ##
##############################################################
# Examine sample size by month and year
xtabs(~fyear+mon,data=kiyiLF)

# Examine LF by month to get a feel for within-year growth
tmp <- filterD(kiyiLF,year %in% c(2005,2006,2011),mon != "Apr")
ggplot(tmp,aes(x=tl)) +
  theme_mhist() +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,300),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..,),binwidth=5,fill="gray50",color="black") +
  facet_grid(mon~fyear) +
  labs(x="Total Length (mm)",y="Relative Frequency")


##############################################################
##  3. Examine Length Frequency by Region                   ##
##                                                          ##
##  North region seems to be significant different than all ##
##    other regions, which did not differ significantly.    ##
##    North region fish seem to be clustered more at the    ##
##    intermediate sizes and "large" fish are not present.  ##
##                                                          ##
##############################################################
# pairwise bootstrapped Kolmogorov-Smirnov tests
nboots <- 1000
ks.pvals <- c(ks.boot(westLF$tl,isleLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(westLF$tl,northLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(westLF$tl,southLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(westLF$tl,eastLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(isleLF$tl,northLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(isleLF$tl,southLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(isleLF$tl,eastLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(northLF$tl,southLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(northLF$tl,eastLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(southLF$tl,eastLF$tl,nboots=nboots)$ks.boot.pvalue)
data.frame(comparisons=apply(combn(regL,2),MARGIN=2,FUN=paste,collapse=" v. "),
           adj.pval=p.adjust(ks.pvals))

# ECDF plots
ggplot(kiyiLF14,aes(tl,color=region)) +
  theme_kiyi() +
  geom_line(stat="ecdf",size=1.25) +
  labs(x="Total Length (mm)",y="Cumulative Density") +
  scale_color_manual(values=clrs)

# LF histograms
ggplot(kiyiLF14,aes(x=tl)) +
  theme_mhist() +
  scale_x_continuous(expand=c(0.02,0),limits=c(140,300),breaks=seq(0,350,25)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  facet_wrap(~region,nrow=1,dir="v") +
  labs(x="Total Length (mm)",y="Relative Frequency")



##############################################################
##  4. Length-Frequency for 2014                            ##
##############################################################
png("manuscript/Figs/FigureX_LF2014.PNG",width=4.5,height=3,units="in",pointsize=24,family="sans",res=600)
ggplot(filterD(kiyiLF,year==2014,mon %in% c("Jun","Jul")),aes(x=tl)) +
  theme_kiyi() +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,300),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  labs(x="Total Length (mm)",y="Relative Frequency")
dev.off()



##############################################################
##  5. Length Frequency for 2001-2014                       ##
##############################################################
png("manuscript/Figs/FigureX_LFProgression.PNG",width=6.5,height=9,units="in",pointsize=24,family="sans",res=600)
tmp <- filterD(kiyiLF,year>=2001,mon %in% c("May","Jun","Jul"))
lvls <- levels(tmp$fyear)
ggplot(tmp,aes(x=tl)) +
  theme_mhist() +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,305),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  facet_wrap(~fyear,nrow=2,dir="v") +
  labs(x="Total Length (mm)",y="Relative Frequency") +
  geom_text(aes(y=0.9),data=data.frame(tl=110,fyear=factor(2004,levels=lvls)),label="11",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=95,fyear=factor(2006,levels=lvls)),label="9",size=5) +
#  geom_text(aes(y=0.45),data=data.frame(tl=80,fyear=factor(2009,levels=lvls)),label="6",size=5) +
#  geom_text(aes(y=0.25),data=data.frame(tl=120,fyear=factor(2010,levels=lvls)),label="6",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=90,fyear=factor(2010,levels=lvls)),label="5",size=5)
dev.off()
