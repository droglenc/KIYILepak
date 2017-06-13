## Read helper code
source("code/aaaInit.R")
## Set random seed ... bootstraps and ALKs will be the same each time
set.seed(34783475)

################################################################################
## Load and manipulate the length-frequency data from 2014.
lf14 <- read.csv("data/clean/lenfreq_2014.csv") %>%
  mutate(year=factor(year),
         mon=factor(mon,levels=c("May","Jun","Jul")))

#### For first paragraph of Results
## months when Kiyi were collected
xtabs(~mon,data=lf14)
## locations where Kiyi were collected (and number of locations)
( lf14_locs <- xtabs(~location,data=lf14) )
length(lf14_locs)
## locations where Kiyi were collected by month
xtabs(~mon+location,data=lf14)
## basic length summary of the entire sample
Summarize(~tl,data=lf14)



################################################################################
## Load the Biological Data (for 2014)
bio <- read.csv("data/clean/bio.csv") %>%
  mutate(sex=factor(sex,levels=c("juvenile","female","male")))

#### For second paragraph of Results
## summary of number of scales
( sctbl <- xtabs(~scaleAge,data=bio) )
sum(sctbl)

##    Between-reader scale comparisons (TAL v. DHO ages)
bio_SS <- filterD(bio,!is.na(scaleAge))
ab_SS <- ageBias(scaleAge_TAL~scaleAge_DHO,data=bio_SS,
                 nref.lab="Reader 2",ref.lab="Reader 1")
summary(ab_SS,what="bias")
summary(ab_SS,what="symmetry")
ap_SS <- agePrecision(scaleAge_TAL~scaleAge_DHO,data=bio_SS)
summary(ap_SS,what="precision")
summary(ap_SS,what="absolute difference")
plot(ab_SS,show.CI=TRUE)

##    summary of the otolith characteristics
( octbl <- xtabs(~otoChar,data=bio) )
# find proportion unuseable
prop.table(octbl)*100
# find proportion of useable that were unreadable
octbl2 <- octbl[-2]
prop.table(octbl2)*100

##    Between-reader otolith comparisons (TAL v. DHO ages)
bio_OO <- filterD(bio,otoChar=="USEABLE")
ab_OO <- ageBias(otoAge_TAL~otoAge_DHO,data=bio_OO,
                 nref.lab="Reader 2",ref.lab="Reader 1")
summary(ab_OO,what="bias")
summary(ab_OO,what="symmetry")
ap_OO <- agePrecision(otoAge_TAL~otoAge_DHO,data=bio_OO)
summary(ap_OO,what="precision")
summary(ap_OO,what="absolute difference")
summary(ap_OO,what="absolute difference",trunc.diff=2)

## Otolith-scales comparisons (only TAL ages)
bio_OS <- bio[complete.cases(bio[,c("scaleAge","otoAge")]),]
ab_OS <- ageBias(scaleAge~otoAge,data=bio_OS,
                 ref.lab="Otolith Age",nref.lab="Scale Age")
ab_OS_bsum <- summary(ab_OS,what="bias")
ab_OS_bsym <- summary(ab_OS,what="symmetry")
plot(ab_OS,show.CI=TRUE)

#### For third paragraph of Results
xtabs(~sex+otoAge,data=filterD(bio,otoChar=="USEABLE"))
xtabs(~sex+scaleAge,data=bio)

# Remove juvenile fish (same as removing all fish <140 mm) and those without otoages
bio_ALK <- filterD(bio,sex!="juvenile",!is.na(otoAge))
# age frequency
( bio_ALK_agefreq <- xtabs(~otoAge,data=bio_ALK) )
# Get raw LF 2014 data, restrict to fish >= 140 mm, and add an
# otoAge variable to record the new ages
lf14_ages <- lf14 %>%
  mutate(otoAge=as.numeric(NA)) %>%
  filter(tl>=140) %>%
  as.data.frame()
# develop ALK
alk  <- prop.table(xtabs(~lcat10+otoAge,data=bio_ALK),margin=1)
# apply ALK
lf14_ages  <- alkIndivAge(alk,otoAge~tl,data=lf14_ages)
# Age frequency tables
xtabs(~otoAge,data=lf14_ages)


#### Modal progression for discussion
lf <- read.csv("data/clean/lenfreq_all.csv") %>%
  mutate(year=factor(year)) %>%
  filterD(year %in% 2003:2014)

library(mixdist)
dtype <- "lnorm"
ctype <- mixconstr(consigma="CCV")

## Trying to following 2003 year-class
lf04 <- filterD(lf,year==2004) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf04_fit <- mix(lf04,mixparam(c(100,200),10),dist=dtype,constr=ctype)
plot(lf04_fit,dist=dtype)
lf04_sum <- summary(lf04_fit)
ycl03_res <- data.frame(year=2004,age=1,
                        mntl=lf04_sum$parameters$mu[1],
                        sdtl=lf04_sum$parameters$sigma[1],
                        setl=lf04_sum$se$mu.se[1])

lf05 <- filterD(lf,year==2005) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf05_fit <- mix(lf05,mixparam(c(100,200),10),dist=dtype,constr=ctype)
plot(lf05_fit,dist=dtype)
lf05_sum <- summary(lf05_fit)
ycl03_res <- rbind(ycl03_res,c(2005,2,lf05_sum$parameters$mu[1],
                               lf05_sum$parameters$sigma[1],lf05_sum$se$mu.se[1]))

lf06 <- filterD(lf,year==2006) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf06_fit <- mix(lf06,mixparam(c(100,150,200),10),dist=dtype,constr=ctype)
plot(lf06_fit,dist=dtype)
lf06_sum <- summary(lf06_fit)
ycl03_res <- rbind(ycl03_res,c(2006,3,lf06_sum$parameters$mu[2],
                               lf06_sum$parameters$sigma[2],lf06_sum$se$mu.se[2]))

lf07 <- filterD(lf,year==2007) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf07_fit <- mix(lf07,mixparam(c(120,160,200),10),dist=dtype,constr=ctype)
plot(lf07_fit,dist=dtype)
lf07_sum <- summary(lf07_fit)
ycl03_res <- rbind(ycl03_res,c(2007,4,lf07_sum$parameters$mu[2],
                               lf07_sum$parameters$sigma[2],lf07_sum$se$mu.se[2]))

lf08 <- filterD(lf,year==2008) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf08_fit <- mix(lf08,mixparam(c(160,200),10),dist=dtype,constr=ctype)
plot(lf08_fit,dist=dtype)
lf08_sum <- summary(lf08_fit)
ycl03_res <- rbind(ycl03_res,c(2008,5,lf08_sum$parameters$mu[1],
                               lf08_sum$parameters$sigma[1],lf08_sum$se$mu.se[1]))

ycl03_res

## Trying to follow the 2009 year-class
lf10 <- filterD(lf,year==2010) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf10_fit <- mix(lf10,mixparam(c(100,200),10),dist=dtype,constr=ctype)
plot(lf10_fit,dist=dtype)
lf10_sum <- summary(lf10_fit)
ycl09_res <- data.frame(year=2010,age=1,
                        mntl=lf10_sum$parameters$mu[1],
                        sdtl=lf10_sum$parameters$sigma[1],
                        setl=lf10_sum$se$mu.se[1])

lf11 <- filterD(lf,year==2011) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf11_fit <- mix(lf11,mixparam(c(130,200),10),dist=dtype,constr=ctype)
plot(lf11_fit,dist=dtype)
lf11_sum <- summary(lf11_fit)
ycl09_res <- rbind(ycl09_res,c(2011,2,lf11_sum$parameters$mu[1],
                               lf11_sum$parameters$sigma[1],lf11_sum$se$mu.se[1]))

lf12 <- filterD(lf,year==2012) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf12_fit <- mix(lf12,mixparam(c(150,200),10),dist=dtype,constr=ctype)
plot(lf12_fit,dist=dtype)
lf12_sum <- summary(lf12_fit)
ycl09_res <- rbind(ycl09_res,c(2012,3,lf12_sum$parameters$mu[1],
                               lf12_sum$parameters$sigma[1],lf12_sum$se$mu.se[1]))

lf13 <- filterD(lf,year==2013) %>%
  group_by(lcat5) %>% summarize(n=n()) %>% as.mixdata()
lf13_fit <- mix(lf13,mixparam(c(160,220),c(10,20)),dist=dtype,constr=ctype)
plot(lf13_fit,dist=dtype)
lf13_sum <- summary(lf13_fit)
ycl09_res <- rbind(ycl09_res,c(2013,4,lf13_sum$parameters$mu[1],
                               lf13_sum$parameters$sigma[1],lf13_sum$se$mu.se[1]))

ycl09_res

################################################################################
## PLOTS

## Which type to make ... varied among journal submissions (tiff for NAJFM)
ptype <- c("PDF","JPG","TIFF")[3]

## Figure 3 ... Scale age comparison
fig3 <- "results/figures/Figure3_ScaleScaleComp"
if (ptype=="JPG") {
  jpeg(paste0(fig3,".jpg"),width=4.5,height=4.5,units="in",pointsize=14,family="sans",quality=100,res=144)
} else if (ptype=="PDF") {
  pdf(paste0(fig3,".pdf"),width=6,height=6,family="Arial",pointsize=14)
} else tiff(paste0(fig3,".tif"),width=3.5,height=3.5,units="in",pointsize=10,family="sans",res=300)
par(mar=c(3,3,0.5,0.75),mgp=c(1.9,0.5,0),tcl=-0.2,las=1)
plot(ab_SS,xlim=c(2.6,8.4),ylim=c(-2.4,2.4),
     ylab="Second Reader - First Reader Age",xlab="First Reader Age")
dev.off()

## Figure 4 ... Otolith age comparison
fig4 <- "results/figures/Figure4_OtoOtoComp"
if (ptype=="JPG") {
  jpeg(paste0(fig4,".jpg"),width=4.5,height=4.5,units="in",pointsize=14,family="sans",quality=100,res=144)
} else if (ptype=="PDF") {
  pdf(paste0(fig4,".pdf"),width=6,height=6,family="Arial",pointsize=14)
} else tiff(paste0(fig4,".tif"),width=3.5,height=3.5,units="in",pointsize=10,family="sans",res=300)
par(mar=c(3,3,0.5,0.75),mgp=c(1.9,0.5,0),tcl=-0.2,las=1)
plot(ab_OO,xlim=c(4,20),ylim=c(-3.4,2.4),
     ylab="Second Reader - First Reader Age",xlab="First Reader Age")
dev.off()

## Figure 5 ... Scale-Otolith age-bias plot
fig5 <- "results/figures/Figure5_ScaleOtoComp"
if (ptype=="JPG") {
  jpeg(paste0(fig5,".jpg"),width=4.5,height=4.5,units="in",pointsize=14,family="sans",quality=100,res=144)
} else if (ptype=="PDF") {
  pdf(paste0(fig5,".pdf"),width=6,height=6,family="Arial",pointsize=14)
} else tiff(paste0(fig5,".tif"),width=3.5,height=3.5,units="in",pointsize=10,family="sans",res=300)
par(mar=c(3,3,0.5,0.5),mgp=c(1.9,0.5,0),tcl=-0.2,las=1)
plot(ab_OS,xlim=c(3.9,16.1),ylim=c(-11.4,1.4),
     ylab="Consensus Scale - Consensus Otolith Age",xlab="Consensus Otolith Age")
dev.off()

## Figure 6 ... Age frequency
fig6 <- "results/figures/Figure6_OtoAgeFreq"
if (ptype=="JPG") {
  jpeg(paste0(fig6,".jpg"),width=4.5,height=4.5,units="in",pointsize=14,family="sans",quality=100,res=144)
} else if (ptype=="PDF") {
  pdf(paste0(fig6,".pdf"),width=6,height=6,family="Arial",pointsize=14)
} else tiff(paste0(fig6,".tif"),width=3.5,height=3.5,units="in",pointsize=10,family="sans",res=300)
par(mar=c(3,3,0.5,0.5),mgp=c(1.9,0.5,0),tcl=-0.2,las=1)
hist(~otoAge,data=lf14_ages,xlab="Age (years)",w=1,
     xaxt="n",yaxt="n",ylim=c(0,300),col="gray70")
axis(1,at=seq(4.5,20.5,1),labels=NA)
axis(1,at=seq(5.5,20.5,5),labels=seq(5,20,5))
axis(2,at=seq(0,300,50),labels=NA)
axis(2,at=seq(0,300,100),labels=seq(0,300,100))
dev.off()

## Figure 7 ... Length Frequency Progression
lf <- read.csv("data/clean/lenfreq_all.csv") %>%
  mutate(year=factor(year)) %>%
  filterD(year %in% 2003:2014)
tmp <- lf %>% group_by(year) %>% summarize(n=n())
lf %<>% mutate(year2=factor(mapvalues(year,levels(year),
                                      paste0(tmp$year," (n=",tmp$n,")"))))
lvls <- levels(lf$year2)

fig7 <- "results/figures/Figure7_LFProgression"
if (ptype=="JPG") {
  jpeg(paste0(fig7,".jpg"),width=6.5,height=9,units="in",pointsize=24,family="sans",quality=100,res=144)
} else if (ptype=="PDF") {
  pdf(paste0(fig7,".pdf"),width=6,height=9,family="Arial",pointsize=24)
} else tiff(paste0(fig7,".tif"),width=7.25,height=10,units="in",pointsize=10,family="sans",res=300)
ggplot(lf,aes(x=tl)) +
  theme_mhist() +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,310),breaks=seq(0,350,50),
                     labels=c("","",100,"",200,"",300,"")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray70",color="black",size=0.1) +
  facet_wrap(~year2,nrow=4,dir="v") +
  labs(x="Total Length (mm)",y="Relative Frequency") +
  geom_vline(xintercept=110,lty=2) +
  geom_text(aes(y=1.10),data=data.frame(tl=88,year2=factor(lvls[2],levels=lvls)),
            label="11",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=95,year2=factor(lvls[4],levels=lvls)),
            label="9",size=5) +
  geom_text(aes(y=0.65),data=data.frame(tl=90,year2=factor(lvls[8],levels=lvls)),
            label="5",size=5)
dev.off()

