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
                 ref.lab="Ager 1",nref.lab="Ager 2")
summary(ab_OO,what="bias")
summary(ab_OO,what="symmetry")
ap_OO <- agePrecision(otoAge_TAL~otoAge_DHO,data=bio_OO)
summary(ap_OO,what="precision")
summary(ap_OO,what="absolute difference")
summary(ap_OO,what="absolute difference",trunc.diff=2)

## Otolith-scales comparisons (only TAL ages)
bio_OS <- bio[complete.cases(bio[,c("scaleAge","otoAge_TAL")]),]
ab_OS <- ageBias(scaleAge~otoAge_TAL,data=bio_OS,
                 ref.lab="Otolith Age",nref.lab="Scale Age")
ab_OS_bsum <- summary(ab_OS,what="bias")
ab_OS_bsym <- summary(ab_OS,what="symmetry")


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



################################################################################
## PLOTS
##
## This is needed for EPS plots that require the Arial font
#Sys.setenv(R_GSCMD="c:/apps/gs/gs9.20/bin/gswin32c.exe")
#library(extrafont)
#font_import() # may be asked to say "y"
#loadfonts(device="postscript")
#postscript("results/figures/Figure2_OtoOtoComp.eps",width=4.5,height=4.5,pointsize=14,family="Arial",horizontal=FALSE,paper="special")
#dev.off()
#embedFonts("results/figures/Figure2_OtoOtoComp.eps",outfile="results/figures/Figure2.eps",options="-dEPSCrop")
# I could not get cropping to work properly

## Figure 2 ... Otolith age comparison
#jpeg("results/figures/Figure2_OtoOtoComp.eps",width=4.5,height=4.5,units="in",pointsize=14,family="sans",quality=100,res=144)
pdf("results/figures/Figure2.pdf",width=6,height=6,family="Arial",pointsize=14)
par(mar=c(3,3,0.5,0.5),mgp=c(1.9,0.5,0),tcl=-0.2,las=1)
plot(ab_OO,show.n=TRUE,nYpos=0.025,cex.n=0.9,lwd.CI=2,col.CIsig="black",yaxt="n",
     lwd.agree=1,xlim=c(3,20),ylim=c(-3.4,2),difference=TRUE,
     show.pts=TRUE,transparency=1/5,ylab="Second Reader",xlab="First Reader")
axis(2,seq(-3,2,1))
axis(1,c(11,13,15,17,19))
dev.off()
embedFonts("results/figures/Figure2.pdf",outfile="results/figures/Figure2.pdf")

## Figure 3 ... Scale-Otolith age-bias plot
#jpeg("results/figures/Figure3_ScaleOtoComp.JPEG",width=4.5,height=4.5,units="in",pointsize=14,family="sans",quality=100,res=144)
pdf("results/figures/Figure3.pdf",width=6,height=6,family="Arial",pointsize=14)
par(mar=c(3,3,0.5,0.5),mgp=c(1.9,0.5,0),tcl=-0.2,las=1)
plot(ab_OS,show.n=TRUE,nYpos=0.025,cex.n=0.9,lwd.CI=2,col.CIsig="black",
     lwd.agree=1,xlim=c(3,16),ylim=c(-10.5,0),difference=TRUE,yaxt="n",
     show.pts=TRUE,transparency=1/5)
axis(2,seq(-10,0,1))
axis(1,seq(4,16,2))
axis(1,c(11,13,15))
dev.off()
embedFonts("results/figures/Figure3.pdf",outfile="results/figures/Figure3.pdf")

## Figure 4 ... Age frequency
#jpeg("results/figures/Figure4_OtoAgeFreq.JPEG",width=4.5,height=4.5,units="in",pointsize=14,family="sans",quality=100,res=144)
pdf("results/figures/Figure4.pdf",width=6,height=6,family="Arial",pointsize=14)
par(mar=c(3,3,0.5,0.5),mgp=c(1.9,0.5,0),tcl=-0.2,las=1)
hist(~otoAge,data=lf14_ages,xlab="Age (years)",w=1,
     xaxt="n",yaxt="n",ylim=c(0,300),col="gray70")
axis(1,at=seq(4.5,20.5,1),labels=NA)
axis(1,at=seq(5.5,20.5,5),labels=seq(5,20,5))
axis(2,at=seq(0,300,50),labels=NA)
axis(2,at=seq(0,300,100),labels=seq(0,300,100))
dev.off()
embedFonts("results/figures/Figure4.pdf",outfile="results/figures/Figure4.pdf")

## Figure 5 ... Length Frequency Progression
lf <- read.csv("data/clean/lenfreq_all.csv") %>%
  mutate(year=factor(year)) %>%
  filterD(year %in% 2003:2014)
tmp <- lf %>% group_by(year) %>% summarize(n=n())
lf %<>% mutate(year2=factor(mapvalues(year,levels(year),
                                      paste0(tmp$year," (n=",tmp$n,")"))))
lvls <- levels(lf$year2)

#jpeg("results/figures/Figure5_LFProgression.JPEG",width=6.5,height=9,units="in",pointsize=24,family="sans",quality=100,res=144)
pdf("results/figures/Figure5.pdf",width=6,height=9,family="Arial",pointsize=24)
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
embedFonts("results/figures/Figure5.pdf",outfile="results/figures/Figure5.pdf")

