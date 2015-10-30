##############################################################
##  LENGTH FREQUENCY ANALYSIS                               ##
##                                                          ##
##  1. Examine Length Frequency by Region                   ##
##  2. Length-Frequency for 2014                            ##
##  3. Length Frequency for 2001-2014                       ##
##                                                          ##
##############################################################

# clear workspace and console
rm(list=ls()); cat("\014")
# load DataInit script ... provide kiyiLF and kiyiLF14
source("DataInit.R")
# Set random seed ... bootstraps will be the same each time
set.seed(34783475)



##############################################################
##  1. Examine Length Frequency by Region                   ##
##                                                          ##
##  NoOnt region seems to be significant different than all ##
##    other regions except for the Western Arm.  All other  ##
##    regions did not differ significantly.                 ##
##    NoOnt region fish seem to be clustered more at the    ##
##    intermediate sizes and "large" fish are not present.  ##
##                                                          ##
##############################################################
# Split by region for comparisons below
westLF  <- filterD(kiyiLF14,region=="West") %>% as.data.frame()
NoMichLF  <- filterD(kiyiLF14,region=="NoMich") %>% as.data.frame()
NoOntLF <- filterD(kiyiLF14,region=="NoOnt") %>% as.data.frame()
SoOntLF <- filterD(kiyiLF14,region=="SoOnt") %>% as.data.frame()
EastMichLF  <- filterD(kiyiLF14,region=="EastMich") %>% as.data.frame()

# pairwise bootstrapped Kolmogorov-Smirnov tests
nboots <- 2500
ks.pvals <- c(ks.boot(westLF$tl,NoMichLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(westLF$tl,NoOntLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(westLF$tl,SoOntLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(westLF$tl,EastMichLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(NoMichLF$tl,NoOntLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(NoMichLF$tl,SoOntLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(NoMichLF$tl,EastMichLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(NoOntLF$tl,SoOntLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(NoOntLF$tl,EastMichLF$tl,nboots=nboots)$ks.boot.pvalue,
              ks.boot(SoOntLF$tl,EastMichLF$tl,nboots=nboots)$ks.boot.pvalue)
data.frame(comparisons=apply(combn(regL,2),MARGIN=2,FUN=paste,collapse=" v. "),
           adj.pval=p.adjust(ks.pvals))

# ECDF plots
ggplot(kiyiLF14,aes(tl,color=region)) +
  theme_kiyi() +
  geom_line(stat="ecdf",size=1.25) +
  labs(x="Total Length (mm)",y="Cumulative Density") +
  scale_color_manual(values=clrs)

# ANOVA for tl
Summarize(tl~region,data=kiyiLF14)
leveneTest(tl~region,data=kiyiLF14)
lm1 <- lm(tl~region,data=kiyiLF14)
anova(lm1)
mc1 <- glht(lm1,mcp(region="Tukey"))
summary(mc1)
siglets <- cld(mc1)$mcletters$Letters
# Kruskal-Wallis for tl
kruskal.test(tl~region,data=kiyiLF14)
dunnTest(tl~region,data=kiyiLF14)

## LF histograms for publication
# prepare some summaries
tmp <- kiyiLF14 %>%
  group_by(regL) %>%
  summarize(n=n(),mean=mean(tl),sd=sd(tl)) %>%
  mutate(x=258,y=Inf,
         mnlbl=paste0("mean=",formatC(mean,format="f",digits=0)," ",siglets),
         sdlbl=paste0("sd=",formatC(sd,format="f",digits=1)))

tmpLF14 <- kiyiLF14 %>%
  mutate(regL2=factor(mapvalues(regL,levels(regL),
                                 paste0(tmp$regL," (n=",tmp$n,")"))))
tmp %<>% mutate(regL2=factor(mapvalues(regL,levels(regL),levels(tmpLF14$regL2))))

# make the plot
png("manuscript/Figs/FigureX_LenFreq14Reg.PNG",width=6.5,height=6.5,
    units="in",pointsize=24,family="sans",res=600)
ggplot(tmpLF14,aes(x=tl)) +
  theme_mhist() +
  scale_x_continuous(expand=c(0.02,0),limits=c(140,280),breaks=seq(0,275,25)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  facet_wrap(~regL2,nrow=1,dir="v") +
  labs(x="Total Length (mm)",y="Relative Frequency") +
  geom_text(aes(x,y,label=mnlbl),data=tmp,vjust=1.5,hjust=0) +
  geom_text(aes(x,y,label=sdlbl),data=tmp,vjust=3.0,hjust=0)
dev.off()


##############################################################
##  2. Length-Frequency for 2014                            ##
##############################################################
#png("manuscript/Figs/FigureX_LF2014.PNG",width=4.5,height=3,units="in",pointsize=24,family="sans",res=600)
ggplot(kiyiLF14,aes(x=tl)) +
  theme_kiyi() +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,300),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  labs(x="Total Length (mm)",y="Relative Frequency")
#dev.off()



##############################################################
##  3. Length Frequency for 2001-2014                       ##
##############################################################
tmp <- kiyiLF %>%
  group_by(fyear) %>%
  summarize(n=n())

tmpLF <- kiyiLF %>%
  mutate(fyear2=factor(mapvalues(fyear,levels(fyear),
                                 paste0(tmp$fyear," (n=",tmp$n,")"))))

png("manuscript/Figs/FigureX_LFProgression.PNG",width=6.5,height=9,
    units="in",pointsize=24,family="sans",res=600)
lvls <- levels(tmpLF$fyear2)
ggplot(tmpLF,aes(x=tl)) +
  theme_mhist() +
  scale_x_continuous(expand=c(0.02,0),limits=c(40,305),breaks=seq(0,350,50)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1.2)) +
  geom_histogram(aes(y=..ncount..),binwidth=5,fill="gray50",color="black") +
  facet_wrap(~fyear2,nrow=2,dir="v") +
  labs(x="Total Length (mm)",y="Relative Frequency") +
  geom_text(aes(y=0.9),data=data.frame(tl=110,fyear2=factor(lvls[4],levels=lvls)),
            label="11",size=5) +
  geom_text(aes(y=0.4),data=data.frame(tl=95,fyear2=factor(lvls[6],levels=lvls)),
            label="9",size=5) +
  geom_text(aes(y=0.65),data=data.frame(tl=90,fyear2=factor(lvls[10],levels=lvls)),
            label="5",size=5)
dev.off()
