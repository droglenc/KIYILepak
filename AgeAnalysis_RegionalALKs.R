##############################################################
##                                                          ##
## AGE ANALYSIS USING REGIONAL AGE-LENGTH KEYS              ##
##                                                          ##
## 1. Load data                                             ##
## 2. Construct observed age-length keys                    ##
## 3. Visualize observed age-length keys                    ##
## 4. Construct smoothed age-length keys                    ##
## 5. Visualize smoothed age-length keys                    ##
## 6. Apply the age-length keys                             ##
## 7. Summarize assigned ages                               ##
## 8. Age frequency by region                               ##
##                                                          ##
##############################################################


# clear workspace and console
rm(list=ls()); cat("\014")
# Set random seed ... all randomization same on each run
set.seed(34783478)



##############################################################
## 1. Load data
##############################################################
source("DataInit.R")
# Get raw age data and restrict to fish >=140 mm (for sample
# size considerations)
kiyiAge <- filter(kiyiAge,tl>=140)
# Get raw LF 2014 data, restrict to fish >= 140 mm, and add an
# otoAge variable to record the new ages
kiyiLF14 %<>% mutate(otoAge=as.numeric(NA)) %>%
  filter(tl>=140) %>%
  as.data.frame()

##############################################################
## 2. Construct observed age-length keys                    ##
##############################################################
## Prepare data
# Separate data.frames for each region
west  <- filterD(kiyiAge,region=="West")
NoMich  <- filterD(kiyiAge,region=="NoMich")
NoOnt <- filterD(kiyiAge,region=="NoOnt")
SoOnt <- filterD(kiyiAge,region=="SoOnt")
EastMich  <- filterD(kiyiAge,region=="EastMich")

## Construct separate ALKs
west.aged  <- west  %>% filterD(!is.na(otoAge)) %>% as.data.frame()
NoMich.aged  <- NoMich  %>% filterD(!is.na(otoAge)) %>% as.data.frame()
NoOnt.aged <- NoOnt %>% filterD(!is.na(otoAge)) %>% as.data.frame()
SoOnt.aged <- SoOnt %>% filterD(!is.na(otoAge)) %>% as.data.frame()
EastMich.aged  <- EastMich  %>% filterD(!is.na(otoAge)) %>% as.data.frame()

west.alk  <- prop.table(xtabs(~lcat10+otoAge,data=west.aged),margin=1)
NoMich.alk  <- prop.table(xtabs(~lcat10+otoAge,data=NoMich.aged),margin=1)
NoOnt.alk <- prop.table(xtabs(~lcat10+otoAge,data=NoOnt.aged),margin=1)
SoOnt.alk <- prop.table(xtabs(~lcat10+otoAge,data=SoOnt.aged),margin=1)
# assumed that 140 and 150 bins were the same as 160 bin for
# applying the ALK below
SoOnt.alk <- rbind(SoOnt.alk[c(1,1),],SoOnt.alk)
rownames(SoOnt.alk)[1:2] <- c(140,150)
EastMich.alk  <- prop.table(xtabs(~lcat10+otoAge,data=EastMich.aged),margin=1)



##############################################################
## 3. Visualize observed age-length keys                    ##
##############################################################
alkPlot(west.alk)
alkPlot(NoMich.alk)
alkPlot(NoOnt.alk)
alkPlot(SoOnt.alk)
alkPlot(EastMich.alk)



##############################################################
## 4. Construct smoothed age-length keys                    ##
##      Ultimately did not use ... too much smoothing for   ##
##      such sporadic year-classes.                         ##
##############################################################
## Construct separate ALKs
lens <- seq(140,290,10)
west.alks  <- predict(multinom(otoAge~lcat10,data=west.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
NoMich.alks  <- predict(multinom(otoAge~lcat10,data=NoMich.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
NoOnt.alks  <- predict(multinom(otoAge~lcat10,data=NoOnt.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
SoOnt.alks  <- predict(multinom(otoAge~lcat10,data=SoOnt.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
EastMich.alks  <- predict(multinom(otoAge~lcat10,data=EastMich.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
rownames(west.alks) <- rownames(NoMich.alks) <- rownames(NoOnt.alks) <- rownames(SoOnt.alks) <- rownames(EastMich.alks) <- lens



##############################################################
## 5. Visualize smoothed age-length keys                    ##
##############################################################
alkPlot(west.alks)
alkPlot(NoMich.alks)
alkPlot(NoOnt.alks)
alkPlot(SoOnt.alks)
alkPlot(EastMich.alks)



##############################################################
## 6. Apply the age-length keys                             ##
##############################################################
# sample size that ALK was applied to
nrow(kiyiLF14)
# get the unaged fish from the length frequency data.frame
west.unaged   <- filterD(kiyiLF14,region=="West")
NoMich.unaged   <- filterD(kiyiLF14,region=="NoMich")
NoOnt.unaged  <- filterD(kiyiLF14,region=="NoOnt")
SoOnt.unaged  <- filterD(kiyiLF14,region=="SoOnt")
EastMich.unaged   <- filterD(kiyiLF14,region=="EastMich")
# apply the ALKS
west.unaged.mod  <- alkIndivAge(west.alk,otoAge~tl,data=west.unaged)
NoMich.unaged.mod  <- alkIndivAge(NoMich.alk,otoAge~tl,data=NoMich.unaged)
NoOnt.unaged.mod <- alkIndivAge(NoOnt.alk,otoAge~tl,data=NoOnt.unaged)
SoOnt.unaged.mod <- alkIndivAge(SoOnt.alk,otoAge~tl,data=SoOnt.unaged)
EastMich.unaged.mod  <- alkIndivAge(EastMich.alk,otoAge~tl,data=EastMich.unaged)

## Put all of the data.frames together to make one with all aged fish
kiyiAge.fnl <- rbind(west.unaged.mod,NoMich.unaged.mod,NoOnt.unaged.mod,
                     SoOnt.unaged.mod,EastMich.unaged.mod)
any(is.na(kiyiAge.fnl$otoAge))     # confirm FALSE



##############################################################
## 4. Age frequency by region                               ##
##############################################################
# Age frequency tables by region (sexes pooled)
#   with corresponding chi-square tests
( kiyi.age1 <- xtabs(~region+otoAge,data=kiyiAge.fnl) )
# Made ages <6,6,7,8,9,10,11,>11 in groups
kiyi.age2 <- cbind(rowSums(kiyi.age1[,1:2]),kiyi.age1[,3:8],rowSums(kiyi.age1[,9:13]))
colnames(kiyi.age2)[c(1,ncol(kiyi.age2))] <- c("<6",">11")
# Made ages <6,6,7,8,9,10,11,>11 in groups
( OA.chi <- chisq.test(kiyi.age2) )
round(prop.table(kiyi.age2,margin=1)*100,0)


# PRESENTATION-QUALITY GRAPHICS
tmp <- kiyiAge.fnl %>%
  group_by(regL) %>%
  summarize(n=n())

tmpAge <- kiyiAge.fnl %>%
  mutate(regL2=factor(mapvalues(regL,levels(regL),
                                paste0(tmp$regL," (n=",tmp$n,")"))))


png("manuscript/Figs/FigureX_AgeFreq2.PNG",width=6.5,height=6.5,
    units="in",pointsize=24,family="sans",res=600)
ggplot(data=tmpAge,aes(x=otoAge)) +
  theme_kiyi() +
  scale_x_continuous(expand=c(0.02,0),limits=c(3.5,14.5),breaks=4:20) +
  scale_y_continuous(expand=c(0.05,0)) +
  geom_bar(color="black",fill="gray50") +
  facet_wrap(~regL2,ncol=1,scales="free_y") +
  labs(x="Consensus Otolith Age",y="Frequency")
dev.off()
