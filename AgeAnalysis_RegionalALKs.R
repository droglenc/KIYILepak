##############################################################
##                                                          ##
## AGE ANALYSIS USEING REGIONAL AGE-LENGTH KEYS             ##
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

# Set random seed ... all randomization same on each run
set.seed(34783478)

##############################################################
## 1. Load data
##############################################################
# Get raw age data and restrict to fish >=140 mm (for sample
# size considerations)
source("DataInit.R")
tmp <- filter(kiyiAge,tl>=140)
# Get raw LF data, reduce to May-July 2014, remove some vars,
# also restrict to fish >- 140 mm, and add an otoAge variable
# to record the new ages
kiyiLF14 <- kiyiLF %>%
  filterD(Use==1,year==2014,mon %in% c("May","Jun","Jul"),
          tl>=140) %>%
  select(-c(OP_ID,YEAR,SERIAL,CRUISE,op_date,Use,UseNotes,year)) %>%
  mutate(otoAge=as.numeric(NA)) %>%
  as.data.frame()

##############################################################
## 2. Construct observed age-length keys                    ##
##############################################################
## Prepare data
# Separate data.frames for each region
west  <- filterD(tmp,region=="West")
east  <- filterD(tmp,region=="East")
north <- filterD(tmp,region=="North")
south <- filterD(tmp,region=="South")
isle  <- filterD(tmp,region=="Isle")

## Construct separate ALKs
west.aged  <- west  %>% filterD(!is.na(otoAge)) %>% as.data.frame()
east.aged  <- east  %>% filterD(!is.na(otoAge)) %>% as.data.frame()
north.aged <- north %>% filterD(!is.na(otoAge)) %>% as.data.frame()
south.aged <- south %>% filterD(!is.na(otoAge)) %>% as.data.frame()
isle.aged  <- isle  %>% filterD(!is.na(otoAge)) %>% as.data.frame()

west.alk  <- prop.table(xtabs(~lcat10+otoAge,data=west.aged),margin=1)
east.alk  <- prop.table(xtabs(~lcat10+otoAge,data=east.aged),margin=1)
north.alk <- prop.table(xtabs(~lcat10+otoAge,data=north.aged),margin=1)
south.alk <- prop.table(xtabs(~lcat10+otoAge,data=south.aged),margin=1)
# assumed that 140 and 150 bins were the same as 160 bin for
# applying the ALK below
south.alk <- rbind(south.alk[c(1,1),],south.alk)
rownames(south.alk)[1:2] <- c(140,150)
isle.alk  <- prop.table(xtabs(~lcat10+otoAge,data=isle.aged),margin=1)


##############################################################
## 3. Visualize observed age-length keys                    ##
##############################################################
alkPlot(west.alk)
alkPlot(isle.alk)
alkPlot(north.alk)
alkPlot(south.alk)
alkPlot(east.alk)

##############################################################
## 4. Construct smoothed age-length keys                    ##
##      Ultimately did not use ... too much smoothing for   ##
##      such sporadic year-classes.                         ##
##############################################################
## Construct separate ALKs
lens <- seq(140,290,10)
west.alks  <- predict(multinom(otoAge~lcat10,data=west.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
isle.alks  <- predict(multinom(otoAge~lcat10,data=isle.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
north.alks  <- predict(multinom(otoAge~lcat10,data=north.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
south.alks  <- predict(multinom(otoAge~lcat10,data=south.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
east.alks  <- predict(multinom(otoAge~lcat10,data=east.aged,maxit=500),
                      data.frame(lcat10=lens),type="probs")
rownames(west.alks) <- rownames(isle.alks) <- rownames(north.alks) <- rownames(south.alks) <- rownames(east.alks) <- lens

##############################################################
## 5. Visualize smoothed age-length keys                    ##
##############################################################
alkPlot(west.alks)
alkPlot(isle.alks)
alkPlot(north.alks)
alkPlot(south.alks)
alkPlot(east.alks)

##############################################################
## 6. Apply the age-length keys                             ##
##############################################################
# get the unaged fish from the length frequency data.frame
west.unaged   <- filterD(kiyiLF14,region=="West")
east.unaged   <- filterD(kiyiLF14,region=="East")
north.unaged  <- filterD(kiyiLF14,region=="North")
south.unaged  <- filterD(kiyiLF14,region=="South")
isle.unaged   <- filterD(kiyiLF14,region=="Isle")
# apply the ALKS
west.unaged.mod  <- alkIndivAge(west.alk,otoAge~tl,data=west.unaged)
east.unaged.mod  <- alkIndivAge(east.alk,otoAge~tl,data=east.unaged)
north.unaged.mod <- alkIndivAge(north.alk,otoAge~tl,data=north.unaged)
south.unaged.mod <- alkIndivAge(south.alk,otoAge~tl,data=south.unaged)
isle.unaged.mod  <- alkIndivAge(isle.alk,otoAge~tl,data=isle.unaged)

## Put all of the data.frames together to make one with all aged fish
kiyiAge.fnl <- rbind(west.unaged.mod,east.unaged.mod,north.unaged.mod,
                     south.unaged.mod,isle.unaged.mod)
any(is.na(kiyiAge.fnl$otoAge))     # confirm FALSE



##############################################################
## 3. Summarize assigned ages                               ##
##############################################################
# Consensus ages histogram
hist(~otoAge,data=kiyiAge.fnl,breaks=2:20,xlab="Age",col="gray50")
# Summary of Consensus Ages
#(fnl.summary <- xtabs(~lcat10+otoAge,data=kiyiAge.fnl))



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

png("manuscript/Figs/FigureX_AgeFreq.PNG",width=6.5,height=6.5,units="in",pointsize=24,family="sans",res=600)
ggplot(data=kiyiAge.fnl,aes(x=otoAge)) +
  theme_bw() + theme_hist +
  scale_x_continuous(expand=c(0.02,0),limits=c(4,14),breaks=4:20) +
  scale_y_continuous(expand=c(0.05,0)) +
  geom_bar(color="black",fill="gray50") +
  facet_wrap(~region,ncol=1,scales="free_y") +
  labs(x="Consensus Otolith Age",y="Frequency")
dev.off()
