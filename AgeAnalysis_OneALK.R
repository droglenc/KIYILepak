##############################################################
##                                                          ##
## 1. Load data                                             ##
## 2. Construct and apply age-length key                    ##
## 3. Summarize assigned ages                               ##
## 4. Age frequency by region                               ##
##                                                          ##
##############################################################

# Set random seed ... all randomization same on each run
set.seed(34783478)

##############################################################
## 1. Load data
##############################################################
# Get raw data
source("DataInit.R")
# Get raw age data and restrict to fish >=140 mm (for sample
# size considerations)
kiyiAge <- filter(kiyiAge,tl>=140)
# Get raw LF 2014 data, restrict to fish >= 140 mm, and add an
# otoAge variable to record the new ages
kiyiLF14 <- kiyiLF14 %>%
  filterD(tl>=140) %>%
  select(-c(OP_ID,YEAR,SERIAL,CRUISE,op_date,year)) %>%
  mutate(otoAge=as.numeric(NA)) %>%
  as.data.frame()

##############################################################
## 2. Construct the age-length key                          ##
##############################################################
## Make one ALK for all fish (see ALKComparison.R)
# Isolate aged fish
kiyiAge.aged <- kiyiAge %>% filterD(!is.na(otoAge))
all(!is.na(kiyiAge.aged$otoAge))  # confirm TRUE
# make the key
alk.freq <- xtabs(~lcat10+otoAge,data=kiyiAge.aged)
alk <- prop.table(alk.freq,margin=1)
round(alk,3)

##############################################################
## 2. Apply the age-length key                              ##
##############################################################
# Isolate unaged fish
all(is.na(kiyiLF14$otoAge))  # confirm TRUE
# Apply the ALK
kiyiLF14 <- alkIndivAge(alk,otoAge~tl,data=kiyiLF14)
any(is.na(kiyiLF14$otoAge))     # confirm FALSE



##############################################################
## 3. Summarize assigned ages                               ##
##############################################################
#-------------------------------------------------------------
# Age frequency tables by region (sexes pooled)
#   with corresponding chi-square tests
#-------------------------------------------------------------
( kiyi.age1 <- xtabs(~region+otoAge,data=kiyiLF14) )
# Made ages <6,6,7,8,9,10,11,>11 in groups
kiyi.age2 <- cbind(rowSums(kiyi.age1[,1:2]),
                   kiyi.age1[,3:8],
                   rowSums(kiyi.age1[,9:14]))
colnames(kiyi.age2)[c(1,ncol(kiyi.age2))] <- c("<6",">11")
# Made ages <6,6,7,8,9,10,11,>11 in groups
( OA.chi <- chisq.test(kiyi.age2) )
round(prop.table(kiyi.age2,margin=1)*100,0)


# PRESENTATION-QUALITY GRAPHICS

png("manuscript/Figs/FigureX_AgeFreq1.PNG",width=6.5,height=6.5,units="in",pointsize=24,family="sans",res=600)
ggplot(data=kiyiLF14,aes(x=otoAge)) +
  theme_kiyi() +
  scale_x_continuous(expand=c(0.02,0),limits=c(3.5,14.5),breaks=4:20) +
  scale_y_continuous(expand=c(0.05,0)) +
  geom_bar(color="black",fill="gray50") +
  facet_wrap(~region,ncol=1,scales="free_y") +
  labs(x="Consensus Otolith Age",y="Frequency")
dev.off()
