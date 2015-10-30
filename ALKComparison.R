##############################################################
##                                                          ##
## COMPARISONS OF AGE-LENGTH KEYS                           ##
##                                                          ##
## 1. Prepare data for comparisons                          ##
## 2. ALK comparisons by sex within regions                 ##
## 3. ALK comparisons by regions (sexes pooled)             ##
## 4. Table of all ALK comparison results                   ##
##                                                          ##
##############################################################

# clear workspace and console
rm(list=ls()); cat("\014")


##############################################################
## 1. Prepare data for comparisons                          ##
##############################################################
# Get raw data and load packages
source("DataInit.R")
# Remove juvenile fish (same as removing all fish <140 mm)
tmp <- filterD(kiyiAge,sex!="juvenile")

# Examine age frequency by region
xtabs(~otoAge,data=tmp)
# Examine age frequency by region
xtabs(~otoAge+region,data=tmp)
# Examine number by region
xtabs(~region,data=tmp)
# Remove age-13 and older fish for the statistical comparisons
#   as they are poorly represented in the sample
tmp <- filterD(tmp,otoAge<13)

# Create region-specific data
West  <- filterD(tmp,region=="West")
NoMich  <- filterD(tmp,region=="NoMich")
NoOnt <- filterD(tmp,region=="NoOnt")
SoOnt <- filterD(tmp,region=="SoOnt")
EastMich  <- filterD(tmp,region=="EastMich")



##############################################################
## 2. ALK comparisons by sex within regions                 ##
##############################################################
# West Comparison
west1 <- multinom(otoAge~lcat10,data=West,maxit=500)
west2 <- multinom(otoAge~lcat10*sex,data=West,maxit=500)
tmpW <- anova(west1,west2)
# EastMich Comparison
EastMich1 <- multinom(otoAge~lcat10,data=EastMich,maxit=500)
EastMich2 <- multinom(otoAge~lcat10*sex,data=EastMich,maxit=500)
tmpE <- anova(EastMich1,EastMich2)
# NoOnt Comparison
NoOnt1 <- multinom(otoAge~lcat10,data=NoOnt,maxit=500)
NoOnt2 <- multinom(otoAge~lcat10*sex,data=NoOnt,maxit=500)
tmpN <- anova(NoOnt1,NoOnt2)
# SoOnt Comparison
SoOnt1 <- multinom(otoAge~lcat10,data=SoOnt,maxit=500)
SoOnt2 <- multinom(otoAge~lcat10*sex,data=SoOnt,maxit=500)
tmpS <- anova(SoOnt1,SoOnt2)
# NoMich Comparison
NoMich1 <- multinom(otoAge~lcat10,data=NoMich,maxit=500)
NoMich2 <- multinom(otoAge~lcat10*sex,data=NoMich,maxit=500)
tmpNM <- anova(NoMich1,NoMich2)

# Sexes combined across all regions
mod1 <- multinom(otoAge~lcat10,data=tmp,maxit=500)
mod2 <- multinom(otoAge~lcat10*sex,data=tmp,maxit=500)
tmpSex <- anova(mod1,mod2)

# Visualize comparison by sex in one region
tmpM <- xtabs(~lcat10+otoAge,data=filter(EastMich,sex=="male"))
tmpF <- xtabs(~lcat10+otoAge,data=filter(EastMich,sex=="female"))
alkPlot(tmpF,type="bubble",col=rgb(1,0,0,1/3),ylim=c(4,12))
alkPlot(tmpM,type="bubble",col=rgb(0,0,1,1/3),add=TRUE)

# Visualize comparison by sex across all regions
tmpM <- xtabs(~lcat10+otoAge,data=filter(tmp,sex=="male"))
tmpF <- xtabs(~lcat10+otoAge,data=filter(tmp,sex=="female"))
alkPlot(tmpF,type="bubble",col=rgb(1,0,0,1/3),ylim=c(4,12))
alkPlot(tmpM,type="bubble",col=rgb(0,0,1,1/3),add=TRUE)


##############################################################
## 3. ALK comparisons by regions (sexes pooled)             ##
##############################################################
mod3 <- multinom(otoAge~lcat10,data=tmp,maxit=500)
mod4 <- multinom(otoAge~lcat10*region,data=tmp,maxit=500)
tmpReg <- anova(mod3,mod4)

# Visualize by region --- Not very useful
(tmpWdf <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="West")))
tmpEdf <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="EastMich"))
tmpNdf <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="NoOnt"))
tmpSdf <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="SoOnt"))
tmpNMdf <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="NoMich"))
alkPlot(tmpWdf,type="bubble",col=rgb(1,0,0,1/3),xlim=c(140,240),ylim=c(4,12))
alkPlot(tmpEdf,type="bubble",col=rgb(0,0,1,1/3),add=TRUE)
alkPlot(tmpNdf,type="bubble",col=rgb(0,1,0,1/3),add=TRUE)
alkPlot(tmpSdf,type="bubble",col=rgb(1,0,1,1/3),add=TRUE)
alkPlot(tmpNMdf,type="bubble",col=rgb(1,1,0,1/3),add=TRUE)


##############################################################
## 4. Table of all ALK comparisons results                  ##
##############################################################
# Combine p-values
res <- data.frame(Comparison=c(paste("By sex within",regS),
                               "By sex, Pooled regions","By region, Pooled sexes"),
           df1   =c(tmpW$'   Df'[2],tmpE$'   Df'[2],tmpN$'   Df'[2],tmpS$'   Df'[2],
                    tmpNM$'   Df'[2],tmpSex$'   Df'[2],tmpReg$'   Df'[2]),
           df2   =c(tmpW$'Resid. df'[2],tmpE$'Resid. df'[2],tmpN$'Resid. df'[2],
                    tmpS$'Resid. df'[2],tmpNM$'Resid. df'[2],tmpSex$'Resid. df'[2],
                    tmpReg$'Resid. df'[2]),
           pvalue=c(tmpW$'Pr(Chi)'[2],tmpE$'Pr(Chi)'[2],tmpN$'Pr(Chi)'[2],
                    tmpS$'Pr(Chi)'[2],tmpNM$'Pr(Chi)'[2],tmpSex$'Pr(Chi)'[2],
                    tmpReg$'Pr(Chi)'[2]))
res


##############################################################
## It appears that one ALK will work for both sexes and all ##
##   regions combined.  No need to make separate ALKs       ##
##############################################################
