##############################################################
##                                                          ##
## 1. Prepare data for comparisons                          ##
## 2. ALK comparisons by sex within regions                 ##
## 3. ALK comparisons by regions (sexes pooled)             ##
## 4. Table of all ALK comparison results                   ##
##                                                          ##
##############################################################

##############################################################
## 1. Prepare data for comparisons                          ##
##############################################################
# Get raw data and load packages
source("DataInit.R")
# Temporarily remove juvenile fish
tmp <- filterD(kiyial,sex!="juvenile")

# Examine age frequency by region
xtabs(~otoAge+region,data=tmp)
# Examine number by region
xtabs(~region,data=tmp)
# Remove age-13 and older fish for the statistical comparisons
#   as they are poorly represented in the sample
tmp <- filterD(tmp,otoAge<13)

# Create region-specific data
West  <- filterD(tmp,region=="West")
East  <- filterD(tmp,region=="East")
North <- filterD(tmp,region=="North")
South <- filterD(tmp,region=="South")
Isle  <- filterD(tmp,region=="Isle")



##############################################################
## 2. ALK comparisons by sex within regions                 ##
##############################################################
# West Comparison
west1 <- multinom(otoAge~lcat10,data=West,maxit=500)
west2 <- multinom(otoAge~lcat10*sex,data=West,maxit=500)
tmpW <- anova(west1,west2)
# East Comparison
east1 <- multinom(otoAge~lcat10,data=East,maxit=500)
east2 <- multinom(otoAge~lcat10*sex,data=East,maxit=500)
tmpE <- anova(east1,east2)
# North Comparison
north1 <- multinom(otoAge~lcat10,data=North,maxit=500)
north2 <- multinom(otoAge~lcat10*sex,data=North,maxit=500)
tmpN <- anova(north1,north2)
# South Comparison
south1 <- multinom(otoAge~lcat10,data=South,maxit=500)
south2 <- multinom(otoAge~lcat10*sex,data=South,maxit=500)
tmpS <- anova(south1,south2)
# Isle Comparison
isle1 <- multinom(otoAge~lcat10,data=Isle,maxit=500)
isle2 <- multinom(otoAge~lcat10*sex,data=Isle,maxit=500)
tmpI <- anova(isle1,isle2)

# Sexes combined across all regions
mod1 <- multinom(otoAge~lcat10,data=tmp,maxit=500)
mod2 <- multinom(otoAge~lcat10*sex,data=tmp,maxit=500)
tmpSex <- anova(mod1,mod2)

# Visualize comparison by sex in one region
tmpM <- xtabs(~lcat10+otoAge,data=filter(East,sex=="male"))
tmpF <- xtabs(~lcat10+otoAge,data=filter(East,sex=="female"))
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
(tmpW <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="West")))
tmpE <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="East"))
tmpN <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="North"))
tmpS <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="South"))
tmpI <- xtabs(~lcat10+otoAge,data=filter(tmp,region=="Isle"))
alkPlot(tmpW,type="bubble",col=rgb(1,0,0,1/3),xlim=c(140,240),ylim=c(4,12))
alkPlot(tmpE,type="bubble",col=rgb(0,0,1,1/3),add=TRUE)
alkPlot(tmpN,type="bubble",col=rgb(0,1,0,1/3),add=TRUE)
alkPlot(tmpS,type="bubble",col=rgb(1,0,1,1/3),add=TRUE)
alkPlot(tmpI,type="bubble",col=rgb(1,1,0,1/3),add=TRUE)


##############################################################
## 4. Table of all ALK comparisons results                  ##
##############################################################
# Combine p-values
res <- data.frame(Comparison=c(paste("By sex within",regS),
                               "By sex, Pooled regions","By region, Pooled sexes"),
           df1   =c(tmpW$'   Df'[2],tmpE$'   Df'[2],tmpN$'   Df'[2],tmpS$'   Df'[2],
                    tmpI$'   Df'[2],tmpSex$'   Df'[2],tmpReg$'   Df'[2]),
           df2   =c(tmpW$'Resid. df'[2],tmpE$'Resid. df'[2],tmpN$'Resid. df'[2],
                    tmpS$'Resid. df'[2],tmpI$'Resid. df'[2],tmpSex$'Resid. df'[2],
                    tmpReg$'Resid. df'[2]),
           pvalue=c(tmpW$'Pr(Chi)'[2],tmpE$'Pr(Chi)'[2],tmpN$'Pr(Chi)'[2],
                    tmpS$'Pr(Chi)'[2],tmpI$'Pr(Chi)'[2],tmpSex$'Pr(Chi)'[2],tmpReg$'Pr(Chi)'[2]))
res


##############################################################
## It appears that one ALK will work for both sexes and all ##
##   regions combined.  No need to make separate ALKs       ##
##############################################################
