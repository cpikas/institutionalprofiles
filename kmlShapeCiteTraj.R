#script for citation trajectory clustering using kmlShape
#Genolini, C., Ecochard,R., Benghezal,M. Driss, R., Andrieu, S., & Subtil, F. (2016).
#kmlShape: An efficient method to cluster longitudinal data (time-series) according 
#to their shapes. PLoS One, 11, e0150738. doi:10.1371/journal.pone.0150738


library("kmlShape", lib.loc="~/R/win-library/3.4")
library(readr)
APL80_05citesyear <- read_csv("~/apltopart15/APL80-05citesyear.csv")
View(APL80_05citesyear)

set.seed(2222)

#only keep columns needed

APL80_05<-data.frame(TI=APL80_05citesyear$Title,PY1=APL80_05citesyear$`PY+1`,PY2=APL80_05citesyear$`PY+2`,
                     PY3=APL80_05citesyear$`PY+3`,PY4=APL80_05citesyear$`PY+4`,PY5=APL80_05citesyear$`PY+5`,
                     PY6=APL80_05citesyear$`PY+6`,PY7=APL80_05citesyear$`PY+7`,PY8=APL80_05citesyear$`PY+8`,
                     PY9=APL80_05citesyear$`PY+9`,PY10=APL80_05citesyear$`PY+10`)

myClds <-cldsWide(APL80_05)

#reduce data size - do by Id - this does a preliminary standard k-means cluster

reduceTraj(myClds,nbSenators = 10)

kmlShape(myClds,5)
plotMeans(myClds)

title(main="Citation Trajectories for Articles Published 1980-2005",
      sub="Years Post Publication",ylab="Citations",outer = true)
