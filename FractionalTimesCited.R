#Calculates Fractional Times Cited
#Christina Pikas



#this imports files saved in tab delimited format - for each article in the set of interest
#there's one or more files of up to 500 records that cite the original article
#we'll pull off the number off references for each, and tally up.

#watch file encoding and BOM characters
# turns out encoding might be better than fileEncoding see: http://stackoverflow.com/questions/18789330/r-on-windows-character-encoding-hell


#set working directory

setwd("~/apltopart15")

library("bibliometrix", lib.loc="~/R/win-library/3.4")
library("plyr", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")


#make a vector that indicates which columns you want to import

mycolClasses<-c("character", rep("NULL",29),"character",rep("NULL",30))


findTCf<-function(savedfile){
  text <- read.delim(savedfile, header=FALSE, sep="\t",colClasses=mycolClasses, quote="", na.strings="", encoding="UTF-8-BOM")
  tcvec<- as.numeric(text$V31[2:(length(text$V31))])
  tcvec[tcvec==0]<-NA
  tcvec<-na.exclude(tcvec)
  tcvecf<-1/tcvec
  TCf<-round(sum(tcvecf),2)
  return(TCf)
}
#first 5 articles in my set had >500 citations so were double files
#reading a list of file names to preserve order

#this is the first 5 that were two files per article
dblfilenames <- read.table("~/apltopart15/filelistdbl.txt", quote="", stringsAsFactors=FALSE)
#this is up to 899
filenames <- read.table("~/apltopart15/filelist.txt", quote="", stringsAsFactors=FALSE)

#this finishes up to 1000
filenames2 <- read.table("~/apltopart15/filelist2.txt", quote="", stringsAsFactors=FALSE)



filenames<-filenames$V1
filenames2<-filenames2$V1
dblfilenames<-dblfilenames$V1

TCfracv<-lapply(filenames,findTCf)
TCfracv2<-lapply(filenames2,findTCf)
TCfracvdbl<-lapply(dblfilenames,findTCf)

#unlist to get a numeric vector
TCfracv<-unlist(TCfracv)
TCfracv2<-unlist(TCfracv2)
TCfracvdbl<-unlist(TCfracvdbl)

TCf<-c(TCfracvdbl,TCfracv,TCfracv2)
TCf.df<-data.frame(filenames=c(dblfilenames,filenames,filenames2),TCf)

#save it out
write.csv(TCf.df, file="APLTCfracv09021017.csv")


#########################
#Use bibliometrix to get the articles from the saved files
#note these were saved in WoS plain text

#get a chr vector that lists all the export files that are in the data subdirectory
filenamesAPLart <- list.files("aplartdwnld", full.names=TRUE)


#incorporates bibliometrix method
getWoSdf<-function(filename){
  holdrecs<-readLines(filename)
  recsdf<-isi2df(holdrecs)
  return(recsdf)
}

APLart<- ldply(filenamesAPLart, getWoSdf)

#save out just in case
write.csv(APLart, file="APLarticles.csv")

#subset to just top 1000 - that's all we have the TCf for 
APLart1000<-APLart[1:1000,]



##########################
#bins for years
# bins  Years
# 1	2011-2015
# 2	2006-2010
# 3	1996-2005
# 4	1986-1995
# 5	-1985

#before combining, we have to deal with all of the ones that required more than one file.
#there's probably a tidy way to do this, but the following works

head(TCf.df)
doubles<-data.frame(filenames=c("rawdata/savedrecs(31-32).txt","rawdata/savedrecs(33-34).txt",
                                "rawdata/savedrecs(35-36).txt","rawdata/savedrecs(37-38).txt",
                                "rawdata/savedrecs(40-39).txt"),
                    TCf=c(TCf.df$TCf[1]+TCf.df$TCf[2],TCf.df$TCf[3]+TCf.df$TCf[4],
                          TCf.df$TCf[5]+TCf.df$TCf[6],TCf.df$TCf[7]+TCf.df$TCf[8],
                          TCf.df$TCf[9]+TCf.df$TCf[10]))
TCf.df.doubles<-rbind(doubles,TCf.df[11:1005,])

APLsum<-cbind(APLart1000,TCf.df.doubles)

#look at overall rank
APLsum$TC<-as.numeric(APLsum$TC)
APLsum$Rank.Overall<-rank(-APLsum$TC)

APLsum$Rank.Overall.F<-rank(-APLsum$TCf)
#are they correlated 
Corr.Overall<-cor.test(APLsum$Rank.Overall,APLsum$Rank.Overall.F, alternative = "greater",method="spearman")
#yes 82%

#handle time by binning by years
APLsum$bin <- cut(APLsum$PY, breaks=c(0,1986,1996,2006,2011,2016))

APLsum %>% count(bin)


#rank by bin
#use this example https://stackoverflow.com/a/34968528/2464155


APLsum.bin.ranked<-APLsum %>% arrange(PY,TC) %>%
  group_by(bin) %>%
  mutate(binrank = rank(-TC, ties.method = 'first')) %>%
  mutate(binrank.f = rank(-TCf,ties.method = 'first'))
  
View(APLsum.bin.ranked %>% filter(binrank.f<=5))
write.csv(APLsum.bin.ranked %>% filter(binrank<=5),"APL80-2005binranked09022017.csv")
write.csv(APLsum.bin.ranked %>% filter(binrank.f<=5),"APL80-2005binrankedf09022017.csv")

