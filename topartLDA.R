# Does Latent Dirichlet Allocation (LDA) to discover topics across articles in the set
# Christina K. Pikas @cpikas

# go through a list of files saved in regular WoS format in a directory
#easier would be to just start with tab del straight from WoS

# pull out the ti, ab, accession, year

#use  text files with  this format:
#header
# PT J
# NR 177
# UT WOS:000347606200010
# ER


#set working directory
setwd("~/apltopart15")

#load up the libraries we'll need
library("bibliometrix", lib.loc="~/R/win-library/3.4")
library("plyr", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("tm", lib.loc="~/R/win-library/3.4")
library("topicmodels", lib.loc="~/R/win-library/3.4")
library("Rmpfr", lib.loc="~/R/win-library/3.4")



#get a chr vector that lists all the export files that are in the data subdirectory
filenames <- list.files("80-2015recs", full.names=TRUE)


#incorporates bibliometrix method
getWoSdf<-function(filename){
  holdrecs<-readLines(filename)
  recsdf<-isi2df(holdrecs)
  return(recsdf)
}

WoSall<- ldply(filenames, getWoSdf)

#save out just in case
write.csv(WoSall, file="apltopart80-15.csv")

###LDA

#first check to see if enough have abstracts so I can use that field:
sum(is.na(WoSall$AB))
#about 25%  1716
sum(is.na(WoSall$TI))
#all have titles 0
sum(is.na(WoSall$ID))
#yeah, not so good coverage here - 1/3 missing 2411
sum(is.na(WoSall$DE))
#more than half missing 4675

hist(WoSall$PY)

#can't do with just the titles, so only using records with abstract. earliest with ab is 1977, latest w/out 2014
#mean py with abs 2014, mean py overall 1999, mean py w/out 1985

WoS4lda<-subset(WoSall,is.na(AB)==FALSE,select = c(UT,PY,AB))

WoS4lda<-subset(WoS4lda,PY>1979)

#n=4948

corp<-Corpus(VectorSource(WoS4lda$AB))

#start preprocessing
#Transform to lower case
corp <-tm_map(corp,content_transformer(tolower))
writeLines(as.character(corp[[2]]))


#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
corp <- tm_map(corp, toSpace, "-")
corp <- tm_map(corp, toSpace, "'")
corp <- tm_map(corp, toSpace, "'")
corp <- tm_map(corp, toSpace, ",")

corp <- tm_map(corp, toSpace, '"' )

#remove punctuation
corp <- tm_map(corp, removePunctuation)
#Strip digits
corp <- tm_map(corp, removeNumbers)
#remove stopwords
corp <- tm_map(corp, removeWords, stopwords("english"))

#check
writeLines(as.character(corp[[2]]))
#Stem document
corp <- tm_map(corp,stemDocument)

#define and eliminate all custom stopwords
#all the stupid copyright statements... sigh.
myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","'ve ",
                 "'re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot", "ieee","method","present",
                 "springer","elsevier","acm","lavoisier","trans tech publications","spie","john wiley", "sons","authors","data")
corp <- tm_map(corp, removeWords, myStopwords)
#remove whitespace
corp <- tm_map(corp, stripWhitespace)

#inspect a document as a check
writeLines(as.character(corp[[2]]))

#Create document-term matrix


dtm <- DocumentTermMatrix(corp, control= list(bounds = list(global = c(10,Inf)),weighting = weightTf ))
#note - can't do Tfidf see: https://stackoverflow.com/questions/14697218/how-to-check-frequency-weighting-in-a-term-document-matrix-in-topicmodels?rq=1


#add rownames
#
rownames(dtm) <- WoS4lda$UT
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq_tf_WoS4lda.csv")

#Set parameters for Gibbs sampling

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

################

# Determine the best number of topics
# omit sample step if this set is not terribly big
# if full dataset is too big, repeat over random samples
# instructions here: http://stackoverflow.com/questions/8273313/random-rows-in-dataframe-in-r


dtm.s<-dtm[sample(nrow(dtm), 1000), ]


# using this method to cross validate http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092

harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}


# generate numerous topic models with different numbers of topics
keep<-20

# sequ is number of topics start, end, number of topics and skip by
sequ <- seq(85,100,5) 

# ifnot using sample, because not too big, dtm.s becomes dtm
fitted_many <- lapply(sequ, function(k) LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])


# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect for the high point, repeat if it's not clear where
plot(sequ, hm_many, type = "l")



# compute optimum number of topics
sequ[which.max(hm_many)]

# k=95 maximizes
########

#Do the LDA
#Number of topics found in previous step

k <- 95

#Run LDA using Gibbs sampling

ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))


#write out results

#docs to topics

ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics-WoS80-15.csv"))



#top terms in each topic

ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))



#probabilities associated with each topic assignment

topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))





###################
# The topics are just a series of words which you then need to name
# This visualization tool may help

library("LDAvis", lib.loc="~/R/win-library/3.4")


#visualize results
#note - CRAN version does not offer ability to chose whether or not to reorder topics
# so it does reorder... and your topic numbers will not be the same as output from previous step
#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param corpus Corpus object used to create the document term
#' matrix for the \code{LDA} model. This should have been create with
#' the tm package's \code{Corpus} function.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's 
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export
#' 

# note: the function is changed from the web because inspect() only gives 10 topics selected randomly

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  # Find required quantitiesc
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- as.matrix(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}



ldaOut.json<-topicmodels_json_ldavis(ldaOut,corp,dtm)

write(ldaOut.json, "ldaOut.json")


#puts on screen or in RStudio viewer
serVis(ldaOut.json)

#note options to put on GitHub gist or particular directory
serVis(ldaOut.json, out.dir = "vis",open.browser = TRUE)



##########################



