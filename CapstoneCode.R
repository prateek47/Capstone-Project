# Capstone Code for LDA models.

library(tm)
library(topicmodels)
library(slam)

setwd("C:/UVa/own work/workspace/PNASjournals")

# assigning the directory of the files to an object
journals2014 <- getwd()
# checking the object
dir(journals2014)

# Creating a Corpus
Journal.corpus <- Corpus(DirSource(journals2014))
# Inspecting the Corpus
summary(Journal.corpus)
# The id of any document in the corpus
Journal.corpus[[3537]]$meta$id
# The content of a document in the corpus
Journal.corpus[[1]]$content

# I am using random 3000 documents from the 2014 corpus
s <- sample(1:3537, 2500)
Journal.corpus.samp <- Journal.corpus[s]

# Applying topic modelling processing steps to the corpus to get it up for analysis
j2014.corp <- tm_map(Journal.corpus.samp, content_transformer(tolower))
# removing special characters
removespec <- content_transformer(function(x, pattern){
  return(gsub(pattern, " ",x))
})
j2014.corp <- tm_map(j2014.corp, removespec, "[—†â‡’“ãî‰ï€ˆ™œ]")
j2014.corp <- tm_map(j2014.corp, removeNumbers)
j2014.corp <- tm_map(j2014.corp, removePunctuation)
j2014.corp <- tm_map(j2014.corp, removeWords, c(stopwords("english"), "fig"))
j2014.corp <- tm_map(j2014.corp, stripWhitespace)
#j2014.corp <- tm_map(j2014.corp, stemDocument)

# checking the contents
j2014.corp[[1]]$content

# creating a document term matrix
pnas.dtm <- DocumentTermMatrix(j2014.corp, control = list(wordLengths= c(4, 15), bounds= list(global = c(1, 1000))))
pnas.dtm
# control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
# This shows a 96% sparsity which needs to be reduced before trying our hand at Latent Dirichlet Allocation


# reducing the empty space to 95%
pnas.dtm2 <- removeSparseTerms(pnas.dtm, 0.95)
pnas.dtm2
# the sparsity is reduced to 87% and 2818 words, 2500 documents

# removing the common words for better understanding of words
#summary(col_sums(pnas.dtm3))

#pnas.dtm9 <- pnas.dtm3[ , col_sums(pnas.dtm3)<3756]
#pnas.dtm9

# more than 150 common words are removed

# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# Prediction corpus for 2015 journals

setwd("C:/UVa/own work/workspace/2015Journaltext")

# assigning the directory of the files to an object
journals2015 <- getwd()
# checking the object
dir(journals2015)

# Creating a Corpus
Journal.corpus2 <- Corpus(DirSource(journals2015))

# Applying topic modelling processing steps to the corpus to get it up for analysis
j2015.corp <- tm_map(Journal.corpus2, content_transformer(tolower))
# removing special characters
removespec <- content_transformer(function(x, pattern){
  return(gsub(pattern, " ",x))
})
j2015.corp <- tm_map(j2015.corp, removespec, "[—†â‡’“ãî‰ï€ˆ™œ]")
j2015.corp <- tm_map(j2015.corp, removeNumbers)
j2015.corp <- tm_map(j2015.corp, removePunctuation)
j2015.corp <- tm_map(j2015.corp, removeWords, c(stopwords("english"), "fig"))
j2015.corp <- tm_map(j2015.corp, stripWhitespace)
#j2015.corp <- tm_map(j2015.corp, stemDocument)

# checking the contents
j2015.corp[[1]]$content

# creating a document term matrix
pnas.dtm3 <- DocumentTermMatrix(j2015.corp, control = list(wordLengths = c(4, 15), bounds= list(global= c(1, 1118))) )
pnas.dtm3
removedoc <- row_sums(pnas.dtm3, na.rm = T) # using the slam library, due to it's speed and 
# reliability over the apply function
# removedoc <- apply(tweets.dtm, 1, sum)
pnas.dtm4 <- pnas.dtm3[removedoc >0, ]
pnas.dtm4

# removing the sparcity, keeping 95% empty spaces
pnas.dtm5 <- removeSparseTerms(pnas.dtm4, .95)
pnas.dtm5
# the sparcity is reduced to 85% and words 3074, 1754 documents

#------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
# testing corpus for 2014 journals

setwd("C:/UVa/own work/workspace/PNASjournals")

Journal.corpus.samp2 <- Journal.corpus[-s]

# Applying topic modelling processing steps to the corpus to get it up for analysis
j2014.corp2 <- tm_map(Journal.corpus.samp2, content_transformer(tolower))
# removing special characters
removespec <- content_transformer(function(x, pattern){
  return(gsub(pattern, " ",x))
})
j2014.corp2 <- tm_map(j2014.corp2, removespec, "[—†â‡’“ãî‰ï€ˆ™œ]")
j2014.corp2 <- tm_map(j2014.corp2, removeNumbers)
j2014.corp2 <- tm_map(j2014.corp2, removePunctuation)
j2014.corp2 <- tm_map(j2014.corp2, removeWords, c(stopwords("english"), "fig"))
j2014.corp2 <- tm_map(j2014.corp2, stripWhitespace)
#j2014.corp2 <- tm_map(j2014.corp2, stemDocument)

# checking the contents
j2014.corp2[[1]]$content

# creating a document term matrix
pnas.dtm6 <- DocumentTermMatrix(j2014.corp2, control = list(wordLengths= c(4, 15), bounds= list(global = c(1, 415))))
pnas.dtm6

# keeping empty spaces to 95% 
pnas.dtm7 <- removeSparseTerms(pnas.dtm6, .95)
pnas.dtm7
# the sparcity is reduced to 87% and words to 2854, 1037 documents

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------

# use ngrams for creating different sets for different types of predictions
#
#
#
#
#
#


#**********************************************************************************************
                              ########### Model 1 #############
#***********************************************************************************************

# Creating LDA model with VEM and aplha 50/k, I am using 3 topic which represent the
# 3 broad categories of Physical Sciences, Social Sciences, Biological Sciences and others
# the seed value is kept to maintain reproducibility.

# creating a model 
pnas.lda.vem <- LDA(pnas.dtm2, 3, method = 'VEM', control = list(seed= 300))

terms(pnas.lda.vem, 25)
# after inspection of words we can say that
# Topic1: Biological Sciences
# Topic2: Physical Sciences
# Topic3: Social Sciences
pnas.lda.vem@gamma[1:10, ]

# Testing model
t <- posterior(pnas.lda.vem, pnas.dtm7)
# Prediction Model
t1 <- posterior(pnas.lda.vem, pnas.dtm5)

##################################### Using 2014 Testing corpus ####################################


# transforming the dataframe of terms and topics and separate dataframe for each topic
term.topic <- data.frame(t(t$terms))
colnames(term.topic) <- c("Biological Sciences", "Physical Sciences", "Social Sciences")

terms.bio <- data.frame(term.topic$`Biological Sciences`)
rownames(terms.bio) <- rownames(term.topic)

terms.socialsc <- data.frame(term.topic$`Social Sciences`)
rownames(terms.socialsc) <- rownames(term.topic)

terms.phy <- data.frame(term.topic$`Physical Sciences`)
rownames(terms.phy) <- rownames(term.topic)

# I can find the word associations here 
#
#
#
#
#
#


# finding the most likely word in each topics.
term.topic$most.likely.topic <- colnames(term.topic)[apply(term.topic, 1, which.max)]

# creating the bag of words of each topic
words.in.bio <- rownames(term.topic)[term.topic$most.likely.topic == "Biological Sciences"]

words.in.phys <- rownames(term.topic)[term.topic$most.likely.topic == "Physical Sciences"]

words.in.social <- rownames(term.topic)[term.topic$most.likely.topic == "Social Sciences"]


#----------------------

docu.topic <- data.frame(t$topics)
colnames(docu.topic) <- c( "Biological Sciences", "Physical Sciences", "Social Sciences")

# finding the most likely topic for each document
docu.topic$most.likely.topic <- colnames(docu.topic)[apply(docu.topic, 1, which.max)]

# using most likely topic I can go back to intial database and find the topic name label and
# check for accuracy.

docu.in.bio <- rownames(docu.topic)[docu.topic$most.likely.topic == "Biological Sciences"]

docu.in.phys <- rownames(docu.topic)[docu.topic$most.likely.topic == "Physical Sciences"]

docu.in.social <- rownames(docu.topic)[docu.topic$most.likely.topic == "Social Sciences"]

# I can use only these documents per topic and perform further analysis to further 
# subcategorize it as per PNAS journals archives
#
#
#
#
#
#
#


##################################### Using 2015 Prediction corpus ####################################

# transforming the dataframe of terms and topics and separate dataframe for each topic
term.topic15 <- data.frame(t(t1$terms))
colnames(term.topic15) <- c("Biological Sciences", "Physical Sciences", "Social Sciences")

terms.bio15 <- data.frame(term.topic15$`Biological Sciences`)
rownames(terms.bio15) <- rownames(term.topic15)

terms.socialsc15 <- data.frame(term.topic15$`Social Sciences`)
rownames(terms.socialsc15) <- rownames(term.topic15)

terms.phy15 <- data.frame(term.topic15$`Physical Sciences`)
rownames(terms.phy15) <- rownames(term.topic15)

# I can find the word associations here 
#
#
#
#
#
#


# finding the most likely word in each topics.
term.topic15$most.likely.topic <- colnames(term.topic15)[apply(term.topic15, 1, which.max)]

# creating the bag of words of each topic
words.in.bio15 <- rownames(term.topic15)[term.topic15$most.likely.topic == "Biological Sciences"]

words.in.phys15 <- rownames(term.topic15)[term.topic15$most.likely.topic == "Physical Sciences"]

words.in.social15 <- rownames(term.topic15)[term.topic15$most.likely.topic == "Social Sciences"]


#----------------------

docu.topic15 <- data.frame(t1$topics)
colnames(docu.topic15) <- c( "Biological Sciences", "Physical Sciences", "Social Sciences")

# finding the most likely topic for each document
docu.topic15$most.likely.topic <- colnames(docu.topic15)[apply(docu.topic15, 1, which.max)]

# using most likely topic I can go back to intial database and find the topic name label and
# check for accuracy.

docu.in.bio15 <- rownames(docu.topic15)[docu.topic15$most.likely.topic == "Biological Sciences"]

docu.in.phys15 <- rownames(docu.topic15)[docu.topic15$most.likely.topic == "Physical Sciences"]

docu.in.social15 <- rownames(docu.topic15)[docu.topic15$most.likely.topic == "Social Sciences"]

# I can use only these documents per topic and perform further analysis to further 
# subcategorize it as per PNAS journals archives
#
#
#
#
#
#
#


#**********************************************************************************************
                                ########### Model 2 #############
#***********************************************************************************************

# Creating LDA model with Gibbs Sampler and aplha 50/k, I am using 3 topics which represent the
# three broad categories of Physical Sciences, Social Sciences, Biological Sciences
# and others
# the seed value is kept to maintain reproducibility.

pnas.lda.gibbs <- LDA(pnas.dtm2, 3, method = 'Gibbs', control = list(seed= 300))

terms(pnas.lda.gibbs, 25)
# after inspection of words we can say that
# Topic1: Physical Sciences 
# Topic2: Biological Sciences
# Topic3: Social Sciences
pnas.lda.gibbs@gamma[1:10, ]

# Testing model
t.gibbs <- posterior(pnas.lda.gibbs, pnas.dtm7)
# Prediction model
t1.gibbs <- posterior(pnas.lda.gibbs, pnas.dtm5)

##################################### Using 2014 Testing Corpus####################################

# transforming the dataframe of terms and topics and separate dataframe for each topic
term.topic2 <- data.frame(t(t.gibbs$terms))
colnames(term.topic2) <- c("Physical Sciences", "Biological Sciences", "Social Sciences" )

terms.bio.gibbs <- data.frame(term.topic2$`Biological Sciences`)
rownames(terms.bio.gibbs) <- rownames(term.topic2)

terms.socialsc.gibbs <- data.frame(term.topic2$`Social Sciences`)
rownames(terms.socialsc.gibbs) <- rownames(term.topic2)

terms.phy.gibbs <- data.frame(term.topic2$`Physical Sciences`)
rownames(terms.phy.gibbs) <- rownames(term.topic2)

# I can perform word cloud visualization using these data frames and even compare the words
# this is done in another file, "Wordcloud_corpfor caps"

# I can find the word associations here and can form better models
#
#
#
#
#
#



# finding the most likely word in each topics.
term.topic2$most.likely.topic <- colnames(term.topic2)[apply(term.topic2, 1, which.max)]

# creating the bag of words of each topic
words.bio.gibbs <- rownames(term.topic2)[term.topic2$most.likely.topic == "Biological Sciences"]

words.in.phys.gibbs <- rownames(term.topic2)[term.topic2$most.likely.topic == "Physical Sciences"]

words.in.social.gibbs <- rownames(term.topic2)[term.topic2$most.likely.topic == "Social Sciences"]


#----------------------

docu.topic2 <- data.frame(t.gibbs$topics)
colnames(docu.topic2) <- c( "Physical Sciences", "Biological Sciences", "Social Sciences")

# finding the most likely topic for each document
docu.topic2$most.likely.topic <- colnames(docu.topic2)[apply(docu.topic2, 1, which.max)]

# using most likely topic I can go back to intial database and find the topic name label and
# check for accuracy.

docu.in.bio.gibbs <- rownames(docu.topic2)[docu.topic2$most.likely.topic == "Biological Sciences"]

docu.in.phys.gibbs <- rownames(docu.topic2)[docu.topic2$most.likely.topic == "Physical Sciences"]

docu.in.social.gibbs <- rownames(docu.topic2)[docu.topic2$most.likely.topic == "Social Sciences"]

# I can use only these documents per topic and perform further analysis to further 
# subcategorize it as per PNAS journals archives
#
#
#
#
#
#
#

##################################### Using 2015 Prediction corpus ####################################

# transforming the dataframe of terms and topics and separate dataframe for each topic
term.topic15.gibs <- data.frame(t(t1.gibbs$terms))
colnames(term.topic15.gibs) <- c("Social Sciences", "Biological Sciences", "Physical Sciences")

terms.bio.gibbs15 <- data.frame(term.topic15.gibs$`Biological Sciences`)
rownames(terms.bio.gibbs15) <- rownames(term.topic15.gibs)

terms.socialsc.gibbs15 <- data.frame(term.topic15.gibs$`Social Sciences`)
rownames(terms.socialsc.gibbs15) <- rownames(term.topic15.gibs)

terms.phy.gibbs15 <- data.frame(term.topic15.gibs$`Physical Sciences`)
rownames(terms.phy.gibbs15) <- rownames(term.topic15.gibs)

# I can perform word cloud visualization using these data frames and even compare the words
# this is done in another file, "Wordcloud_corpfor caps"

# I can find the word associations here and can form better models
#
#
#
#
#
#



# finding the most likely word in each topics.
term.topic15.gibs$most.likely.topic <- colnames(term.topic15.gibs)[apply(term.topic15.gibs, 1, which.max)]

# creating the bag of words of each topic
words.bio.gibbs15 <- rownames(term.topic15.gibs)[term.topic15.gibs$most.likely.topic == "Biological Sciences"]

words.in.phys.gibbs15 <- rownames(term.topic15.gibs)[term.topic15.gibs$most.likely.topic == "Physical Sciences"]

words.in.social.gibbs15 <- rownames(term.topic15.gibs)[term.topic15.gibs$most.likely.topic == "Social Sciences"]


#----------------------

docu.topic15.gibs <- data.frame(t.gibbs$topics)
colnames(docu.topic15.gibs) <- c( "Social Sciences", "Biological Sciences", "Physical Sciences")

# finding the most likely topic for each document
docu.topic15.gibs$most.likely.topic <- colnames(docu.topic15.gibs)[apply(docu.topic15.gibs, 1, which.max)]

# using most likely topic I can go back to intial database and find the topic name label and
# check for accuracy.

docu.in.bio.gibbs15 <- rownames(docu.topic15.gibs)[docu.topic15.gibs$most.likely.topic == "Biological Sciences"]

docu.in.phys.gibbs15 <- rownames(docu.topic15.gibs)[docu.topic15.gibs$most.likely.topic == "Physical Sciences"]

docu.in.social.gibbs15 <- rownames(docu.topic15.gibs)[docu.topic15.gibs$most.likely.topic == "Social Sciences"]

# I can use only these documents per topic and perform further analysis to further 
# subcategorize it as per PNAS journals archives
#
#
#
#
#
#
#



#-----------------------------------------------------------------------------------------------------

#######>>>>>> Comparing the words Testing Corpus for model1 and model2, check the word cloud file<<<<<<###########
#__________________________________________________________________________________________________

# for Testing corpus
#-----------------------

# words for 'Biological Sciences'
comp.bio <- data.frame(matrix(ncol = 2, nrow = 2818))
colnames(comp.bio) <- c("Model1.words", "Model2.words")
comp.bio$Model1.words <- term.topic$`Biological Sciences`
comp.bio$Model2.words <- term.topic2$`Biological Sciences`
rownames(comp.bio) <- rownames(terms.bio)

# words for 'physical Sciences'
comp.phy <- data.frame(matrix(ncol = 2, nrow = 2818))
colnames(comp.phy) <- c("Model1.words", "Model2.words")
comp.phy$Model1.words <- term.topic$`Physical Sciences`
comp.phy$Model2.words <- term.topic2$`Physical Sciences`
rownames(comp.phy) <- rownames(terms.phy)


# words for 'Social Sciences'
comp.socialsc <- data.frame(matrix(ncol = 2, nrow = 2818))
colnames(comp.socialsc) <- c("Model1.words", "Model2.words")
comp.socialsc$Model1.words <- term.topic$`Social Sciences`
comp.socialsc$Model2.words <- term.topic2$`Social Sciences`
rownames(comp.socialsc) <- rownames(terms.socialsc)

# For prediction corpus
#-------------------------

comp.bio15 <- data.frame(matrix(ncol = 2, nrow = 2818))
colnames(comp.bio15) <- c("Model1.words", "Model2.words")
comp.bio15$Model1.words <- term.topic15$`Biological Sciences`
comp.bio15$Model2.words <- term.topic15.gibs$`Biological Sciences`
rownames(comp.bio15) <- rownames(terms.bio15)

# words for 'physical Sciences'
comp.phy15 <- data.frame(matrix(ncol = 2, nrow = 2818))
colnames(comp.phy15) <- c("Model1.words", "Model2.words")
comp.phy15$Model1.words <- term.topic15$`Physical Sciences`
comp.phy15$Model2.words <- term.topic15.gibs$`Physical Sciences`
rownames(comp.phy15) <- rownames(terms.phy15)


# words for 'Social Sciences'
comp.socialsc15 <- data.frame(matrix(ncol = 2, nrow = 2818))
colnames(comp.socialsc15) <- c("Model1.words", "Model2.words")
comp.socialsc15$Model1.words <- term.topic15$`Social Sciences`
comp.socialsc15$Model2.words <- term.topic15.gibs$`Social Sciences`
rownames(comp.socialsc15) <- rownames(terms.socialsc15)


#-------------------------------------------------------------------------------------------------
setwd("C:/UVa/own work/workspace")
save.image("Capstonebechmarking.RData")
setwd("C:/UVa/own work/workspace/PNASjournals")
