# 【参考Webサイト】
# Fitting LDA Models in R：http://i.amcat.nl/lda/stm.html

# Install Packages
#install.packages("tm")
#install.packages("magrittr")
#install.packages("stringr")
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("RMeCab", repos = "http://rmecab.jp/R")
#install.packages("purrr")
#install.packages("topicmodels")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("quanteda")
#install.packages("corpustools")
#install.packages("stm")
#install.packages("igraph")

#===============================================================#
# Load Data & Construct Document-Feature Matrix(DFM)
#===============================================================#
# Data : State of the union speeaches of Bush and Obama
# Covariates(Side informations) : Year, President
# DFM = [DocumentID, Side information1, 2, ...]

library(quanteda)
library(corpustools)
data("sotu_texts", package = "corpustools")
#sotu_texts
sotu_texts$id = as.character(sotu_texts$id)
sotu_texts$year = as.numeric(format(sotu_texts$date, "%Y"))
sotu = corpus(sotu_texts, docid_field = "id", text_field = "text")
sotu_dfm = dfm(sotu, remove_punct = TRUE, remove = stopwords("english")) # Construct DFM
sotu_dfm = dfm_trim(sotu_dfm, min_termfreq = 2)
#head(docvars(sotu_dfm))

# Number of documents per president(?)
table(docvars(sotu_dfm)$president)

#===============================================================#
# 1. W/O any covariates(But, w/ correlations between topics)
#===============================================================#
library(stm)

# "stm" function
#  K : Number of topics desired
#  max.em.its : Maximum number of EM iterations
#  control : List of additional advanced parameters
#   alpha : Prevalence hyperparameter in collapsed gibbs sampling in LDA initializations
m = stm(sotu_dfm, K = 10, max.em.its = 100, control = list(alpha = 1))

#=================================#
# Various Result Visualization
#=================================#

# Top topics
plot(m, type = "summary")

# Top words per topic
labelTopics(m, topic = 9)
cloud(m, topic = 9)

# Words between two topics
plot(m, type = "perspectives", topics = c(4, 5))

# Correlation between topics
library(igraph)
corr = topicCorr(m)
plot(corr)

#===============================================================#
# 2. W/ Prevalence Covariate "year"
#===============================================================#
m2 = stm(sotu_dfm, K = 10, prevalence = ~year, max.em.its = 100)

# Regression : Effect of "year" on expected topic proportion of each topic
prep = estimateEffect(1:10 ~year, stmobj = m2, meta = docvars(sotu_dfm))
summary(prep, topics = 1)
plot(prep, "year", method = "continuous", topics = c(1, 3), model = m2)

#===============================================================#
# 3. W/ Prevalence & Content Covariates "year" & "president"
#===============================================================#
library(ggplot2)

# Create spline function of year
splines = cbind(data.frame(year = 1990:2000), s(1990:2000, 4))
splines = reshape2::melt(splines, id.var = "year")
#ggplot(splines, aes(x = year, y = value, color = variable, group = variable)) + geom_line()

# STM w/ prevalence & content covariates
m3 = stm(sotu_dfm, K = 10, prevalence = ~s(year, 4), content = ~president, max.em.its = 100)

# Estimate the effect of "year" in a non-linear fashion
prep2 = estimateEffect(1:10 ~s(year, 4), stmobj = m3, meta = docvars(sotu_dfm))
summary(prep2, topics = 2)
plot(prep2, "year", method = "continuous", topics = 2, model = m3)

# Visualization in terms of top words
labelTopics(m3)
plot(m3, type = "perspectives", topics = 2) # Words per president per topic















