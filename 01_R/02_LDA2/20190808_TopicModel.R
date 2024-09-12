# 【参考Webサイト】
# 6 Topic modeling：https://www.tidytextmining.com/topicmodeling.html

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
#install.packages("gutenbergr")
#install.packages("scales")

#===============================================================#
# Load Data & Apply LDA
#===============================================================#
# Data : 2246 news articles from an American News Agency

library(topicmodels)
data("AssociatedPress") # Document Term Matrix(DTM)
#AssociatedPress

# LDA
ap_lda = LDA(AssociatedPress, k = 2, control = list(seed = 1234)) # k : Number of topics
#ap_lda

#=====================================================#
# Visualization1 : Word-topic probabilities 
# (Word distribution for each topic)
#=====================================================#
library(tidytext)
library(dplyr)
library(ggplot2)

#=======================================#
# Probability of each word within topic
#=======================================#
ap_topics = tidy(ap_lda, matrix = "beta")
#ap_topics

#=======================================#
# Top10 words within each topic
#=======================================#
ap_top_terms = ap_topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
ap_top_terms %>% mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~topic, scales = "free") + coord_flip()

#=======================================#
# Difference in prob. of each word across topics
#=======================================#
library(tidyr)
beta_spread = ap_topics %>% mutate(topic = paste0("topic", topic)) %>% spread(topic, beta) %>% filter(topic1 > .001 | topic2 > .001) %>% mutate(log_ratio = log2(topic2 / topic1))
#beta_spread

# Top10 words w.r.t log_ratio
beta_spread2 = beta_spread %>% top_n(10, log_ratio) %>% arrange(-log_ratio) 

# Bottom10 words w.r.t log_ratio
beta_spread3 = beta_spread %>% top_n(-10, log_ratio) %>% arrange(-log_ratio) 

# Bind Top10 & Bottom10 words
beta_spread4 = bind_rows(beta_spread2, beta_spread3)

# Visualization
beta_spread4 %>% mutate(term = reorder(term, log_ratio)) %>% ggplot(aes(term, log_ratio)) + geom_col(show.legend = FALSE) + coord_flip()

#=====================================================#
# Visualization2 : Document-topic probabilities 
# (Word distribution for each topic)
#=====================================================#
ap_documents = tidy(ap_lda, matrix = "gamma")
#ap_documents

# Most common words in Document6
tidy(AssociatedPress) %>% filter(document == 6) %>% arrange(desc(count))


#===============================================================#
# Example : Classify book chapters into correct books
#===============================================================#
# When examining a statistical method, it can be useful to try it on a very simple case where you know the “right answer”. 
# For example, we could collect a set of documents that definitely relate to four separate topics, 
# then perform topic modeling to see whether the algorithm can correctly distinguish the four groups. 

#=====================================================#
# Load Data
#=====================================================#

# Book titles : 4 books
titles = c("Twenty Thousand Leagues under the Sea", "The War of the Worlds", "Pride and Prejudice", "Great Expectations")

# Text of each book from "gutenbergr" package
library(gutenbergr)
books = gutenberg_works(title %in% titles) %>% gutenberg_download(meta_fields = "title")

#=====================================================#
# Pre-processing
#=====================================================#
library(stringr)

# Divide books into chapters(documents)
by_chapter = books %>% group_by(title) %>% mutate(chapter = cumsum(str_detect(text, regex("^chapter", ignore_case = TRUE)))) %>% ungroup() %>% filter(chapter > 0) %>% unite(document, title, chapter)

# Split into words
by_chapter_word = by_chapter %>% unnest_tokens(word, text)

# Find document-word counts
word_counts = by_chapter_word %>% anti_join(stop_words) %>% count(document, word, sort = TRUE) %>% ungroup()
#word_counts

# Transform document-word counts into document-term matrix for applying LDA
chapters_dtm = word_counts %>% cast_dtm(document, word, n)
#chapters_dtm

#=====================================================#
# LDA on Chapters
#=====================================================#
chapters_lda = LDA(chapters_dtm, k = 4, control = list(seed = 1234))
#chapters_lda

#=======================================#
# Results
#=======================================#

# Word distribution within topic
chapter_topics = tidy(chapters_lda, matrix = "beta")
#chapter_topics

# Top5 words within each topic
top_terms = chapter_topics %>% group_by(topic) %>% top_n(5, beta) %>% ungroup() %>% arrange(topic, -beta)
#top_terms
top_terms %>% mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~topic, scales = "free") + coord_flip()

#=====================================================#
# Per-document Classification
#=====================================================#
chapters_gamma = tidy(chapters_lda, matrix = "gamma")
#chapters_gamma

# Re-separate the document name into title and chapter
# - Split "document" column into "title" and "chapter" columns
chapters_gamma = chapters_gamma %>% separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
#chapters_gamma

# Visualization
# - Reorder titles in order of topic 1, 2, ... before plotting
chapters_gamma %>% mutate(title = reorder(title, gamma * topic)) %>% ggplot(aes(factor(topic), gamma)) + geom_boxplot() + facet_wrap(~title)

#=======================================#
# How many chapters misclassified?
#=======================================#

# Find the topic that was most associated with each chapter(Classification of each chapter)
chapter_classifications = chapters_gamma %>% group_by(title, chapter) %>% top_n(1, gamma) %>% ungroup()
#chapter_classifications

# Compare each to the "consensus" topic for each book 
# (the most common topic among its chapters), and see which were most often misidentified
book_topics = chapter_classifications %>% count(title, topic) %>% group_by(title) %>% top_n(1, n) %>% ungroup() %>% transmute(consensus = title, topic)
chapter_classifications %>% inner_join(book_topics, by = "topic") %>% filter(title != consensus)

#=====================================================#
# By Word Assignments
#=====================================================#

# Construct a data frame of document-term counts
# - Which words in each document were assigned to which topic?
assignments = augment(chapters_lda, data = chapters_dtm)
#assignments

# Which words were incorrectly classified?
# - Column "title" : TRUE title, Column "consensus" : Assigned title
assignments = assignments %>% separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>% inner_join(book_topics, by = c(".topic" = "topic"))
#assignments

# Visualize a confusion matrix
# - How often words from one book were assigned to another book?
library(scales)
assignments %>% count(title, consensus, wt = count) %>% group_by(title) %>% mutate(percent = n / sum(n)) %>% ggplot(aes(consensus, title, fill = percent)) + geom_tile() + scale_fill_gradient2(high = "red", label = percent_format()) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid = element_blank()) + labs(x = "book words were assigned to", y = "Book words came from", fill = "% of assignments")

# What were the most commonly mistaken words?
wrong_words = assignments %>% filter(title != consensus)
#wrong_words
wrong_words %>% count(title, consensus, term, wt = count) %>% ungroup() %>% arrange(desc(n))

# There are a few wrongly classified words that never appeared in the novel they were misassigned to
# Ex. the word "flopson" appears only in Great Expectations, but it's assigned to Pride and Prejudice
word_counts %>% filter(word == "flopson")





