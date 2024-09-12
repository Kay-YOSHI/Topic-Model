# 【参考Webサイト】
# [R]トピックモデル(LDA)を用いた大量文書の教師なし分類：https://qiita.com/YM_DSKR/items/017a5dddeb56fcdf1054

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

# Libraries
library(tm)
library(magrittr)
library(stringr)
library(tidytext)
library(dplyr)
library(RMeCab)
library(purrr)
library(topicmodels)
library(ggplot2)

#===============================================================#
# Load All Text Data as Corpus
#===============================================================#
corpus = VCorpus(DirSource(dir = "C:/Users/YOSHIHARA/Box Sync/0001_Research/0005_Project with Kei TAKAHASHI/0002_第9回スポーツデータ解析コンペ/0000_お勉強/0001_Topic Model/R/0001_R Files/20190806_LDA1/all_text", encoding = "utf-8"), readerControl = list(language = "eng"))
#inspect(corpus[[1]])

#===============================================================#
# Preprocessing
#===============================================================#
# For English documents, the followings are enough

#corpus %<>% tm_map(stripWhitespace) # Delete white spaces
#corpus %<>% tm_map(content_transformer(tolower)) # Transform capital letter to lower-case letter
#corpus %<>% tm_map(removePunctuation) # Delete punctuations
#corpus %<>% tm_map(removeNumbers) # Delete numbers
#corpus %<>% tm_map(removeWords, stopwords("english")) # Delete stop words

#===============================================================#
# Transform Corpus to "tibble" Form
#===============================================================#

# Using "tidy" function, Transform corpus to tibble
df_1 = tidy(corpus)

# Delete discarded columns(Extract id/text columns from "df_1")
df_id = df_1 %>% select("id", "text")

#===============================================================#
# Write out "tibble" to csv file & Load csv as dataframe
#===============================================================#
# Objective : Transform character code from utf-8 to shift-jis

df_id2 = df_id %>% as.data.frame()
write.csv(df_id2, "df_id2.csv", row.names = F)
df_id3 = read.csv("df_id2.csv", header = T)

#===============================================================#
# Morphological Analysis : Construct Frequency Table
#===============================================================#
df_id4 = df_id3 %>% RMeCabDF("text", 1)
df_id_tokens = purrr::pmap_df(list(nv = df_id4, title = df_id2$id), function(nv, title){tibble(title = title, term = nv, hinshi = names(nv))})
#tail(df_id_tokens, 10)

#===============================================================#
# Delete stopwords & Count words
#===============================================================#

# Load list of stopwords
stop_word = read.csv("jpn_stopword.csv", header = F, fileEncoding = "UTF-8-BOM")
stop_word = data.frame(stop_word)
colnames(stop_word) = c("term")
stop_word$term = as.character(stop_word$term)

# Delete stopwords & Restrict to noun and adjective $ Count words
df_id_tokens %<>% anti_join(stop_word, by = "term") %>% filter(hinshi %in% c("名詞", "形容詞")) %>% group_by(title, term) %>% summarise(count = n()) %>% ungroup
#tail(df_id_tokens, 10)

#===============================================================#
# Transform Frequency Table to Document Term Matrix(DTM)
#===============================================================#
DTM_id = cast_dtm(df_id_tokens, document = "title", term = "term", value = "count")

#===============================================================#
# Apply Topic Model to DTM
#===============================================================#
topic_id = LDA(DTM_id, k = 9) # k : Number of topics(exogenous)
topic_id2 = topic_id %>% tidy() %>% as.data.frame # Transform result to data frame
#tail(topic_id2, 40)

#===============================================================#
#Visualization
#===============================================================#

# Term(Word) distribution of Top20 words for each topic
topic_id_3 = group_by(topic_id2, topic) %>% top_n(20, beta) %>% ungroup() %>% mutate(term = reorder(term, beta)) %>% arrange(topic, -beta)
topic_id_3 %>% mutate(term = reorder(term, beta)) %>% group_by(topic, term) %>% arrange(desc(beta)) %>% ungroup() %>% mutate(term = factor(paste(term, topic, sep = ""), levels = rev(paste(term, topic, sep = "")))) %>% ggplot(aes(x = term, y = beta, fill = beta)) + geom_bar(stat = "identity") + facet_wrap(~topic, scales = "free") + coord_flip() + scale_x_discrete(labels = function(x)gsub("_.+$", "", x))

# Top20 documents for each topic
topic_id_gamma = topic_id %>% tidy(matrix = "gamma") %>% as.data.frame 
topic_id_gamma2 = group_by(topic_id_gamma, topic) %>% top_n(20, gamma) %>% ungroup() %>% mutate(document = reorder(document, gamma)) %>% arrange(topic, -gamma)
ggplot(topic_id_gamma2, aes(x = document, y = gamma, fill = gamma)) + geom_bar(stat = "identity") + facet_wrap(~topic, scales = "free") + coord_flip()

# Average topic distribution for each document category
topic_id_gamma$document %<>% str_remove_all("-\\d{7}.txt")
topic_id_cat = topic_id_gamma %>% group_by(document, topic) %>% summarise(mean = mean(gamma)) %>% ggplot(aes(x = topic, y = mean, fill = as.factor(topic))) + geom_bar(stat = "identity") + facet_wrap(~document) + scale_x_continuous(breaks = seq(1, 9, by = 1), labels = c(1,2,3,4,5,6,7,8,9))
topic_id_cat


