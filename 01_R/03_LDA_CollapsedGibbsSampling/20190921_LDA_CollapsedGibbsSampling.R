# 【参考Webサイト】
# Latent Dirichlet Allocation Using Gibbs Sampling：
# https://ethen8181.github.io/machine-learning/clustering_old/topic_model/LDA.html

# Libraries
library(ggplot2)

# Load "LDA1" function
source("C:/Users/YOSHIHARA/Box Sync/0001_Research/0005_Project with Kei TAKAHASHI/0002_第9回スポーツデータ解析コンペ/0000_お勉強/0001_Topic Model/R/0001_R Files/20190921_LDA_CollapsedGibbsSampling/LDA1.R")

#=================================================================================#
# Create a set of documents & Convert each document into wordIDs 
#=================================================================================#
# A total of 8 documents

# Create a set of documents
rawdocs = c(
	"eat turkey on turkey day holiday", 
	"i like to eat cake on holiday", 
	"turkey trot race on thanksgiving holiday", 
	"snail race the turtle", 
	"time travel space race", 
	"movie on thanksgiving", 
	"movie at air and space museum is cool movie", 
	"aspiring movie star"
)

# Create a kind of word list for each document
docs = strsplit(rawdocs, split = " ")

# Unique words
vocab = unique(unlist(docs))

# Replace words with wordIDs
for(i in 1 : length(docs)){
	docs[[i]] = match(docs[[i]], vocab)
}
#docs

#=================================================================================#
# Preparation
#=================================================================================#

# Set the number of topics
#K = 2

#------------------------------------------#
# Initialize count matrices
#------------------------------------------#

# wt : word-topic matrix(Column: topic, Row: word)
# the count of each word being assigned to each topic
#wt = matrix(0, K, length(vocab))
#colnames(wt) = vocab

# ta : topic assignment list
#ta = lapply(docs, function(x) rep(0, length(x)))
#names(ta) = paste0("doc", 1 : length(docs))

# dt : document-topic matrix(Column: document, Row: topic)
# the number of words assigned to each topic for each document
#dt = matrix(0, length(docs), K)

#------------------------------------------#
# Randomly assign topics to words
#------------------------------------------#
#for(d in 1 : length(docs)){
#	for(w in 1 : length(docs[[d]])){
#
#		# Randomly assign topic(1 ~ K) to word "w" in document "d" 
#		ta[[d]][w] = sample(1 : K, 1)
#
#		# Increment word-topic matrix
#		ti = ta[[d]][w]                # chosen topic
#		wi = docs[[d]][w]              # chosen word
#		wt[ti, wi] = wt[ti, wi] + 1
#	}
#
#	# Update document-topic matrix (Count words in document "d" assinged to each topic "t")
#	for(t in 1 : K){
#		dt[d, t] = sum(ta[[d]] == t)
#	}
#}
#print(ta)
#print(wt)
#print(dt)

#=================================================================================#
# Estimate "z_di" : Collapsed Gibbs Sampling
#=================================================================================#

#------------------------------------------#
# ONE iteration only (for understanding)
#------------------------------------------#
# Update topic for w_11 (=1st word of the 1st document)

# Define parameters
#alpha = 1 # Dirichlet hyperparameter for topic distribution "theta"
#beta = 1  # Dirichlet hyperparameter for word distribution "phi"

# Initial topic assigned to w11 & its corresponding wordID
#t0 = ta[[1]][1]    # topic assigned
#wid = docs[[1]][1] # wordID

# Decrement word-topic & document-topic matrices for 1st word of the 1st document
#wt[t0, wid] = wt[t0, wid] - 1
#dt[1, t0] = dt[1, t0] - 1

# Calculate sampling probability: P(z_11 = j | z_-i, w_i, d_i)
#left = (wt[, wid] + beta) / (rowSums(wt) + length(vocab) * beta)
#right = (dt[1, ] + alpha) / (sum(dt[1, ]) + K * alpha)

# Draw net topic for w_11
#t1 = sample(1 : K, 1, prob = left * right)
#t1

#------------------------------------------#
# 
#------------------------------------------#

# Define parameters
K = 2
alpha = 1
beta = 0.001
iterations = 5000
set.seed(1234)

# Apply Collapsed Gibbs Sampling
lda1 = LDA1(docs = docs, vocab = vocab, K = K, alpha = alpha, beta = beta, iterations = iterations)
#lda1

#=================================================================================#
# Estimate topic and word distributions 
#=================================================================================#

#------------------------------------------#
# Topic distribution
#------------------------------------------#

# Topic distribution for each document
theta = (lda1$dt + alpha) / (rowSums(lda1$dt) + K * alpha)

# Topic assigned to each document with the highest probability
topic = apply(theta, 1, which.max)

#------------------------------------------#
# Word distribution
#------------------------------------------#

# Word distribution for each topic
phi = (lda1$wt + beta) / (rowSums(lda1$wt) + length(vocab) * beta)

# FIGURE Ex.: Word distribution for topic 1
barplot(phi[1, ])

# ggplot ver.
#phifig = as.data.frame(phi)
#ggplot(phifig[1, ], aes(x = colnames(phifig), y = rownames(phifig)[1])) + geom_bar(stat = "identity")

# Top n words for each topic
Terms = function(phi, n){
	term = matrix(0, n, K)
	for(p in 1 : nrow(phi)){
		term[, p] = names(sort(phi[p, ], decreasing = TRUE)[1 : n])
	}
	return(term)
}
term = Terms(phi = phi, n = 3)

#------------------------------------------#
# Display document list and top n words for each topic
#------------------------------------------#
list(original_text = rawdocs[topic == 1], words = term[, 1])
list(original_text = rawdocs[topic == 2], words = term[, 2])







