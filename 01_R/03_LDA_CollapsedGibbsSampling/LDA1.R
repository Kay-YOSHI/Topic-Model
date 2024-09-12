# (2019/09/21)
# We should take into account the following parameters
# - burnin : number of ommited Gibbs iterations at beginning (Samples at beginning most likely do not correctly reflect the properties of distribution)
# - thin : number of omitted in-between Gibbs iterations (In order to prevent correlations between samples during the iteration)

LDA1 = function(docs, vocab, K, alpha, beta, iterations){
	
	#---------------------------------------------------#
	# Initialize count matrices
	#---------------------------------------------------#

	# Word-topic count matrix
	wt = matrix(0, K, length(vocab))
	colnames(wt) = vocab

	# Topic assignment list
	ta = lapply(docs, function(x) rep(0, length(x)))
	names(ta) = paste0("doc", 1 : length(docs))

	# Document-topic count matrix
	dt = matrix(0, length(docs), K)

	#---------------------------------------------------#
	# Randomly assign topics to words
	#---------------------------------------------------#
	for(d in 1 : length(docs)){
		for(w in 1 : length(docs[[d]])){

		# Randomly assign topic(1 ~ K) to word "w" in document "d" 
		ta[[d]][w] = sample(1 : K, 1)

		# Increment word-topic matrix
		ti = ta[[d]][w]                # chosen topic
		wi = docs[[d]][w]              # chosen word
		wt[ti, wi] = wt[ti, wi] + 1
		}

		# Update document-topic matrix (Count words in document "d" assinged to each topic "t")
		for(t in 1 : K){
			dt[d, t] = sum(ta[[d]] == t)
		}
	}

	#---------------------------------------------------#
	# Collapsed Gibbs Sampling
	#---------------------------------------------------#
	for(i in 1 : iterations){

		# For each document
		for(d in 1 : length(docs)){

			# For each word
			for(w in 1 : length(docs[[d]])){

				# Initial topic assigned to w11 & its corresponding wordID
				t0 = ta[[d]][w]    # topic assigned
				wid = docs[[d]][w] # wordID

				# Decrement word-topic & document-topic matrices for 1st word of the 1st document
				wt[t0, wid] = wt[t0, wid] - 1
				dt[d, t0] = dt[d, t0] - 1

				# Calculate sampling probability: P(z_11 = j | z_-i, w_i, d_i)
				left = (wt[, wid] + beta) / (rowSums(wt) + length(vocab) * beta)
				right = (dt[d, ] + alpha) / (sum(dt[d, ]) + K * alpha)

				# Draw net topic for w_11
				t1 = sample(1 : K, 1, prob = left * right)

				# Update topic assignment list with newly sampled topic for word "w"
				ta[[d]][w] = t1

				# Increment word-topic & document-topic count matrices with the new sampled topic for word "w"
				wt[t1, wid] = wt[t1, wid] + 1
				dt[d, t1] = dt[d, t1] + 1

				# Examine when topic assignments change
				#if(t0 != t1){
				#	print(paste0("doc:", d, "word:", w, "topic:", t0, "=>", t1))
				#}
			}
		}
	}

	# Display iteration process
	print(paste0("All Iterations FINISHED"))

	# Return a list containing the final word-topic and document-topic count matrices
	return(list(wt = wt, dt = dt))
}
