# LDA by Collapsed Gibbs Sampling
# Ref: https://shuyo.hatenablog.com/entry/20110214/lda

# Import libraries
import numpy

class LDA:
	def __init__(self, K, alpha, beta, docs, V, smartinit = TRUE):
		self.K = K
		self.alpha = alpha # parameter of topics prior
		self.beta = beta   # parameter of words prior
		self.docs = docs
		self.V = V

		# Count matrices
		self.z_m_n = []
		self.n_m_z = numpy.zeros((len(self.docs), K)) + alpha # Document-topic count matrix:dt
		self.n_z_t = numpy.zeros((K, V)) + beta               # Word-topic count matrix:wt
		self.n_z = numpy.zeros(K) + V * beta

		# Initial assignment of topic to words(?)
		self.N = 0
		for m, doc in enumerate(docs): # m:インデックス番号
			self.N += len(doc)
			z_n = []
			for t in doc:
				if smartinit:
					p_z = self.n_z_t[:, t] * self.n_m_z[m] / self.n_z         # sampling equation
					z = numpy.random.multinomial(1, p_z / p_z.sum()).argmax() # assigned topic
				else:
					z = numpy.random.randint(0, K)
				z_n.append(z)
				self.n_m_z[m, z] += 1
				self.n_z_t[z, t] += 1
				self.n_z[z] += 1
			self.z_m_n.append(numpy.array(z_n))

	def inference(self):
		"""Learning in ONE iteration"""
		for m, doc in enumerate(self.docs):
			z_n = self.z_m_n[m]
			n_m_z = self.n_m_z[m]
			for n, t in enumerate(doc):

				# Discount count matrices for n-th word "t" with topic "z"
				z = z_n[n]
				n_m_z[z] -= 1
				self.n_z_t[z, t] -= 1
				self.n_z[z] -= 1

				# Sampling new topic "new_z" for word "t"
				p_z = self.n_z_t[:, t] * n_m_z / self.n_z                     # sampling equation
				new_z = numpy.random.multinomial(1, p_z / p_z.sum()).argmax() # assigned topic

				# Set old topic with the new one & Increment count matrices
				z_n[n] = new_z
				n_m_z[new_z] += 1
				self.n_z_t[new_z, t] += 1
				self.n_z[new_z] += 1

	def worddist(self):
		"""Obtain word distribution"""
		return self.n_z_t / self.n_z[:, numpy.newaxis]

	def perplexity(self, docs = None):
		if docs == None: docs = self.docs
		phi = self.worddist()             # Word distribution
		log_per = 0
		N = 0
		Kalpha = self.K * self.alpha
		for m, doc in enumerate(docs):
			theta = self.n_m_z[m] / (len(self.docs[m]) + Kalpha) # Topic distribution
			for w in doc:
				log_per -= numpy.log(numpy.inner(phi[:, w], theta)) # log-likelihood
			N += len(doc)
		return numpy.exp(log_per / N)

def lda_learning(lda, iteration, voca):
	"""Iterating sampling"""
	pre_perp = lda.perplexity()
	print("Initial perplexity = %f" % pre_perp)
	for i in range(iteration):
		lda.inference()       # sampling
		perp = lda.perplexity # computing perplexity
		print("-%d p = %f" % (i + 1, perp))
		if pre_perp:
			if pre_perp < perp:
				output_word_topic_dist(lda, voca)
				pre_perp = None
			else:
				pre_perp = perp
	output_word_topic_dist(lda, voca)

def output_word_topic_dist(lda, voca):
	zcount = numpy.zeros(lda.K, dtype = int)
	wordcount = [dict() for k in range(lda.K)]
	for xlist, zlist in zip(lda.docs, lda.z_m_n):
		for x, z in zip(xlist, zlist):
			zcount[z] += 1
			if x in wordcount[z]:
				wordcount[z][x] += 1
			else:
				wordcount[z][x] = 1

	phi = lda.worddist()
	for k in range(lda.K):
		print("\n-- topic: %d (%d words)" % (k, zcount[k]))
		for w in numpy.argsort(-phi[k])[:20]:
			print("%s: %f (%d)" % (voca[w], phi[k, w], wordcount[k].get(w, 0)))

def main():
	import optparse
	import vocabulary
	parser = optparse.OptionParser()
	parser.add_option("-f", dest="filename", help="corpus filename")
	parser.add_option("-c", dest="corpus", help="using range of Brown corpus' files(start:end)")
	parser.add_option("--alpha", dest="alpha", type="float", help="parameter alpha", default=0.5)
	parser.add_option("--beta", dest="beta", type="float", help="parameter beta", default=0.5)
	parser.add_option("-k", dest="K", type="int", help="number of topics", default=20)
	parser.add_option("-i", dest="iteration", type="int", help="iteration count", default=100)
	parser.add_option("-s", dest="smartinit", action="store_true", help="smart initialize of parameters", default=False)
	parser.add_option("--stopwords", dest="stopwords", help="exclude stop words", action="store_true", default=False)
	parser.add_option("--seed", dest="seed", type="int", help="random seed")
	parser.add_option("--df", dest="df", type="int", help="threshold of document freaquency to cut words", default=0)
	(options, args) = parser.parse_args()
	if not (options.filename or options.corpus): parser.error("need corpus filename(-f) or corpus range(-c)")

	if options.filename:
		corpus = vocabulary.load_file(options.filename)
	else:
		corpus = vocabulary.load_corpus(options.corpus)
		if not corpus: parser.error("corpus range(-c) forms 'start:end'")
	if options.seed != None:
		numpy.random.seed(options.seed)

	voca = vocabulary.Vocabulary(options.stopwords)
	docs = [voca.doc_to_ids(doc) for doc in corpus]
	if options.df > 0: docs = voca.cut_low_freq(docs, options.df)

	lda = LDA(options.K, options.alpha, options.beta, docs, voca.size(), options.smartinit)
	print ("corpus=%d, words=%d, K=%d, a=%f, b=%f" % (len(corpus), len(voca.vocas), options.K, options.alpha, options.beta))

	#import cProfile
	#cProfile.runctx('lda_learning(lda, options.iteration, voca)', globals(), locals(), 'lda.profile')
	lda_learning(lda, options.iteration, voca)

if __name__ == "__main__":
	main()