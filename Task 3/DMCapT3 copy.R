
file <- "./categories/Chinese.txt"
train.file <- "./categories/Chinese_test.txt"
corpus.file <- "./categories/Chinese_corpus.txt"
sp.reviews <- readLines(file)
sp.nonblank <- reviews != c("")
sp.reviews <- reviews[sp.nonblank]
sp.review.sample <- sample(sp.reviews, 5000)
write(sp.review.sample, train.file)
write(sp.reviews, corpus.file)
sp.terms<-read.csv("ranking_1.csv", header = FALSE)
sp.terms <- as.vector(sp.terms$V1[1:10000])
#sp.terms <- c("Chinese", sp.terms)
sp.top <- sp.terms[1:3000]


tm.terms <- read.delim("topPhrases.txt", header = FALSE, sep = "\t")
snames <- tm.terms[1]
tm.matrix <- matrix(tm.terms[, 2], 1, length(tm.terms[[1]]))
colnames(tm.matrix) <- names[[1]]
tm.stm <- as.simple_triplet_matrix(tm.matrix)
tm.top <- as.vector(tm.terms[[1]][1:3700])

labels <- read.delim("Chinese.mod.label", header = FALSE, sep = "\t")
labels <- as.vector(labels[1:124,1])
combined <- c("Chinese", labels, sp.top, tm.top)
write(combined, "Chinese_terms.txt")

seedwords <- c("egg rolls", "fried rice", "Mongolian beef", "crab puffs", "prices are reasonable")

SEED <- 2010
k<-2
Gibbs = LDA(tm.stm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 500, keep = 50))
t <- terms(Gibbs, 50)
