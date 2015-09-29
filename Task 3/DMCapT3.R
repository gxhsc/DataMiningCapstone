
file <- "./categories/Chinese.txt"
train.file <- "./categories/Chinese_test.txt"
corpus.file <- "./categories/Chinese_corpus.txt"
sp.reviews <- readLines(file)
sp.nonblank <- reviews != c("")
sp.reviews <- reviews[nonblank]
sp.review.sample <- sample(sp.reviews, 5000)
write(sp.review.sample, train.file)
write(sp.reviews, corpus.file)
sp.terms<-read.csv("ranking_1.csv", header = FALSE)
sp.terms <- as.vector(sp.terms$V1[1:10000])
sp.terms <- c("Chinese", sp.terms)
write(sp.terms, "Chinese_segphrase")

tm.terms <- read.delim("topPhrases.txt", header = FALSE, sep = "\t")
names <- tm.terms[1]
tm.matrix <- as.matrix(tm.terms[, 2], byrow = TRUE)
rownames(tm.matrix) <- names[[1]]
colnames(tm.matrix) <- c("Frequency")

