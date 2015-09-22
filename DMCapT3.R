
file <- "./categories/Chinese.txt"
train.file <- "./categories/Chinese_test.txt"
corpus.file <- "./categories/Chinese_corpus.txt"
reviews <- cuisine.doc <- readLines(file)
nonblank <- reviews != c("")
reviews <- reviews[nonblank]
review.sample <- sample(reviews, 5000)
write(review.sample, train.file)
write(reviews, corpus.file)
