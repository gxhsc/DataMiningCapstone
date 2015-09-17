library(NLP)
library(SnowballC)
library(topicmodels)
library(tm)
library(slam)
library(lsa)
library(stats)
library(corrplot)
library(RColorBrewer)

# Import the reviews for each cuisine. Reviews for each cuisine are contained in a separate file.
# For each file read in the file name and file contents.   Insert review text as a row in the data
# frame.  The row name becomes the file name, which is the cuisine name.

dir <- DirSource(directory = "./categories", encoding = "", recursive = FALSE, ignore.case = FALSE, mode = "text")
reviews <- list()
docs <- c()
cuisine.names <- c()
open(dir)
num.cuisines <- dir$length
for (i in 1:num.cuisines) {
    file <- dir$filelist[i]
    
    n <- (strsplit(file, "/"))
    n <- n[[1]][3]
    n <- strsplit(n, ".txt")
    cuisine.names <- c(cuisine.names, n[[1]])
    
    cuisine.doc <- readLines(file)
    # Add the vector of individual reviews for each cusine to the reviews list
    #reviews[[i]] <- cuisine.doc
    reviews[[i]] <- iconv(cuisine.doc, to = "ASCII", sub="")
    
    # Concatenate all reviews for the cusine and append to the docs matrix
    docs <- rbind(docs, paste(cuisine.doc, collapse = " "))
}
close(dir)
docs <- data.frame(docs)

# Create the document corpus from the data frame.
dfs <- DataframeSource(docs)
corpus <- Corpus(dfs)

# Pre-process the corpus: review extra white space, remove punctuation, remove numbers, remove stop 
# words and stem it.
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

# Create the term-document matrix with term frequency weighting.
dtm.tf <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, stemming = FALSE, minWordLength = 3))
sd <- summary(col_sums(dtm.tf))
sd
term.tfidf <- tapply(dtm.tf$v/row_sums(dtm.tf)[dtm.tf$i], dtm.tf$j, mean) * log2(nDocs(dtm.tf)/col_sums(dtm.tf > 0))
summary(term.tfidf)
dtm.tf <- dtm.tf[,term.tfidf >= 5.0e-06]
dtm.tf <- dtm.tf[row_sums(dtm.tf) > 0,]
sd2 <- summary(col_sums(dtm.tf))
sd2
tdm.tf <- as.TermDocumentMatrix(dtm.tf)
#tdm.tf <- TermDocumentMatrix(corpus, control = list(stopwords = TRUE, stemming = FALSE, minWordLength = 3))

# Create the term-document matrix with term frequency - inverse document frequency weighting.
tdm.tfidf <- weightTfIdf(tdm.tf, normalize = TRUE)

# Compute a topic model from term document matrix
SEED <- 2010
k<-25
#Gibbs = LDA(tdm.tf, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000, keep = 50))

# Compute the cosine similarity matrix from the term document matrix.
#cosine.matrix <- crossprod_simple_triplet_matrix(tdm.tf)/(sqrt(col_sums(tdm.tf^2) %*% t(col_sums(tdm.tf^2))))
cosine.matrix <- cosine(as.matrix(tdm.tf))
rownames(cosine.matrix) <- cuisine.names
colnames(cosine.matrix) <- cuisine.names

# Compute the cosine similarity matrix from the tf-idf term document matrix.
#cosine.matrix.tfidf <- crossprod_simple_triplet_matrix(tdm.tfidf)/(sqrt(col_sums(tdm.tfidf^2) %*% t(col_sums(tdm.tfidf^2))))
cosine.matrix.tfidf <- cosine(as.matrix(tdm.tfidf))
rownames(cosine.matrix.tfidf) <- cuisine.names
colnames(cosine.matrix.tfidf) <- cuisine.names

#write.matrix(format(cosine.matrix, scientific=FALSE), file = paste("./", "sim_matrix_noidf.csv", sep="/"), sep=",")
#write.matrix(format(cosine.matrix.tfidf, scientific=FALSE), file = paste("./", "sim_matrix_tfidf.csv", sep="/"), sep=",")

# Create visualization of matrix
#blue.colors <- brewer.pal(9,"Blues")
#heatmap(cosine.matrix, cexCol = 0.5, cexRow = 0.5, Rowv=NA, Colv=NA, col = blue.colors, revC = TRUE, scale="column")
#heatmap(cosine.matrix.tfidf, cexCol = 0.7, cexRow = 0.7, Rowv=NA, Colv=NA, col = blue.colors, revC = TRUE, scale="column")

###############################################################################
# Task 2.3
###############################################################################

# Perform clustering on cosine similarity matrix with no idf
set.seed(1010)
km <- kmeans(cosine.matrix, 10, iter.max = 10)
cluster <- km$cluster
size <- km$size


cluster.matrix <- cbind(cosine.matrix, cluster)
cluster.matrix <- rbind(cluster.matrix, cluster)

by.cluster <- order(cluster.matrix[,"cluster"])
cluster.matrix <- cluster.matrix[by.cluster, by.cluster]
cluster.matrix <- cluster.matrix[!rownames(cluster.matrix) %in% "cluster", ]
cluster.matrix <- cluster.matrix[, !colnames(cluster.matrix) %in% "cluster"]

# Perform clustering on cosine similarity matrix with tf-idf
set.seed(1010)
km.tfidf <- kmeans(cosine.matrix.tfidf, centers = 10, iter.max = 10, algorith = "MacQueen")
cluster <- km$cluster
size.tfidf <- km$size

cluster.matrix.tfidf <- cbind(cosine.matrix.tfidf, cluster)
cluster.matrix.tfidf <- rbind(cluster.matrix.tfidf, cluster)

by.cluster <- order(cluster.matrix.tfidf[,"cluster"])
cluster.matrix.tfidf <- cluster.matrix.tfidf[by.cluster, by.cluster]
cluster.matrix.tfidf <- cluster.matrix.tfidf[!rownames(cluster.matrix.tfidf) %in% "cluster", ]
cluster.matrix.tfidf <- cluster.matrix.tfidf[, !colnames(cluster.matrix.tfidf) %in% "cluster"]

# Plot the results for the tf (no idf) similartiy matrix
corrplot(cosine.matrix, method = "circle", cl.lim = c(0, 1), tl.cex = 0.6, tl.col = "black")

# Plot the results for the tf-idf similartiy matrix
corrplot(cosine.matrix.tfidf, method = "circle", cl.lim = c(0, 1), tl.cex = 0.6, tl.col = "black")

# Plot the results for k-means clustering of tf (no idf) matrix
corrplot(cluster.matrix, method = "circle", cl.lim = c(0.4, 1), tl.cex = 0.6, tl.col = "black")
corrRect(size)

# Plot results for k-means clustering of tf-idf matrix
corrplot(cluster.matrix.tfidf, method = "circle", cl.lim = c(0, 1), tl.cex = 0.6, tl.col = "black")
corrRect(size.tfidf)

#Plot the results for hierarchical clustering of the tf matrix
corrplot(cosine.matrix, method = "circle", order = "hclust", hclust.method ="average", addrect = 10, cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black", outline = TRUE)

#Plot the results for hierarchical clustering of the tf-idf matrix
corrplot(cosine.matrix.tfidf, method = "circle", order = "hclust", hclust.method ="average", addrect = 10, cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black", outline = TRUE)

