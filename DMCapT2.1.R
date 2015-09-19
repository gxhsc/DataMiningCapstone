library(NLP)
library(SnowballC)
library(topicmodels)
library(tm)
library(slam)
library(lsa)
library(stats)
library(corrplot)
library(RColorBrewer)
library(proxy)
library(MASS)
library(utils)

# Function to perform column-wise scaling, to values between 0 and 1, on a matrix
scale.mat <- function(m) {
    maxs <- apply(m, 2, max)
    mins <- apply(m, 2, min)
    scale.default(m, center = mins, scale = maxs - mins)
}

###############################################################################
# Task 2.1
###############################################################################

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
term.tfidf <- tapply(dtm.tf$v/row_sums(dtm.tf)[dtm.tf$i], dtm.tf$j, mean) * log2(nDocs(dtm.tf)/col_sums(dtm.tf > 0))
summary(term.tfidf)
dtm.tf <- dtm.tf[,term.tfidf >= 5.0e-06]
dtm.tf <- dtm.tf[row_sums(dtm.tf) > 0,]
sd2 <- summary(col_sums(dtm.tf))
tdm.tf <- as.TermDocumentMatrix(dtm.tf)

# Create the term-document matrix with term frequency - inverse document frequency weighting.
tdm.tfidf <- weightTfIdf(tdm.tf, normalize = TRUE)

# #######################################################################
# DELETE
# #######################################################################
#tf.matrix <- as.matrix(read.csv("tf_sim_matrix.csv", header = TRUE, sep = ","))
#tfidf.matrix <- as.matrix(read.csv("tfidf_sim_matrix.csv", header = TRUE, sep = ","))
#topic.dist <- as.matrix(read.csv("topic_dist.csv", header = FALSE, sep = ","))
#lda.matrix.dist <- as.matrix(read.csv("lda_matrix_dist.csv", header = TRUE, sep = ","))
#lda.matrix.simil <- as.matrix(read.csv("lda_matrix_simil.csv", header = TRUE, sep = ","))

# Compute the cosine similarity matrix from the term document matrix.
tf.matrix <- cosine(as.matrix(tdm.tf))
rownames(tf.matrix) <- cuisine.names
colnames(tf.matrix) <- cuisine.names
#write.matrix(format(tf.matrix, scientific=FALSE), file = paste("./", "tf_sim_matrix.csv", sep="/"), sep=",")
tf.scaled <- scale.mat(tf.matrix)

# Plot the results for the tf similartiy matrix
corrplot(tf.scaled, method = "circle", is.corr = FALSE, title = "Term Frequency", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")

# Compute the cosine similarity matrix from the tf-idf term document matrix.
tfidf.matrix <- cosine(as.matrix(tdm.tfidf))
rownames(tfidf.matrix) <- cuisine.names
colnames(tfidf.matrix) <- cuisine.names
#write.matrix(format(tfidf.matrix, scientific=FALSE), file = paste("./", "tfidf_sim_matrix.csv", sep="/"), sep=",")
tfidf.scaled <- scale.mat(tfidf.matrix)

# Plot the results for the tf-idf similartiy matrix
corrplot(tfidf.scaled, method = "circle", is.corr = FALSE, title = "TF-IDF", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")

# Compute a topic model from term document matrix
#SEED <- 2010
#k<-50
#Gibbs = LDA(dtm.tf, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000, keep = 50))
#topic.dist <- Gibbs@gamma
#write.matrix(format(topic.dist, scientific=FALSE), file = paste("./", "topic_dist.csv", sep="/"), sep=",")

# Compute the distance matrix using the topic distribution from the LDA model
lda.dist <- dist(topic.dist, method = "euclidean", diag = TRUE, upper = TRUE)
lda.matrix.dist <- as.matrix(lda.dist)
rownames(lda.matrix.dist) <- cuisine.names
colnames(lda.matrix.dist) <- cuisine.names
#write.matrix(format(lda.matrix.dist, scientific=FALSE), file = paste("./", "lda_matrix_dist.csv", sep="/"), sep=",")

# Convert the distance matrix to a similarity matrix
lda.simil <- as.simil(lda.dist)
lda.matrix.simil <- as.matrix(lda.simil, diag = 1)
rownames(lda.matrix.simil) <- cuisine.names
colnames(lda.matrix.simil) <- cuisine.names
#write.matrix(format(lda.matrix.simil, scientific=FALSE), file = paste("./", "lda_matrix_simil.csv", sep="/"), sep=",")
lda.scaled <- scale.mat(lda.matrix.simil)

# Plot the results for the LDA similartiy matrix
corrplot(lda.scaled, method = "circle", is.corr = FALSE, title = "LDA", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")

# ##############################################################################
# Task 2.3
# ##############################################################################

#########################
# K-Means Clustering
#########################
set.seed(2015)

# Perform K-Means clustering on term frequency similarity matrix 
km.tf <- kmeans(tf.matrix, centers = 10, algorithm = "MacQueen")
cluster <- km.tf$cluster
size <- km.tf$size
tf.clust <- cbind(tf.matrix, cluster)
tf.clust <- rbind(tf.clust, cluster)
by.cluster <- order(tf.clust[,"cluster"])
tf.clust <- tf.clust[by.cluster, by.cluster]
tf.clust <- tf.clust[!rownames(tf.clust) %in% "cluster", ]
tf.clust <- tf.clust[, !colnames(tf.clust) %in% "cluster"]
tf.clust.scaled <- scale.mat(tf.clust)

# Plot the results for k-means clustering of tf matrix
corrplot(tf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF with K-Means Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

km.tf <- kmeans(tf.matrix, centers = 25, algorithm = "MacQueen")
cluster <- km.tf$cluster
size <- km.tf$size
tf.clust <- cbind(tf.matrix, cluster)
tf.clust <- rbind(tf.clust, cluster)
by.cluster <- order(tf.clust[,"cluster"])
tf.clust <- tf.clust[by.cluster, by.cluster]
tf.clust <- tf.clust[!rownames(tf.clust) %in% "cluster", ]
tf.clust <- tf.clust[, !colnames(tf.clust) %in% "cluster"]
tf.clust.scaled <- scale.mat(tf.clust)

# Plot the results for k-means clustering of tf matrix
corrplot(tf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF with K-Means Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform K-Means clustering on tf-idf similarity matrix
km.tfidf <- kmeans(tfidf.matrix, centers = 10, algorithm = "MacQueen")
cluster <- km.tfidf$cluster
size <- km.tfidf$size
tfidf.clust <- cbind(tfidf.matrix, cluster)
tfidf.clust <- rbind(tfidf.clust, cluster)
by.cluster <- order(tfidf.clust[,"cluster"])
tfidf.clust <- tfidf.clust[by.cluster, by.cluster]
tfidf.clust <- tfidf.clust[!rownames(tfidf.clust) %in% "cluster", ]
tfidf.clust <- tfidf.clust[, !colnames(tfidf.clust) %in% "cluster"]
tfidf.clust.scaled <- scale.mat(tfidf.clust)

# Plot results for k-means clustering of tf-idf matrix
corrplot(tfidf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF-IDF with K-Means Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

km.tfidf <- kmeans(tfidf.matrix, centers = 25, algorithm = "MacQueen")
cluster <- km.tfidf$cluster
size <- km.tfidf$size
tfidf.clust <- cbind(tfidf.matrix, cluster)
tfidf.clust <- rbind(tfidf.clust, cluster)
by.cluster <- order(tfidf.clust[,"cluster"])
tfidf.clust <- tfidf.clust[by.cluster, by.cluster]
tfidf.clust <- tfidf.clust[!rownames(tfidf.clust) %in% "cluster", ]
tfidf.clust <- tfidf.clust[, !colnames(tfidf.clust) %in% "cluster"]
tfidf.clust.scaled <- scale.mat(tfidf.clust)

# Plot results for k-means clustering of tf-idf matrix
corrplot(tfidf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF-IDF with K-Means Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform K-Means clustering on the lda similarity matrix
km.lda <- kmeans(lda.matrix.simil, centers = 10, algorithm = "MacQueen")
cluster <- km.lda$cluster
size <- km.lda$size
lda.clust <- cbind(tfidf.matrix, cluster)
lda.clust <- rbind(lda.clust, cluster)
by.cluster <- order(lda.clust[,"cluster"])
lda.clust <- lda.clust[by.cluster, by.cluster]
lda.clust <- lda.clust[!rownames(lda.clust) %in% "cluster", ]
lda.clust <- lda.clust[, !colnames(lda.clust) %in% "cluster"]
lda.clust.scaled <- scale.mat(lda.clust)

# Plot results for k-means clustering of lda similarity matrix
corrplot(lda.clust.scaled, method = "circle", is.corr = FALSE, title = "LDA with K-Means Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform K-Means clustering on the lda similarity matrix 
km.lda <- kmeans(lda.matrix.simil, centers = 25, algorithm = "MacQueen")
cluster <- km.lda$cluster
size <- km.lda$size
lda.clust <- cbind(lda.matrix.simil, cluster)
lda.clust <- rbind(lda.clust, cluster)
by.cluster <- order(lda.clust[,"cluster"])
lda.clust <- lda.clust[by.cluster, by.cluster]
lda.clust <- lda.clust[!rownames(lda.clust) %in% "cluster", ]
lda.clust <- lda.clust[, !colnames(lda.clust) %in% "cluster"]
lda.clust.scaled <- scale.mat(lda.clust)

# Plot results for k-means clustering of lda similarity matrix
corrplot(lda.clust.scaled, method = "circle", is.corr = FALSE, title = "LDA with K-Means Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

#########################
# Hierarchical Clustering
#########################
set.seed(2015)
# Perform Hierarchical clustering on term frequency distance matrix 
tf.dist <- as.dist(tf.matrix)
hc.tf <- hclust(tf.dist, method = "average")
cluster <- cutree(hc.tf, k = 10)
size <- as.vector(table(cluster))
tf.clust <- cbind(tf.matrix, cluster)
tf.clust <- rbind(tf.clust, cluster)
by.cluster <- order(tf.clust[,"cluster"])
tf.clust <- tf.clust[by.cluster, by.cluster]
tf.clust <- tf.clust[!rownames(tf.clust) %in% "cluster", ]
tf.clust <- tf.clust[, !colnames(tf.clust) %in% "cluster"]
tf.clust.scaled <- scale.mat(tf.clust)

# Plot the results for hierarchical clustering of tf matrix
corrplot(tf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF with Hierarchical Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform Hierarchical clustering on term frequency distance matrix 
tf.dist <- as.dist(tf.matrix)
hc.tf <- hclust(tf.dist, method = "average")
cluster <- cutree(hc.tf, k = 25)
size <- as.vector(table(cluster))
tf.clust <- cbind(tf.matrix, cluster)
tf.clust <- rbind(tf.clust, cluster)
by.cluster <- order(tf.clust[,"cluster"])
tf.clust <- tf.clust[by.cluster, by.cluster]
tf.clust <- tf.clust[!rownames(tf.clust) %in% "cluster", ]
tf.clust <- tf.clust[, !colnames(tf.clust) %in% "cluster"]
tf.clust.scaled <- scale.mat(tf.clust)

# Plot the results for hierarchical clustering of tf matrix
corrplot(tf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF with Hierarchical Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform Hierarchical clustering on tf-idf distance matrix 
tfidf.dist <- as.dist(tfidf.matrix)
hc.tfidf <- hclust(tfidf.dist, method = "average")
cluster <- cutree(hc.tfidf, k = 10)
size <- as.vector(table(cluster))
tfidf.clust <- cbind(tfidf.matrix, cluster)
tfidf.clust <- rbind(tfidf.clust, cluster)
by.cluster <- order(tfidf.clust[,"cluster"])
tfidf.clust <- tfidf.clust[by.cluster, by.cluster]
tfidf.clust <- tfidf.clust[!rownames(tfidf.clust) %in% "cluster", ]
tfidf.clust <- tfidf.clust[, !colnames(tfidf.clust) %in% "cluster"]
tfidf.clust.scaled <- scale.mat(tfidf.clust)

# Plot the results for hierarchical clustering of tf-idf matrix
corrplot(tfidf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF-IDF with Hierarchical Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform Hierarchical clustering on tf-idf distance matrix 
tfidf.dist <- as.dist(tfidf.matrix)
hc.tfidf <- hclust(tfidf.dist, method = "average")
cluster <- cutree(hc.tfidf, k = 25)
size <- as.vector(table(cluster))
tfidf.clust <- cbind(tfidf.matrix, cluster)
tfidf.clust <- rbind(tfidf.clust, cluster)
by.cluster <- order(tfidf.clust[,"cluster"])
tfidf.clust <- tfidf.clust[by.cluster, by.cluster]
tfidf.clust <- tfidf.clust[!rownames(tfidf.clust) %in% "cluster", ]
tfidf.clust <- tfidf.clust[, !colnames(tfidf.clust) %in% "cluster"]
tfidf.clust.scaled <- scale.mat(tfidf.clust)

# Plot the results for hierarchical clustering of tf-idf matrix
corrplot(tfidf.clust.scaled, method = "circle", is.corr = FALSE, title = "TF-IDF with Hierarchical Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform Heirarchical clustering on term lda distance matrix
hc.lda <- hclust(lda.dist, method = "average")
cluster <- cutree(hc.lda, k = 10)
size <- as.vector(table(cluster))
lda.clust <- cbind(lda.matrix.simil, cluster)
lda.clust <- rbind(lda.clust, cluster)
by.cluster <- order(lda.clust[,"cluster"])
lda.clust <- lda.clust[by.cluster, by.cluster]
lda.clust <- lda.clust[!rownames(lda.clust) %in% "cluster", ]
lda.clust <- lda.clust[, !colnames(lda.clust) %in% "cluster"]
lda.clust.scaled <- scale.mat(lda.clust)

# Plot results for heirarchical clustering of lda similarity matrix
corrplot(lda.clust.scaled, method = "circle", is.corr = FALSE, title = "LDA with Hierarchical Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)

# Perform Heirarchical clustering on lda distance matrix
hc.lda <- hclust(lda.dist, method = "average")
cluster <- cutree(hc.lda, k = 25)
size <- as.vector(table(cluster))
lda.clust <- cbind(lda.matrix.simil, cluster)
lda.clust <- rbind(lda.clust, cluster)
by.cluster <- order(lda.clust[,"cluster"])
lda.clust <- lda.clust[by.cluster, by.cluster]
lda.clust <- lda.clust[!rownames(lda.clust) %in% "cluster", ]
lda.clust <- lda.clust[, !colnames(lda.clust) %in% "cluster"]
lda.clust.scaled <- scale.mat(lda.clust)

# Plot results for heirarchical clustering of lda similarity matrix
corrplot(lda.clust.scaled, method = "circle", is.corr = FALSE, title = "LDA with Hierarchical Clustering", cl.lim = c(0, 1), tl.cex = 0.5, tl.col = "black")
corrRect(size)
