library(topicmodels)
library(tm)
library(slam)
library(lsa)

# Import the reviews for each cuisine

# Create the document corpus

# Create the term-document matrix
x <- matrix(c(5,0,3,0,2,0,0,2,0,0,3,0,2,0,1,1,0,1,0,1), ncol = 2, byrow = FALSE)
print(x)
#dtm <- as.simple_triplet_matrix(x)
dtm <- as.TermDocumentMatrix(x, weighting = weightTf)


# Compute the cosine similarity matrix from the document term matrix
cosine_dist_mat <- crossprod_simple_triplet_matrix(dtm)/(sqrt(col_sums(dtm^2) %*% t(col_sums(dtm^2))))
