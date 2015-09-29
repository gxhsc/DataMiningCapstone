library(topicmodels)
library(NLP)
library(tm)
library(jsonlite)
library(slam)
library(MASS)
library(igraph)
library(R.utils)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array of scores back, so we use
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence = tolower(sentence)
        
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
        
        return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

business.file <- './yelp_academic_dataset_business.json'
business <- stream_in(file(business_file))

review.file <- './yelp_academic_dataset_review.json'
review <- stream_in(file(review_file))

# Get Chinese-ish restaurant ids
is.chinese <- grepl("Chinese.*Restaurants",business$categories)
is.asianfus <- grepl("Asian Fusion.*Restaurants",business$categories)
is.dimsum <- grepl("Dim Sum.*Restaurants",business$categories)
is.cantonese <- grepl("Cantonese.*Restaurants",business$categories)
is.chineseish <- is.chinese | is.asianfus | is.dimsum | is.cantonese
restaurant.id <- business$business_id[is.chineseish]

# Select those reviews that are for restaurants
chinese.review <- review[review$business_id %in% restaurant.id,]
# Extract just the review text and restaurant ranking (stars)
chinese.review <- subset(chinese.review, select =c("stars", "text"))
chinese.review$text <- tolower(chinese.review$text)

hu.liu.pos = scan('data/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('data/negative-words.txt', what='character', comment.char=';')
pos.words <- c(hu.liu.pos, "wow", "authentic", "flavorful", "tasty", "gem","yum")
neg.words <- c(hu.liu.neg, "flavorless", "poisoning", "mushy", "yuck", "dry")

#sample.review <- chinese.review[sample(nrow(chinese.review), 30), , drop = FALSE]
result = score.sentiment(chinese.review$text, pos.words, neg.words)
sent.score <- result$score
chinese.review <- cbind(chinese.review, sent.score)
#sample.review <- cbind(sample.review, lui.sentiment)
#sample.review$text <- gsub("\n", " ", sample.review$text)
#write(sample.review$text, "data/chinese_sample.txt", sep = "")

#x <- readLines("data/chinese_sample.txt")

#rntn.sentiment <- read.delim("data/RDMsent.txt", header = FALSE, sep = "\t", quote = "")
#rntn.sentiment <- rntn.sentiment$V3
#sample.review <- cbind(sample.review, rntn.sentiment)

# Read dish terms from Task 3
dish.terms <- read.delim("Chinese_terms.txt", header = TRUE, sep = "\t")
dish.terms <- dish.terms$Chinese

# Remove duplicate dish names
dish.terms <- as.vector(unique(dish.terms))

# Read annotated terms provide with Task 4
student.terms <- readLines("data/student_dn_annotations.txt")

# Extract the common dish terms between dish terms and student annotated Chinese terms
dish.terms <- c(dish.terms, student.terms)
dish.terms <- dish.terms[duplicated(dish.terms)]

dish.metrics <- data.frame(term=dish.terms, frequency = 0, sentiment = 0)
for (i in 1:length(dish.terms)) { 
    term <- dish.terms[i]
    matches <- grep(term, chinese.review$text)
    dish.metrics[i,"frequency"] <- length(matches)
    if (length(matches) > 0) dish.metrics[i,"sentiment"] <- sum(chinese.review$sent.score[matches])
}    
# Calculate the mean sentiment score and add it to metrics data frame
avg.sent <- dish.metrics$sentiment / dish.metrics$frequency
dish.metrics <- cbind(dish.metrics, avg.sent)

# Sort the results by term frequency

# Plot the results
    

