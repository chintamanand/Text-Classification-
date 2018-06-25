
#install.packages("pander")
library(pander)
library(dplyr)
setwd("F:/4thyear/Wine Quality")
data1<-read.csv("toyamazonPhilips.csv", header=TRUE)
pandoc.table(data1[2:4,1:3], justify = c('left', 'left', 'center'), style = 'grid')
pandoc.table()
table(data1$rating)
data1 <- data1 %>% filter(rating != 3) %>% mutate(rating_new = if_else(rating >= 4, 1, 0))

# We create a training set
product_review_training <-  data1[1:150, ]

library(tm)
corpus_toy <- Corpus(VectorSource(product_review_training$review))
tdm_toy <- DocumentTermMatrix(corpus_toy, list(removePunctuation = TRUE, 
                                               removeNumbers = TRUE))

training_set_toy <- as.matrix(tdm_toy)

training_set_toy <- cbind(training_set_toy, product_review_training$rating_new)

colnames(training_set_toy)[ncol(training_set_toy)] <- "y"

training_set_toy <- as.data.frame(training_set_toy)
training_set_toy$y <- as.factor(training_set_toy$y)

library(caret)
review_toy_model <- train(y ~., data = training_set_toy, method = 'svmLinear3')

test_review_data <- data1[151:191, ]

test_corpus <- Corpus(VectorSource(test_review_data$review))
test_tdm <- DocumentTermMatrix(test_corpus, control=list(dictionary = Terms(tdm_toy)))
test_tdm <- as.matrix(test_tdm)

#Build the prediction  
model_toy_result <- predict(review_toy_model, newdata = test_tdm)

check_accuracy <- as.data.frame(cbind(prediction = model_toy_result, rating = test_review_data$rating_new))

check_accuracy <- check_accuracy %>% mutate(prediction = as.integer(prediction) - 1)

check_accuracy$accuracy <- if_else(check_accuracy$prediction == check_accuracy$rating, 1, 0)
round(prop.table(table(check_accuracy$accuracy)), 3)
