# Intent Classification for chatbot

setwd("C://Users/Administrator/Desktop")

script <- read.csv("text.csv")

install.packages("tm")

library(tm)

head(script)




# create a corpus from character vectors

chat<-VCorpus(VectorSource(script$sentence))

summary(script$intent)



# main intent에 대한 subset 구성

data_part_request <- subset(script, script$intent=="Part request") ## 70 obs.

data_ask_part_position <- subset(script, script$intent=="ask part position") ## 44 obs.

data_describe_the_parts <- subset(script, script$intent=="describe the parts") ## 78 obs.

data_check_part_position <- subset(script, script$intent=="check part position") ## 25 obs.

data_part_number <- subset(script, script$intent=="part #") ## 67 obs.

data_ask_location <- subset(script, script$intent=="ask visitor's location") ## 44 obs.

data_location <- subset(script, script$intent=="location") ## 70 obs.

data_distributor_info <- subset(script, script$intent=="part distributor's info") ## 77 obs.

data_thank_you <- subset(script, script$intent=="thank you") ## 84 obs.

data_check_additional_request <- subset(script, script$intent=="check additional request") ## 35 obs.

data_no_additional_request <- subset(script, script$intent=="no additional request") ## 40 obs.

data_closing <- subset(script, script$intent=="closing") ## 121 obs.





# paste each intent train set

part.request<-subset(script, script$intent=="Part request")

part.request.script<-paste0(part.request$sentence,collapse = " ")

part.request.script

ask.part.position<-subset(script, script$intent=="ask part position")

ask.part.position.script<-paste0(ask.part.position$sentence,collapse = " ")

ask.part.position.script

describe.the.parts<-subset(script, script$intent=="describe the parts")

describe.the.parts.script<-paste0(describe.the.parts$sentence,collapse = " ")

describe.the.parts.script

check.part.position<-subset(script, script$intent=="check part position")

check.part.position.script<-paste0(check.part.position$sentence,collapse = " ")

check.part.position.script

part.number<-subset(script, script$intent=="part #")

part.number.script<-paste0(part.number$sentence,collapse = " ")

part.number.script

ask.location<-subset(script, script$intent=="ask visitor's location")

ask.location.script<-paste0(ask.location$sentence,collapse = " ")

ask.location.script

location<-subset(script, script$intent=="location")

location.script<-paste0(location$sentence,collapse = " ")

location.script

distributor.info<-subset(script, script$intent=="part distributor's info")

distributor.info.script<-paste0(distributor.info$sentence,collapse = " ")

distributor.info.script

thankyou<-subset(script, script$intent=="thank you")

thankyou.script<-paste0(thankyou$sentence,collapse = " ")

thankyou.script

check.additional.request<-subset(script, script$intent=="check additional request")

check.additional.request.script<-paste0(check.additional.request$sentence,collapse = " ")

check.additional.request.script

no.additional.request<-subset(script, script$intent=="no additional request")

no.additional.request.script<-paste0(no.additional.request$sentence,collapse = " ")

no.additional.request.script

closing<-subset(script, script$intent=="closing")

closing.script<-paste0(closing$sentence,collapse = " ")

closing.script


# setup for matrix - intent merge

intent.list<-list(part.request.script, ask.part.position.script, describe.the.parts.script,

                  check.part.position.script, part.number.script, ask.location.script,
                  
                  location.script, distributor.info.script, thankyou.script,
                  
                  check.additional.request.script, no.additional.request.script,
                  
                  closing.script)

N.intents<-length(intent.list)

N.intents

names(intent.list) <- paste0("intent", c(1:N.intents))

train_total<- c(intent.list)




# query : mention to check intent

query <- " your welcome. is it located on the left or right door?  "




# To vector space

my.docs <- VectorSource(c(intent.list, query))

my.docs$Names <- c(names(intent.list), "query")

my.corpus <- Corpus(my.docs)

my.corpus

inspect(my.corpus)



# stemming / TDM

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)

colnames(term.doc.matrix.stm) <- c(names(intent.list), "query")

inspect(term.doc.matrix.stm[0:30, ])

term.doc.matrix <- as.matrix(term.doc.matrix.stm)



# get tf-idf weights

get.tf.idf.weights <- function(tf.vec) {

    # Computes tfidf weights from term frequency vector
  
  n.docs <- length(tf.vec)
  
  doc.frequency <- length(tf.vec[tf.vec > 0])
  
  weights <- rep(0, length(tf.vec))
  
  weights[tf.vec > 0] <- (1 + log2(tf.vec[tf.vec > 0])) * log2(n.docs/doc.frequency)
  
  return(weights)

  }


# tf-idf matrix

tfidf.matrix <- t(apply(term.doc.matrix, 1, 
                        
                        FUN = function(row) {get.tf.idf.weights(row)}))

colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix[0:3, ]


# Normalization

tfidf.matrix <- scale(tfidf.matrix, center = FALSE,
               
                             scale = sqrt(colSums(tfidf.matrix^2)))

tfidf.matrix[0:3, ]



# dot product

query.vector <- tfidf.matrix[, (N.intents + 1)]

tfidf.matrix <- tfidf.matrix[, 1:N.intents]

doc.scores <- t(query.vector) %*% tfidf.matrix



# rank the documents

results.df <- data.frame(doc = names(intent.list), score = t(doc.scores),

                                                  text = unlist(intent.list))

results.df <- results.df[order(results.df$score, decreasing = TRUE), ]



# the results

options(width=2000)

print(results.df, row.names = FALSE, right = FALSE, digits= 2)

print(results.df)

print(results.df[1, 1:2])

write.csv(results.df, "result_0320(1).csv")


########################################################################################


## reputation - input query

valid<- read.csv("validation_set.csv", header=FALSE)

valid2<-read.csv("validation_set_2.csv", header=FALSE)

str(valid)

print(valid[2,1])

str(valid[2,1])

num<-length(valid[,1]) # validation set = (num x 1)

num

valid[3,1]

valid<-valid[,-2]

print(query)

query<-as.character(valid[i,1])

head(valid)

valid[100,1]

query<-as.character(valid[1,1])

str(query)



# test set

### 0320 test

for(i in 1:num){

    query<-as.character(valid2[i,1])
  
    my.docs <- VectorSource(c(intent.list, query))
  
    my.docs$Names <- c(names(intent.list), "query")
  
    my.corpus <- Corpus(my.docs)

    term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)

    colnames(term.doc.matrix.stm) <- c(names(intent.list), "query")
  
    term.doc.matrix <- as.matrix(term.doc.matrix.stm)
  

    tfidf.matrix <- t(apply(term.doc.matrix, 1,

                            FUN = function(row) {get.tf.idf.weights(row)}))
  
    colnames(tfidf.matrix) <- colnames(term.doc.matrix)
  
    tfidf.matrix <- scale(tfidf.matrix, center = FALSE,
                        
                          scale = sqrt(colSums(tfidf.matrix^2)))
  
  query.vector <- tfidf.matrix[, (N.intents + 1)]
  
  tfidf.matrix <- tfidf.matrix[, 1:N.intents]
  
  doc.scores <- t(query.vector) %*% tfidf.matrix
  
  results.df <- data.frame(doc = names(intent.list), score = t(doc.scores),
  
                                                    text = unlist(intent.list))
  
  results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
  
  print(results.df[,1:2])
  
  valid2[i,2]<-results.df[1,1]

  }


write.csv(valid2, "validation2_0320.csv")


 

#######################################################################################

# test - input query


query <- "i am sorry to hear about this. it would be best if our certified technician could examine your unit. may i know when you purchased it? "

my.docs <- VectorSource(c(intent.list, query))

my.corpus <- Corpus(my.docs)

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)

colnames(term.doc.matrix.stm) <- c(names(intent.list), "query")

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

tfidf.matrix <- t(apply(term.doc.matrix, 1,

                                                FUN = function(row) {get.tf.idf.weights(row)}))

colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix <- scale(tfidf.matrix, center = FALSE,

                                            scale = sqrt(colSums(tfidf.matrix^2)))

query.vector <- tfidf.matrix[, (N.intents + 1)]

tfidf.matrix <- tfidf.matrix[, 1:N.intents]

doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = names(intent.list), score = t(doc.scores),

                                                  text = unlist(intent.list))

results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

valid[2,2] <- results.df[1,1]



####################################################################################


# 0320 svm model try

install.packages("RTextTools")

library(RTextTools)

train<-read.csv("trainset.csv")

dtMatrix <- create_matrix(train["script"])

dtMatrix

container <- create_container(dtMatrix, train$X, trainSize=1:12, virgin=FALSE)

model <- train_model(container, "SVM", kernel="linear", cost=1)

predictionData<- valid2

predMatrix<-create_matrix(predictionData, originalMatrix = dtMatrix)

predSize = length(predictionData)

predictionContainer <-create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)











