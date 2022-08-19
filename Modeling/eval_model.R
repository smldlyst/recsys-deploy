setwd("D:/Project/Hackathon/")
library(data.table)
library(tidyverse)
library(magrittr)
library(recommenderlab)

data <- fread("./Data/BC_BS_TOUR_TRRSRT_MDORDER_INFO_202107.csv")
glimpse(data)

## create a 1-5 rating matrix
set.seed(2021)
full_matrix <- matrix(sample(c(NA,1,2,3,4,5), 63000,
                             replace=TRUE,
                             # prob=c(0.2, 0.05, 0.1, 0.18, 0.25, 0.22))# 80% 평가
                             # prob=c(0.4, 0.03, 0.07, 0.14, 0.2, 0.16)) # 60% 평가 (Robust)
                             # prob=c(0.5, 0.03, 0.06, 0.12, 0.15, 0.14))# 50% 평가 (normal)
                             # prob=c(0.6, 0.02, 0.04, 0.08, 0.14, 0.12))# 40% 평가 (normal)
                             prob=c(0.7, 0.01, 0.02, 0.07, 0.11, 0.09)) # 30% 평가 (Good)
                             # prob=c(0.8, 0.01, 0.02, 0.04, 0.07, 0.06))# 20% 평가 (Basic)
                             # prob=c(0.9, 0.005, 0.01, 0.02, 0.04, 0.025))# 10% 평가 (Weak)
                             # prob=c(0.95, 0.003, 0.006, 0.012, 0.02, 0.014))# 5% 평가 (Bad)
                      ,
                      nrow=1000, ncol=63,
                      dimnames=list(users=paste("user_", 1:1000, sep=''),
                                    items=paste(data$PLACE_NM, sep='')))

# head(full_matrix)
rating_matrix <- as(full_matrix, "realRatingMatrix")


## EDA
vector_ratings <- as.vector(rating_matrix@data)
table_ratings <- table(vector_ratings)
table_ratings 

vector_ratings <- vector_ratings[vector_ratings != 0]
vector_ratings <- factor(vector_ratings)
qplot(vector_ratings) + ggtitle("평점별 분포") + xlab("관광지 만족도")

## evalutate model using split method
e <- evaluationScheme(rating_matrix, method="split", train=0.7, given=-1)

rating_matrix
getData(e, "train")
getData(e, "known")
getData(e, "unknown")

model_ubcf <- Recommender(getData(e, "train"), method = "UBCF", 
                          param=list(normalize = "center", method="Cosine"))
pred_ubcf <- predict(model_ubcf, getData(e, "known"), type="ratings")
metric_ubcf <- calcPredictionAccuracy(pred_ubcf, getData(e, "unknown"))

model_ibcf <- Recommender(getData(e, "train"), method = "IBCF", 
                     param=list(normalize = "center", method="Cosine"))
pred_ibcf <- predict(model_ibcf, getData(e, "known"), type="ratings")
metric_ibcf <- calcPredictionAccuracy(pred_ibcf, getData(e, "unknown"))

model_svd <- Recommender(getData(e, "train"), method = "SVD")
pred_svd <- predict(model_svd, getData(e, "known"), type="ratings")
metric_svd <- calcPredictionAccuracy(pred_svd, getData(e, "unknown"))

model_random <- Recommender(getData(e, "train"), method = "RANDOM")
pred_random <- predict(model_random, getData(e, "known"), type="ratings")
metric_random <- calcPredictionAccuracy(pred_random, getData(e, "unknown"))

result <- rbind(metric_random, metric_ubcf, metric_ibcf, metric_svd)
round(result, 2)
