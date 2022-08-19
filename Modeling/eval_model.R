setwd("D:/Project/Hackathon/RecSys")
library(data.table)
library(tidyverse)
library(magrittr)
library(recommenderlab)

data <- fread("BC_BS_TOUR_TRRSRT_MDORDER_INFO_202107.csv")
glimpse(data)

## create a 1-5 rating matrix
set.seed(2021)
full_matrix <- matrix(sample(c(NA,1,2,3,4,5), 63000,
                             replace=TRUE,
                             prob=c(0.4, 0.03, 0.07, 0.14, 0.2, 0.16))
                             # prob=c(0.6, 0.02, 0.04, 0.10, 0.13, 0.11))
                             ,
                      nrow=1000, ncol=63,
                      dimnames=list(users=paste("user_", 1:1000, sep=''),
                                    items=paste(data$PLACE_NM, sep='')))


# head(full_matrix)
rating_matrix <- as(full_matrix, "realRatingMatrix")
rating_matrix


## EDA
# vector_ratings <- as.vector(rating_matrix@data)
# table_ratings <- table(vector_ratings)

# vector_ratings <- vector_ratings[vector_ratings != 0]
# vector_ratings <- factor(vector_ratings)
# qplot(vector_ratings) + ggtitle("평점별 분포") + xlab("관광지 만족도")

## [Evaluating recommender techniques]
n_fold <- 10
items_to_keep <- 15 # 추천 생성에 사용되는 아이템의 최소 개수
rating_threshold <- 3
eval_sets <- evaluationScheme(data = rating_matrix,
                              method = "cross-validation",
                              train = 0.7,
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

# rating_matrix
# eval_sets@knownData
# eval_sets@unknownData
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

# [Identifying the most suitable model]
# Comparing models
models_to_evaluate <- list(
  random = list(name = "RANDOM", param=NULL),
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  SVD = list(name="SVD")
)

n_recommendations <- c(1, 5, seq(10, 60, 10)) # item의 수 변화에 따른 성능 확인

list_results <- evaluate(x = eval_sets, method = models_to_evaluate,
                         n = n_recommendations)

avg_matrices <- lapply(list_results, avg)
avg_matrices

# Identifying the most suitable model
plot(list_results, annotate = 1, legend = "topleft") + title("ROC curve")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") + title("Precision-recall")

## Tour Spot Recommendation
# recc_model <- Recommender(data = rating_matrix, method ="UBCF")
# new_matrix <- matrix(NA, nrow = 1, ncol = nrow(data))
# colnames(new_matrix) <- data$PLACE_NM
# 
# new_matrix
# new_matrix[,"흰여울마을"] <- 5
# new_matrix[,"감천문화마을"] <- 4
# new_matrix[,"민락수변공원"] <- 4
# 
# new_matrix <- as(new_matrix, "realRatingMatrix")
# 
# recc_predicted <- predict(object = recc_model,
#                           newdata = new_matrix, n = 20)
# 
# data.frame("추천된 관광지"= getList(recc_predicted)[[1]],
#            "추천 평점" = getRatings(recc_predicted)[[1]])