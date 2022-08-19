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
                             prob=c(0.4, 0.03, 0.07, 0.14, 0.2, 0.16))
                      # prob=c(0.6, 0.02, 0.04, 0.10, 0.13, 0.11))
                      ,
                      nrow=1000, ncol=63,
                      dimnames=list(users=paste("user_", 1:1000, sep=''),
                                    items=paste(data$PLACE_NM, sep='')))

# head(full_matrix)
rating_matrix <- as(full_matrix, "realRatingMatrix")


### Tour Mate Recommender
new_matrix <- matrix(NA, nrow = 1, ncol = nrow(data))
colnames(new_matrix) <- data$PLACE_NM
new_matrix
new_matrix[,"감천문화마을"] <- 5
new_matrix[,"광안리해수욕장"] <- 5
new_matrix[,"일광해수욕장"] <- 3
new_matrix[,"청사포"] <- 4
new_matrix[,"송도해수욕장"] <- 3

new_matrix <- as(new_matrix, "realRatingMatrix")

## 유사도 확인 
# Calculate Similarity
# similarity matrix (예시)
# similarity_users <- similarity(rating_matrix[1:1000,], method =  "cosine", which = "users")
# image(as.matrix(similarity_users), main = "User similarity")

similarity_users <- similarity(rating_matrix, new_matrix, method="cosine", which = "users") # 새로운 사용자의 평점과 기존 사용자들의 평점간 유사도 계산
user_sim_mat <- as.matrix(similarity_users )
# image(user_sim_mat, main = "User similarity")
index <- which(user_sim_mat %in% sort(user_sim_mat, decreasing=TRUE)[1:10])[1:10] # 유사도가 높은 사용자 순으로 내림차순 정렬
rownames(user_sim_mat)[index][1:10] # 유사도 높은 사용자 이름 출력

# raitng_matrix_topN <- as(rating_matrix@data[index,], "realRatingMatrix") # 유사도 높았던 데이터 가져와서 
# topN_new_sim <- similarity(raitng_matrix_topN, new_matrix, method="cosine", which = "users") # new_mat와 유사도 계산
# image(topN_new_sim, main = "User similarity") # 완전 유사함
# save(rating_matrix, file = "D:/Project/Hackathon/RecSys/rating_matrix.RData")

## Tour Mate Recommender
setwd("D:/Project/Hackathon/RecSys")
Rec_Tourmate <-
  function(spot_i, rating_i, spot_j, rating_j, spot_k, rating_k){
    load("D:/Project/Hackathon/RecSys/rating_matrix.RData")
    data <- fread("BC_BS_TOUR_TRRSRT_MDORDER_INFO_202107.csv")
    new_matrix <- matrix(NA, nrow = 1, ncol = nrow(data))
    colnames(new_matrix) <- data$PLACE_NM
    new_matrix[,spot_i] <- as.numeric(rating_i)
    new_matrix[,spot_j] <- as.numeric(rating_j)
    new_matrix[,spot_k] <- as.numeric(rating_k)
    new_matrix <- as(new_matrix, "realRatingMatrix")
    
    similarity_users <- similarity(rating_matrix, new_matrix, method="cosine", which = "users")
    user_sim_mat <- as.matrix(similarity_users)
    index <- which(user_sim_mat %in% sort(user_sim_mat, decreasing=TRUE)[1:10]) # 유사도가 높은 사용자 순으로 내림차순 정렬
    rownames(user_sim_mat)[index][1:10] # 유사도 높은 사용자 이름 출력
  }

Rec_Tourmate("광안리해수욕장", 5, "흰여울마을", 4, "청사포", 3)


similarity(rating_matrix, new_matrix, method="cosine", which = "users") # 새로운 사용자의 평점과 기존 사용자들의 평점간 유사도 계산

# unique(data$PLACE_NM)