library(data.table)
library(tidyverse)
library(magrittr)
library(recommenderlab)

setwd("D:/Project/Hackathon/User_RecSys_API")
data <- fread("BC_BS_TOUR_TRRSRT_MDORDER_INFO_202107_2.csv", encoding="UTF-8")
glimpse(data)

# create a 1-5 rating matrix
set.seed(2021)
full_matrix <- matrix(sample(c(NA,1,2,3,4,5), 46000,
                             replace=TRUE, prob=c(0.4, # 미평가 비율
                                                  0.05, 0.05, 0.2, 0.1, 0.1)),
                      nrow=1000, ncol=46,
                      dimnames=list(users=paste("user_", 1:1000, sep=''),
                                    items=paste(data$PLACE_NM, sep='')))

# Recommendation
rating_matrix <- as(full_matrix, "realRatingMatrix")
# save(rating_matrix, file = "D:/Project/Hackathon/User_Recommender_API/rating_matrix.RData")

## Tour Mate Recommender
RecSys_Tourmate <-
  function(spot_i, rating_i, spot_j, rating_j, spot_k, rating_k){
    load("D:/Project/Hackathon/User_Recommender_API/rating_matrix.RData")
    data <- fread("BC_BS_TOUR_TRRSRT_MDORDER_INFO_202107_2.csv", encoding="UTF-8")
    
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

RecSys_Tourmate("광안리해수욕장", 5, "흰여울문화마을", 4, "청사포", 3)
