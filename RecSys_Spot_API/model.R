setwd("D:/Project/Hackathon/")
library(data.table)
library(tidyverse)
library(magrittr)
library(recommenderlab)

data <- fread("./Data/BC_BS_TOUR_TRRSRT_MDORDER_INFO_202107_2.csv", encoding="UTF-8")
glimpse(data)

## create a 1-5 rating matrix
set.seed(2021)
full_matrix <- matrix(sample(c(NA,1,2,3,4,5), 46000,
                             replace=TRUE, prob=c(0.4, 0.03, 0.07, 0.14, 0.2, 0.16)),
                      nrow=1000, ncol=46,
                      dimnames=list(users=paste("user_", 1:1000, sep=''),
                                    items=paste(data$PLACE_NM, sep='')))
full_matrix

## Recommendation
rating_matrix <- as(full_matrix, "realRatingMatrix")
recc_model <- Recommender(data = rating_matrix, method ="UBCF")
recc_model

new_matrix <- matrix(NA, nrow = 1, ncol = nrow(data))
colnames(new_matrix) <- data$PLACE_NM
new_matrix
new_matrix[,"감천문화마을"] <- 5
new_matrix[,"광안리해수욕장"] <- 4
new_matrix[,"청사포"] <- 4

new_matrix <- as(new_matrix, "realRatingMatrix")

recc_predicted <- predict(object = recc_model,
                          newdata = new_matrix, n = 20)

data.frame("추천된 관광지"= getList(recc_predicted)[[1]],
           "추천 평점" = getRatings(recc_predicted)[[1]])


# save(recc_model, file = "D:/Project/Hackathon/RecSys_2/model.RData")

### make recommendations
#### 예시
RecSys <-
  function(spot_i, rating_i, spot_j, rating_j, spot_k, rating_k){
    load(file = "D:/Project/Hackathon/RecSys_2/model.RData")
    data <- fread("BC_BS_TOUR_TRRSRT_MDORDER_INFO_202107_2.csv", encoding="UTF-8")
    new_matrix <- matrix(NA, nrow = 1, ncol = nrow(data))
    colnames(new_matrix) <- data$PLACE_NM
    new_matrix[,spot_i] <- as.numeric(rating_i)
    new_matrix[,spot_j] <- as.numeric(rating_j)
    new_matrix[,spot_k] <- as.numeric(rating_k)
    new_matrix <- as(new_matrix, "realRatingMatrix")
    
    recc_predicted <- predict(object = recc_model,
                              newdata = new_matrix, n = 20)
    cbind('추천된 관광지' = getList(recc_predicted)[[1]],
          '추천 평점 예측값' = sprintf("%1.1f", getRatings(recc_predicted)[[1]]))
    
    
  }

RecSys("광안리해수욕장", 4, "감천문화마을", 5, "청사포", 4)
