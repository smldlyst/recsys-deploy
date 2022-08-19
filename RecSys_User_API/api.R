# This is a Plumber API
library(data.table)
library(recommenderlab)
library(plumber)

#* @apiTitle 여행메이트 추천시스템 API
#* @apiDescription 
 
#* @param spot_i 이전에 가보았던 관광지 중, 평가를 내릴 관광지명을 입력하세요. {예 : 광안리해수욕장}
#* @param rating_i 위 입력한 관광지에 대하여, 만족도를 평가하세요. {예: 1 ~ 5} (1: 매우 불만족 / 2: 약간 불만족 / 3: 보통 / 4: 만족 / 5: 매우 만족)

#* @param spot_j 이전에 가보았던 관광지 중, 평가를 내릴 관광지명을 입력하세요. {예 : 감천문화마을}
#* @param rating_j 위 입력한 관광지에 대하여, 만족도를 평가하세요. {예: 1 ~ 5} (1: 매우 불만족 / 2: 약간 불만족 / 3: 보통 / 4: 만족 / 5: 매우 만족)

#* @param spot_k 이전에 가보았던 관광지 중, 평가를 내릴 관광지명을 입력하세요. {예 : 청사포}
#* @param rating_k 위 입력한 관광지에 대하여, 만족도를 평가하세요. {예: 1 ~ 5} (1: 매우 불만족 / 2: 약간 불만족 / 3: 보통 / 4: 만족 / 5: 매우 만족)

#* @post /predict
function(spot_i, rating_i, spot_j, rating_j, spot_k, rating_k){
    load("./rating_matrix.RData")
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