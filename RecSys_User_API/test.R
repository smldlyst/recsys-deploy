library(httr)
library(jsonlite)

url = "http://127.0.0.1:8080/predict?spot_i=%EA%B4%91%EC%95%88%EB%A6%AC%ED%95%B4%EC%88%98%EC%9A%95%EC%9E%A5&rating_i=4&spot_j=%EA%B0%90%EC%B2%9C%EB%AC%B8%ED%99%94%EB%A7%88%EC%9D%84&rating_j=2&spot_k=%EC%B2%AD%EC%82%AC%ED%8F%AC&rating_k=4"
response = POST(url)
fromJSON(content(response, type = "text", encoding = "utf-8"))



