#package_load

library(httr)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(readr)

#function

csv_reader <- function(csv) {
  read_csv(csv, locale = locale(encoding = "euc-kr"))
}

mean_reader <- function(reader) {
  reader %>% 
    group_by(법정동명) %>% 
    summarise(mean = mean(공시지가))
}

geo_maker <- function(geo_code) {
  url <- "https://api.vworld.kr/req/address"
  params <- list(
    service = "address",
    request = "getcoord",
    version = "2.0",
    address = geo_code,
    refine = "true",
    simple = "false",
    format = "json",
    type = "road",
    key = Sys.getenv("MY_API_KEY")
  ) 
  
  full_url <- paste0(url, "?service=", params$service,
                     "&request=", params$request,
                     "&version=", params$version,
                     "&address=", curlEscape(params$address),
                     "&refine=", params$refine,
                     "&simple=", params$simple,
                     "&format=", params$format,
                     "&type=", params$type,
                     "&key=", params$key
  )
  
  response <- GET(full_url)
  content <- fromJSON(content(response, "text"))
  content <- content$response$result$point %>% as.data.frame()
  return(content)
}

table_maker <- function(data, raw_data) {
  map(1:nrow(data), function(i) {
    geo_maker(data$법정동명[i])
  }) %>%
    bind_rows() %>% 
    bind_cols(raw_data$mean) %>% 
    select(mean, x, y)
}


#main_code
seoul <- csv_reader("~/Visual_landvalue_map/data/seoul.csv")
chungnam <- csv_reader("~/Visual_landvalue_map/data/chungnam.csv")

seoul_raw <- mean_reader(seoul)
chungnam_raw <- mean_reader(chungnam)

seoul_main <- table_maker(seoul, seoul_raw)
chungnam_main <- table_maker(chungnam, chungnam_raw)