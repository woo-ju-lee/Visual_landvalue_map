#package_load

library(httr)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(readr)
library(deckgl)
library(RColorBrewer)
library(htmlwidgets)
library(formattable)

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
    type = "parcel",
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

table_maker <- function(data) {
  map(1:nrow(data), function(i) {
    geo_maker(data$법정동명[i])
  }) %>%
    bind_rows() %>% 
    bind_cols(mean = data$mean, geo = data$법정동명, mean_chr = comma(data$mean, format = "d")) %>% 
    select(geo, mean, x, y, mean_chr)
}


#main_code

seoul <- csv_reader("~/Visual_landvalue_map/data/seoul.csv")
chungnam <- csv_reader("~/Visual_landvalue_map/data/chungnam.csv")

seoul_raw <- mean_reader(seoul)
chungnam_raw <- mean_reader(chungnam)

seoul_main <- table_maker(seoul_raw)
chungnam_main <- table_maker(chungnam_raw)

seoul_main[,2:4] <- apply(seoul_main[,2:4], 2, as.numeric) %>% as.data.frame()
seoul_main$mean_chr <- as.character(seoul_main$mean_chr)

deck <- deckgl(
  longitude = mean(seoul_main$x),
  latitude = mean(seoul_main$y),
  zoom = 11,
  pitch = 45,
  width     = "100vw",
  height    = "100vh",
  sizingPolicy = sizingPolicy(
    browser.fill = TRUE,
    viewer.fill  = TRUE
  )
) %>% 
  add_basemap() %>% 
  add_hexagon_layer(
    data = seoul_main,
    id = "apt-price",
    properties = list(
      extruded             = TRUE,
      radius               = 10,
      elevationScale       = 50,
      getPosition          = ~c(x, y),
      getElevationWeight   = ~mean,
      elevationAggregation = "MEAN",
      getColorWeight       = ~mean,
      colorAggregation     = "MEAN",
      colorRange           = brewer.pal(6, "YlOrRd"),
      opacity              = 0.7,
      autoHighlight        = TRUE,
      tooltip = "
      <div style='
           font-family: \"Nanum Gothic\", sans-serif;
           font-size: 14px;
           line-height: 1.3;
           color: #FFFFFF;
         '>
        {{#points}}
          <div style='
               font-size: 18px;
               font-weight: bold;
             '>{{geo}}</div>
        {{/points}}
        {{#points}}
          <div style='
               font-size: 14px;
             '>가격: {{mean_chr}}원</div>
        {{/points}}
      </div>
      "
    )
  )

deck