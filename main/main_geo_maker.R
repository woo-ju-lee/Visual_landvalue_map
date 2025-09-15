#package_load

library(httr)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(readr)
library(formattable)
library(tools)
library(furrr)
library(data.table)
library(rvest)

#function

html_reader <- function() {
  read_html("https://www.vworld.kr/dtmk/dtmk_ntads_s002.do?usrIde=any_0000009349&pageSize=10&pageUnit=10&listPageIndex=1&gidsCd=&searchKeyword=%EA%B3%B5%EC%8B%9C&svcCde=NA&gidmCd=&searchBrmCode=&datIde=&searchFrm=&dsId=6&searchSvcCde=&searchOrganization=&dataSetSeq=6&searchTagList=&pageIndex=1&sortType=00&datPageIndex=1&datPageSize=100&startDate=2024-09-12&endDate=2025-09-12&sidoCd=&sigunguCd=&dsNm=&formatSelect=") %>% html_nodes("li") %>% html_elements("div.less") %>% html_nodes("span") %>% html_text() %>% unique()
}

file_downloader <- function(idx) {
  walk2(
    1370 + seq_along(file_name[idx]),
    file.path("~/Visual_landvalue_map/data", paste0(file_name, ".zip")),
    ~ download.file(
      paste0("https://www.vworld.kr/dtmk/downloadResourceFile.do?ds_id=20171128DS00144&fileNo=", .x),
      destfile = .y,
      mode = "wb",
      quiet = FALSE
    )
  )
}

csv_reader <- function() {
  
  path = "~/Visual_landvalue_map/data"
  
  file_list = paste0(path, list.files(path))

  data_list <- map(file_list, ~read_csv(.x, locale = locale(encoding = "euc-kr")))

  combined_df <- rbindlist(data_list)
  
  return(combined_df)
}

mean_reader <- function(reader) {
  reader %>% 
    group_by(법정동명) %>% 
    summarise(mean = mean(공시지가))
}

geo_maker <- function(geo_code) {
  tryCatch({
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
    
    full_url <- paste0(
      url, "?", 
      imap_chr(params, ~ paste0(.y, "=", curlEscape(.x))) %>% 
        paste(collapse = "&")
    )
    
    response <- GET(full_url)
    content <- fromJSON(content(response, "text"))
    
    # 상태 확인 로직을 먼저 수행
    if (content$response$status == "OK") {
      point_df <- content$response$result$point %>% as.data.frame()
      # x와 y를 숫자형으로 변환
      point_df$x <- as.numeric(as.character(point_df$x)) 
      point_df$y <- as.numeric(as.character(point_df$y)) 
      return(point_df) # 숫자형 x, y를 가진 데이터 프레임 반환
    } else {
      # API 호출은 성공했으나, 주소를 찾지 못했거나 다른 API 관련 문제
      return(data.frame(x = NA_real_, y = NA_real_)) 
    }
  }, error = function(e) {
    # API 호출 자체에서 오류 발생 (예: 네트워크 문제, 잘못된 URL 등)
    # 디버깅을 위해 오류 메시지를 출력할 수 있습니다: print(paste("오류 발생 (주소:", geo_code, "):", e$message))
    return(data.frame(x = NA_real_, y = NA_real_)) 
  })
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

file_name <- html_reader()

file.exists(paste0("~/Visual_landvalue_map/data/", file_name, ".zip"))

file_downloader()

combine_df <- csv_reader()

combine_raw <- mean_reader(combine_df)

combine_main <- table_maker(combine_raw)

seoul_main[,2:4] <- apply(seoul_main[,2:4], 2, as.numeric) %>% as.data.frame()
seoul_main$mean_chr <- as.character(seoul_main$mean_chr)

geo_maker("경상북도 경산시 진량읍 평사리(平沙)")
geo_maker("경상북도 경산시 진량읍 평사리(坪沙)")
geo_maker("경기도 파주시 진서면 어룡리")

combine_main[2858, 1] <- "경기도 파주시 진서면 어룡리"
combine_main[2858, 3:4] <- geo_maker("경기도 파주시 진서면 어룡리")

combine_main[5654, 3:4] <- geo_maker("경상북도 경산시 진량읍 평사리(坪沙)")
combine_main[5655, 3:4] <- geo_maker("경상북도 경산시 진량읍 평사리(平沙)")

combine_main[, 2:4] <- apply(combine_main[, 2:4], 2, as.numeric) %>% as.data.frame()
combine_main$mean_chr <- as.character(comma(round(combine_main$mean_chr), digits = 0))

write_csv(combine_main, "main_geo_file.csv")
