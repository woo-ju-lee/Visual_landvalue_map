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
library(fs)

#function

html_reader <- function(date = c(Sys.Date() - 365)) {
  indexing <- read_html(paste0("https://www.vworld.kr/dtmk/dtmk_ntads_s002.do?usrIde=any_0000009349&pageSize=10&pageUnit=100&listPageIndex=1&gidsCd=&searchKeyword=%EA%B3%B5%EC%8B%9C&svcCde=NA&gidmCd=&searchBrmCode=&datIde=&searchFrm=&dsId=6&searchSvcCde=&searchOrganization=&dataSetSeq=6&searchTagList=&pageIndex=&sortType=00&datPageIndex=&datPageSize=100&startDate=", date, "&endDate=", Sys.Date(), "&sidoCd=&sigunguCd=&dsNm=&formatSelect=CSV")) %>%
    html_nodes("article.content div button") %>% 
    html_text() %>% 
    as.numeric() %>% 
    max(na.rm = T)
  
  land_name <- map(1:indexing, function(i) {
    read_html(paste0("https://www.vworld.kr/dtmk/dtmk_ntads_s002.do?usrIde=any_0000009349&pageSize=10&pageUnit=100&listPageIndex=1&gidsCd=&searchKeyword=%EA%B3%B5%EC%8B%9C&svcCde=NA&gidmCd=&searchBrmCode=&datIde=&searchFrm=&dsId=6&searchSvcCde=&searchOrganization=&dataSetSeq=6&searchTagList=&pageIndex=1&sortType=00&datPageIndex=", i, "&datPageSize=100&startDate=", date, "&endDate=", Sys.Date(), "&sidoCd=&sigunguCd=&dsNm=&formatSelect=CSV")) %>% 
      html_nodes("li") %>% 
      html_elements("div.less") %>% 
      html_nodes("span") %>% 
      html_text()
  }) %>% 
    unlist()
  
  land_date <- map(1:indexing, function(i){
    read_html(paste0("https://www.vworld.kr/dtmk/dtmk_ntads_s002.do?usrIde=any_0000009349&pageSize=10&pageUnit=100&listPageIndex=1&gidsCd=&searchKeyword=%EA%B3%B5%EC%8B%9C&svcCde=NA&gidmCd=&searchBrmCode=&datIde=&searchFrm=&dsId=6&searchSvcCde=&searchOrganization=&dataSetSeq=6&searchTagList=&pageIndex=1&sortType=00&datPageIndex=", i, "&datPageSize=100&startDate=", date, "&endDate=", Sys.Date(), "&sidoCd=&sigunguCd=&dsNm=&formatSelect=CSV")) %>% 
      html_elements("section article div.list.bd.box.hover ul li div.item.row div.txt > span:nth-of-type(3) em") %>% 
      html_text()
  }) %>% 
    unlist()
  
  url_index <- map(1:indexing, function(i) {
    read_html(paste0("https://www.vworld.kr/dtmk/dtmk_ntads_s002.do?usrIde=any_0000009349&pageSize=10&pageUnit=100&listPageIndex=1&gidsCd=&searchKeyword=%EA%B3%B5%EC%8B%9C&svcCde=NA&gidmCd=&searchBrmCode=&datIde=&searchFrm=&dsId=6&searchSvcCde=&searchOrganization=&dataSetSeq=6&searchTagList=&pageIndex=1&sortType=00&datPageIndex=", i, "&datPageSize=100&startDate=", date, "&endDate=", Sys.Date(), "&sidoCd=&sigunguCd=&dsNm=&formatSelect=CSV")) %>% html_nodes("section article div.list.bd.box.hover ul li") %>% 
    html_elements("div.btns button") %>% 
    html_attr("onclick") %>% 
    stringr::str_match(",\\s*'(\\d+)'\\s*,") %>% 
    .[,2] %>% 
    as.integer()
  }) %>% unlist()
  
  list(land_name = paste0(land_name, "_", land_date), url_index = url_index)
}

x1 <- html_reader("2023-08-01")

file_downloader <- function(df_data,
                            out_dir = "~/Visual_landvalue_map/data",
                            sleep_each = 0.3,
                            tries = 5,
                            timeout_sec = 1200) {
  dir.create(path.expand(out_dir), recursive = TRUE, showWarnings = FALSE)
  base <- "https://www.vworld.kr/dtmk/downloadResourceFile.do?ds_id=20171128DS00144&fileNo="
  options(timeout = max(timeout_sec, getOption("timeout", 60)))
  
  backoff <- function(k) min(60, 2^(k-1))
  
  map2(df_data$url_index, df_data$land_name, ~{
    url  <- paste0(base, .x)
    dest <- file.path(path.expand(out_dir), paste0(.y, ".zip"))
    tmp  <- paste0(dest, ".part")
    
    if (file.exists(dest) && file.info(dest)$size > 0) return(dest)
    
    for (k in seq_len(tries)) {
      if (file.exists(tmp)) unlink(tmp)
      ok <- try({
        download.file(url, destfile = tmp, mode = "wb",
                      method = "libcurl", quiet = FALSE)
        TRUE
      }, silent = TRUE)
      
      if (isTRUE(ok) && file.info(tmp)$size > 0) {
        file.rename(tmp, dest)
        Sys.sleep(sleep_each)
        return(dest)
      } else {
        if (file.exists(tmp)) unlink(tmp)
        if (k < tries) Sys.sleep(backoff(k))
      }
    }
    
    stop(sprintf("다운로드 실패: %s (index=%s)", url, .x))
  })
}

file_downloader(x1)

make_chunks <- function(path = NULL,
                        chunk_size = 17,
                        encoding = "euc-kr") {
  if (is.null(path)) path <- get_data_path()
  
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("CSV 파일이 없습니다: ", path)
  
  chunk_dir <- file.path(path, "chunks")
  if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)
  
  idx <- split(seq_along(files), ceiling(seq_along(files) / chunk_size))
  col_spec <- cols(.default = col_character())  # 타입 충돌 방지
  
  purrr::walk2(idx, seq_along(idx), ~{
    fi <- files[.x]
    dt <- data.table::rbindlist(
      lapply(fi, function(f) {
        as.data.table(readr::read_csv(f, locale = locale(encoding = encoding),
                                      col_types = col_spec))
      }),
      use.names = TRUE, fill = TRUE
    )
    out <- file.path(chunk_dir, sprintf("chunk_%03d.rds", .y))
    saveRDS(dt, out)
    rm(dt); gc()
    message(sprintf("[OK] Saved %s (files %d-%d)", out, min(.x), max(.x)))
  })
  
  invisible(chunk_dir)
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

write.csv(combine_df, "combine_df.csv")

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
