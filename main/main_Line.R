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
library(tools)
library(furrr)
library(data.table)

#function

csv_reader <- function() {
  
  path = "~/Visual_landvalue_map/data/"
  
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
combine_main$mean_chr <- as.character(comma(round(combine_main$mean_chr)))

make_deckgl_column <- function(data) {

  data_filtered <- data[!is.na(data$x) & !is.na(data$y), ]
  
  if (nrow(data_filtered) == 0) {
    message("표시할 유효한 데이터가 없습니다 (x, y 좌표 확인 필요).")
    return(NULL)
  }

  num_colors <- 6
  palette_hex <- RColorBrewer::brewer.pal(num_colors, "YlOrRd")
  valid_means <- data_filtered$mean[!is.na(data_filtered$mean)]
  
  if (length(valid_means) == 0) {
    data_filtered$color_hex <- palette_hex[1]
  } else if (length(unique(valid_means)) < 2) {
    data_filtered$color_hex <- palette_hex[1]
  } else if (length(unique(valid_means)) < num_colors) {
    data_filtered$color_hex <- palette_hex[as.numeric(factor(data_filtered$mean))]
  } else {
    breaks <- quantile(data_filtered$mean, probs = seq(0, 1, length.out = num_colors + 1), na.rm = TRUE, type = 7)
    unique_breaks <- unique(breaks)
    if (length(unique_breaks) < 2) {
      data_filtered$color_hex <- palette_hex[1]
    } else {
      data_filtered$color_hex <- cut(data_filtered$mean,
                                     breaks = unique_breaks,
                                     labels = palette_hex,
                                     include.lowest = TRUE,
                                     right = FALSE)
    }
  }
  data_filtered$color_hex <- as.character(data_filtered$color_hex)
  data_filtered$color_hex[is.na(data_filtered$color_hex) & !is.na(data_filtered$mean)] <- palette_hex[num_colors]
  data_filtered$color_hex[is.na(data_filtered$color_hex)] <- palette_hex[1]
  
  rgb_colors_list <- lapply(data_filtered$color_hex, function(hex) {
    if (is.na(hex)) return(c(128, 128, 128))
    as.numeric(grDevices::col2rgb(hex))
  })
  data_filtered$fill_color_rgb <- rgb_colors_list

  data_filtered$mean_transformed <- sqrt(data_filtered$mean)

  data_filtered <- data_filtered[!is.na(data_filtered$mean_transformed) & is.finite(data_filtered$mean_transformed), ]
  
  if (nrow(data_filtered) == 0) {
    message("표시할 유효한 데이터가 없습니다 (변환 후 유효한 mean_transformed 값 없음).")
    return(NULL)
  }
  
  deckgl(
    longitude = mean(data_filtered$x, na.rm = TRUE),
    latitude = mean(data_filtered$y, na.rm = TRUE),
    zoom = 10,
    pitch = 45,
    width = "100vw",
    height = "100vh",
    sizingPolicy = sizingPolicy(
      browser.fill = TRUE,
      viewer.fill = TRUE
    )
  ) %>%
    add_basemap() %>%
    add_column_layer(
      data = data_filtered,
      id = "landvalue-columns-sqrt",
      properties = list(
        diskResolution = 5,
        radius = 100,
        extruded = TRUE,
        elevationScale = 50,
        getPosition = ~c(x, y),
        getElevation = ~mean_transformed,
        getFillColor = ~fill_color_rgb,
        opacity = 0.7,
        fp64 = FALSE,
        autoHighlight = TRUE,
        pickable = TRUE,
        tooltip = list(
          html = "
          <div style='
            font-family: \"Nanum Gothic\", sans-serif;
            font-size: 14px;
            line-height: 1.3;
            color: #333333;
            background-color: rgba(255, 255, 255, 0.9);
            padding: 10px;
            border-radius: 5px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.2);
          '>
          
            <div style='font-size: 16px; font-weight: bold; margin-bottom: 5px;'>{{geo}}</div>
            
            <div>가격 : {{mean_chr}}원</div>
            
            </div>
          "
        )
      )
    )
}
map_viz <- make_deckgl_column(combine_main)

htmlwidgets::saveWidget(map_viz, "temp_map.html", selfcontained = TRUE)

final_html_file <- "map_with_viewport.html"
temp_html_file <- "temp_map_for_processing.html"

if (!is.null(map_viz)) {
  htmlwidgets::saveWidget(map_viz, temp_html_file, selfcontained = TRUE)
  
  html_content <- readLines(temp_html_file, warn = FALSE)

  head_line_index <- grep("<head>", html_content, ignore.case = TRUE)
  
  if (length(head_line_index) == 1) {
    meta_tag_line <- '  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">'
    
    part1 <- html_content[1:head_line_index[1]]
    part2 <- html_content[(head_line_index[1] + 1):length(html_content)]
    html_content_modified <- c(part1, meta_tag_line, part2)
    
    writeLines(html_content_modified, final_html_file)
    message(paste("성공: 뷰포트 메타 태그가 추가된 파일이 다음 위치에 저장되었습니다:", final_html_file))

  } else {
    warning("<head> 태그를 찾을 수 없어 뷰포트 메타 태그를 자동으로 추가하지 못했습니다. HTML 파일을 수동으로 수정해주세요.")
    message(paste("오류: 원본 HTML 파일이 다음 위치에 저장되었습니다 (수동 수정 필요):", temp_html_file))

  }
} else {
  message("map_viz 객체가 생성되지 않았습니다. make_deckgl_column 함수를 먼저 실행해주세요.")
}
