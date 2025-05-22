#package_load

library(deckgl)
library(RColorBrewer)
library(htmlwidgets)
library(tidyverse)
library(tools)
library(data.table)
library(purrr)
library(grDevices)

#main_code

main_geo_file <- read_csv("~/Visual_landvalue_map/main_geo_file.csv")

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

map_viz <- make_deckgl_column(main_geo_file)

map_viz

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
