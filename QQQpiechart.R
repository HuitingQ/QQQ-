# Load required libraries
library(data.table)
library(plotly)
library(purrr)
library(readr)
library(magrittr)
library(tidyr)
library(dplyr)

folder_path <- "G://trading/QQQ/"
rds_file <- "G://trading/QQQ/all_data.rds"

# Check if the RDS file exists
if (file.exists(rds_file)) {
  # Load data from RDS file
  all_data <- readRDS(rds_file)
} else {
  # Get all CSV files in the folder
  all_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Read and merge all CSV files
  all_data <- map_df(all_files, read_csv)
  
  # Save the merged data to an RDS file for future use
  saveRDS(all_data, rds_file)
}

# 将 all_data 转换为 data.table 对象
all_data <- as.data.table(all_data)

# 使用data.table语法过滤特定时间范围内的数据
all_data <- all_data[hour(Timestamp) >= 9 & (hour(Timestamp) < 16 | (hour(Timestamp) == 16 & minute(Timestamp) == 0))]

# 排序
setorder(all_data, Timestamp)

# Calculate daily data
daily_data <- all_data[, list(
  Open = Open[1],
  High = max(High),
  Low = min(Low),
  Close = Close[.N]
), by = .(Date = as.Date(Timestamp))]

# Calculate the gap and gap fill percentage
daily_data[, Gap := shift(Close, type = "lag") - Open]
daily_data[, GapSizePercentage := (Gap / shift(Close, type = "lag")) * 100]
daily_data[, GapFillPercentage := dplyr::case_when(
  Gap > 0 ~ (Low - Open) / Gap * 100,
  Gap < 0 ~ (High - Open) / abs(Gap) * 100,
  TRUE ~ NA_real_
)]

daily_data <- daily_data[abs(GapSizePercentage) > 0.1& abs(GapSizePercentage) <2, ]

# Remove NA values
daily_data <- daily_data[!is.na(GapFillPercentage)]

# 加载所需的库
library(ggplot2)

# 添加新的分类条件来分类 GapFillPercentage
daily_data[, FillCategory := case_when(
  Gap > 0 & GapFillPercentage < -100 ~ "Positive Gap, Fill < -100%",
  Gap > 0 & GapFillPercentage >= -100 ~ "Positive Gap, Fill >= -100%",
  Gap < 0 & GapFillPercentage > 100 ~ "Negative Gap, Fill > 100%",
  Gap < 0 & GapFillPercentage <= 100 ~ "Negative Gap, Fill <= 100%",
  TRUE ~ NA_character_
)]

# 指定类别的顺序
ordered_categories <- c("Negative Gap, Fill < 100%", "Positive Gap, Fill < 100%", "Negative Gap, Fill >= 100%", "Positive Gap, Fill >= 100%")

# 使用 factor 函数并设置 levels 参数
daily_data$Category <- factor(daily_data$Category, levels = ordered_categories)


# 加载所需的库
library(plotly)

# 根据 FillCategory 计算每个类别的数量
category_counts <- table(daily_data$FillCategory)

# 创建一个交互式的饼图
plot <- plot_ly(labels = names(category_counts), values = as.vector(category_counts), type = 'pie') %>%
  layout(title = "Gap and Gap Fill Percentage Categories",
         showlegend = T)

# 打印图表
plot


