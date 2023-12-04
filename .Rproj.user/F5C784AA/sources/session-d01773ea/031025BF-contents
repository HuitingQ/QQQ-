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

# Create interactive scatter plot using plotly
plot <- plot_ly(
  daily_data,
  x = ~GapSizePercentage,
  y = ~GapFillPercentage,
  text = ~paste("Date: ", Date, "<br>Gap Size %: ", round(GapSizePercentage, 2),
                "<br>Gap Fill %: ", round(GapFillPercentage, 2)),
  mode = "markers",
  type = 'scatter',
  marker = list(size = 10, opacity = 0.5, color = 'rgba(0, 100, 200, 0.6)')
) %>%
  layout(
    title = "Gap Size Percentage vs Gap Fill Percentage",
    xaxis = list(title = "Gap Size Percentage"),
    yaxis = list(title = "Gap Fill Percentage")
  )

# Print the plot
plot
