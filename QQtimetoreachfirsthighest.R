# 加载必要的库
library(readr)  # 加载readr库来读取CSV文件
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)  # 确保加载magrittr包来使用%>%操作符
library(purrr)

folder_path <- "G://trading/QQQ/"

all_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# 读取和合并所有 CSV 文件
all_data <- map_df(all_files, read_csv)

# 加载 data.table 库
library(data.table)

# 将 all_data 转换为 data.table 对象
all_data <- as.data.table(all_data)

# 使用data.table语法过滤特定时间范围内的数据
all_data <- all_data[hour(Timestamp) >= 9 & (hour(Timestamp) < 16 | (hour(Timestamp) == 16 & minute(Timestamp) == 0))]

# 排序
setorder(all_data, Timestamp)

# 计算每日的最大和最小振幅及到达时间
results <- all_data %>%
  arrange(Timestamp) %>%
  group_by(Date = as.Date(Timestamp)) %>%
  summarise(
    MaxAmplitude = max(pmax((High - first(Open)) / first(Open) * 100, 
                            (first(Open) - Low) / first(Open) * 100, na.rm = TRUE)),  # 计算振幅
    TimeToReach = which.max(pmax((High - first(Open)) / first(Open) * 100, 
                                 (first(Open) - Low) / first(Open) * 100, na.rm = TRUE))  # 计算到达最大振幅所需的时间
  ) %>%
  filter(MaxAmplitude >= 0, MaxAmplitude <= 3)
  

library(plotly)

# 使用 plot_ly 创建交互式散点图
plot <- plot_ly(
  data = results,  # 使用计算出的 results 数据框
  x = ~MaxAmplitude,
  y = ~TimeToReach,
  text = ~paste('Amplitude: ', MaxAmplitude, '%<br>Time to Reach: ', TimeToReach, ' mins'),  # 创建悬停文本
  type = 'scatter',
  mode = 'markers'
)

# 添加轴标签和标题
plot <- layout(
  plot,
  title = "Time to Reach First Highest or Lowest Point vs Amplitude",
  xaxis = list(title = "Amplitude (%)"),
  yaxis = list(title = "Minutes to Reach")
)

# 显示图形
plot