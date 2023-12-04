# 加载所需的库
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# 读取文件夹中的所有Excel文件
files <- list.files(path = "G/炒股/云聪交易计/QQQ交易规律总结/QQQ统计作业/QQQ(1)/QQQ", pattern = "\\.xlsx$", full.names = TRUE)

# 初始化一个数据框来存储结果
all_data <- data.frame()

# 遍历每个文件
for (file in files) {
  # 读取Excel文件
  data <- read_excel(file)
  
  # 将数据按日期分组，并计算每组的最高和最低振幅以及到达时间
  data <- data %>%
    arrange(Timestamp) %>%
    group_by(Date = as.Date(Timestamp)) %>%
    mutate(Amplitude = pmax((High - first(Open)) / first(Open) * 100, (first(Open) - Low) / first(Open) * 100),
           MinutesToReach = row_number() - 1) %>%
    slice(which.max(Amplitude)) %>%
    ungroup()
  
  # 将结果添加到总数据框中
  all_data <- rbind(all_data, data)
}

# 计算每个振幅和时间组合的出现频率
all_data <- all_data %>%
  add_count(Amplitude, MinutesToReach, name = "Frequency")

# 使用ggplot2创建散点图，其中点的大小表示频率，颜色表示到达时间
ggplot(all_data, aes(x = Amplitude, y = MinutesToReach, size = Frequency, color = MinutesToReach)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 20)) +
  labs(title = "Time to Reach First Highest or Lowest Point vs Amplitude",
       x = "Amplitude (%)",
       y = "Minutes to Reach",
       size = "Frequency",
       color = "Minutes to Reach") +
  theme_minimal()
