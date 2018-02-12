
# loading packages --------------------------------------------------------

library('magrittr')
library('ggplot2')
library('tidyverse')

# plot histogram ----------------------------------------------------------

h <- faithful %>% ggplot(aes(waiting))
h + geom_histogram(fill = 'white', colour = 'black', binwidth = 5) # 组距
# equivalent to
w <- faithful$waiting
ggplot(NULL, aes(x = w)) +
  geom_histogram(bins = 10) # 分组数
# 将x的取值切分为15组
binsize <- diff(range(faithful$waiting)) / 15
h + geom_histogram(binwidth = binsize, fill ='white', colour = 'black')
h + geom_histogram(binwidth = 8, fill = 'white', colour = 'black', boundary = 35)

birwt <- MASS::birthwt
bt <- birwt %>% 
  ggplot(aes(x = bwt)) +
  geom_histogram(fill = 'white', colour = 'black')
bt
bt + facet_grid(smoke ~ .)

birwt$smoke0 <- if_else(birwt$smoke == 0, 'No smoke', 'Smoke')
bt + facet_grid(smoke0 ~ .)
bt + facet_grid(race ~ ., scales = 'free') ## 单独y轴标签

birwt %>% 
  ggplot(aes(x = bwt, fill = smoke0)) +
  geom_histogram(position = 'identity', alpha = 0.4)
