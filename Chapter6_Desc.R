
# loading packages --------------------------------------------------------

library('magrittr')
library('ggplot2')
library('tidyverse')
library('gcookbook')

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
bt + facet_grid(smoke ~ .)
bt + facet_grid(race ~ ., scales = 'free') ## 单独y轴标签

birwt %>% 
  ggplot(aes(x = bwt, fill = smoke0)) +
  geom_histogram(position = 'identity', alpha = 0.4)

# density curve -----------------------------------------------------------

faithful %>% 
  ggplot(aes(x = waiting)) +
  geom_density()
faithful %>% 
  ggplot(aes(x = waiting)) +
  geom_line(stat = 'density') +
  expand_limits(y = 0)

w <- faithful$waiting
ggplot(NULL, aes(x = w)) + geom_density()
# adjust the width of curve
faithful %>% 
  ggplot(aes(x = waiting)) +
  geom_line(stat = 'density', adjust = .25, colour = 'red') +
  geom_line(stat = 'density') +
  geom_line(stat = 'density', adjust = 2, colour = 'blue')
faithful %>% 
  ggplot(aes(x = waiting)) +
  geom_density(fill = 'blue', alpha = .2) +
  xlim(45, 105)
faithful %>% 
  ggplot(aes(x = waiting)) +
  geom_density(fill = 'blue', colour = NA, alpha = .2) +
  geom_line(stat = 'density') +
  xlim(45, 105)
faithful %>% 
  ggplot(aes(x = waiting, y = ..density..)) +
  geom_histogram(fill = 'cornsilk', colour = 'grey60', size = .2) +
  geom_density() +
  xlim(35, 105)

# 分组密度曲线 ------------------------------------------------------------------

birwt %>% 
  ggplot(aes(x = bwt, colour = smoke0)) +
  geom_density()
birwt %>% 
  ggplot(aes(bwt, fill = smoke0)) +
  geom_density(alpha = .3, colour = NA) +
  geom_line(stat = 'density')
birwt %>% 
  ggplot(aes(bwt)) +
  geom_density() +
  facet_grid(smoke0 ~ .)
birwt %>% 
  ggplot(aes(x = bwt, y = ..density..)) +
  geom_histogram(fill = 'white', colour = 'black') +
  geom_density() +
  facet_grid(smoke0 ~ .)

# 频数多边形 -------------------------------------------------------------------

faithful %>% 
  ggplot(aes(waiting)) +
  geom_freqpoly()
binsize <- range(faithful$waiting) %>% diff()/15
faithful %>% 
  ggplot(aes(waiting)) +
  geom_freqpoly(binwidth = binsize)

# 基本箱线图 -------------------------------------------------------------------

birwt %>% 
  ggplot(aes(factor(race), bwt)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + ## 添加触须
  geom_boxplot(width = .5, outlier.colour = 'red', 
               outlier.size = 1.5, outlier.shape = 21)
  

# single boxplot
birwt %>% 
  ggplot(aes(1, bwt)) +
  geom_boxplot() +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank())
# add notch
birwt %>% 
  ggplot(aes(factor(race), bwt)) +
  geom_boxplot(notch = TRUE)
# add mean
birwt %>% 
  ggplot(aes(factor(race), bwt)) +
  geom_boxplot() +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, fill = 'white')
# 小提琴图
p <- heightweight %>% ggplot(aes(sex, heightIn))
p + geom_violin()
p + geom_violin() + 
  geom_boxplot(width = .1, fill = 'black', outlier.colour = NA) +
  stat_summary(fun.y = median, geom = 'point', fill = 'white', shape = 21, size = 2.5)
p + geom_violin(trim = FALSE) ## 保留尾部
# 图面积和每组数据值数目成正比
p + geom_violin(scale = 'count')
p + geom_violin(adjust = 2)
p + geom_violin(adjust = .5)
  











