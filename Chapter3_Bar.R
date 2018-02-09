# Chapter 3: Bar

# load packages -----------------------------------------------------------

library("ggplot2")
library('gcookbook')
library("dplyr")

# simple bar --------------------------------------------------------------

data("pg_mean")
pg_mean %>% str
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_bar(stat = 'identity')
# x is continuous
data("BOD")
ggplot(BOD, aes(Time, demand)) +
  geom_bar(stat = 'identity')
# convert x to factor
ggplot(BOD, aes(factor(Time), demand)) +
  geom_bar(stat = 'identity')


# cluster bar -------------------------------------------------------------

data("cabbage_exp")
cabbage_exp %>% str
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black') +
  scale_fill_brewer(palette = 'Pastel1')


# 频数条形图 -------------------------------------------------------------------

data("diamonds")
diamonds %>% glimpse()
# 离散型
ggplot(diamonds, aes(x = cut)) +
  geom_bar() ## equ to: geom_bar(stat = "bin")
diamonds$cut %>% levels()
# 连续性
ggplot(diamonds, aes(x = carat)) + ## geom_histogram(binwidth = 0.2)
  geom_bar()


# 条形图着色 -------------------------------------------------------------------

data("uspopchange")
uspopchange %>% glimpse()
# 人口增长最快的十个州
upc <- uspopchange %>% 
  filter(rank(Change) > 40)
# 州分布
upc$Region %>% table
# 州映射为颜色
upc %>% ggplot(aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
  geom_bar(stat = 'identity', colour = 'black') +
  scale_fill_manual(values = c('#669933', '#FFCC66')) +
  xlab('State')


# 正负条形图分别着色 ---------------------------------------------------------------

data("climate")
csub <- climate %>% 
  filter(Source == 'Berkeley' & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
# 将pos映射为颜色
csub %>% ggplot(aes(Year, Anomaly10y, fill = pos)) +
  geom_bar(stat = 'identity', position = 'identity', colour = 'black', size = 0.15) +
  scale_fill_manual(values = c('#CCEEFF', '#FFDDDD'), guide = F)
  

# 调整条形宽度和间距 ---------------------------------------------------------------

# 宽度default: geom_bar(width = 0.9), max = 1
pg_mean %>% View()
pg_mean %>% 
  ggplot(aes(group, weight)) +
  geom_bar(stat = 'identity', width = 0.9)

# 间距
cabbage_exp %>% ggplot(aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5)
cabbage_exp %>% ggplot(aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity', position = position_dodge(0.7), width = 0.5)



# 堆积条形图上下颠倒 ---------------------------------------------------------------

cabbage_exp %>% ggplot(aes(Date, Weight, fill = Cultivar, order = plyr::desc(Cultivar))) +
  geom_bar(stat = 'identity') +
#  guides(fill = guide_legend(reverse = T)) +
  scale_fill_brewer(palette = 'Pastel1')


# 百分比堆积条形图 ----------------------------------------------------------------

library("plyr")
ce <- cabbage_exp %>% ddply("Date", transform, percent_weight = Weight / sum(Weight) * 100)
ce %>% ggplot(aes(Date, percent_weight, fill = Cultivar)) +
  geom_bar(stat = 'identity')


# Add text label ----------------------------------------------------------

p <- cabbage_exp %>% 
  ggplot(aes(interaction(Date, Cultivar), Weight)) +
  geom_bar(stat = 'identity')
# add text label under the upper line
p + geom_text(aes(label = Weight), vjust = 1.5, colour = 'white')
# above the upper line and set the range of y axis
p + geom_text(aes(label = Weight), vjust = -0.5) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)
# Or
p + geom_text(aes(y = Weight + 0.1, label = Weight))

# cluster bar
p1 <- cabbage_exp %>% 
  ggplot(aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity', position = 'dodge')
p1 + geom_text(aes(label = cabbage_exp$Weight), vjust = 1.5, position = position_dodge(.9), size = 3)

# 堆积挑图数据标签
# 排序再分组顶端
ce <- cabbage_exp %>% 
  arrange(Date, desc(Cultivar)) %>% 
  ddply("Date", transform, label_y = cumsum(Weight))
ce %>% 
  ggplot(aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = label_y, label = Weight), vjust = 1.25, colour = 'white')
# 中间
ce1 <- cabbage_exp %>% 
  arrange(Date, desc(Cultivar)) %>% 
  ddply("Date", transform, label_y = cumsum(Weight) - 0.5 * Weight)
ce1 %>% 
  ggplot(aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = label_y, label = Weight), colour = 'white')
# final version
ce1 %>% 
  ggplot(aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity', colour = 'black') +
  geom_text(aes(y = label_y, label = paste0(format(Weight, nsmall = 2), 'kg')), size = 4) +
  scale_fill_brewer(palette = 'Pastel1')
  
  
  
  
  
  
  
  
  















