
# Loading packages --------------------------------------------------------

library('gcookbook')
library('ggplot2')
library('tidyverse')
library('magrittr')

# Simple line -------------------------------------------------------------

data('BOD')
BOD1 <- BOD
BOD %>% 
  ggplot(aes(Time, demand)) +
  geom_line()
# convert time to factor
BOD1$Time <- factor(BOD1$Time)
p <- BOD1 %>% 
  ggplot(aes(Time, demand, group = 1)) +
  geom_line()
p
# 设置Y轴范围
p + ylim(0, max(BOD1$demand))
p + expand_limits(y = 0)

# Add data point ----------------------------------------------------------

BOD1 %>% 
  ggplot(aes(Time, demand, group = 1)) +
  geom_line() +
  geom_point()

worldpop %>% 
  ggplot(aes(Year, Population)) +
  geom_line() +
  geom_point() +
  scale_y_log10() ## y值取对数
# equ to, but: Y 轴值域不同
worldpop %>% 
  ggplot(aes(Year, log10(Population))) +
  geom_line() +
  geom_point()

# 多重线图 ----------------------------------------------------------------

library('plyr')
tg <- ToothGrowth %>% 
  ddply(c('supp', 'dose'), summarise, length = mean(len))
# 变量映射为颜色
tg %>% 
  ggplot(aes(dose, length, colour = supp)) +
  geom_line()
# 变量映射为线条
tg %>% 
  ggplot(aes(dose, length, linetype = supp)) +
  geom_line()
# convert x to factor
tg %>% 
  ggplot(aes(factor(dose), length, colour = supp, group = supp)) +
  geom_line()
# map supp to colour or shape
tg %>% 
  ggplot(aes(dose, length, colour = supp)) +
  geom_line()
tg %>% 
  ggplot(aes(dose, length, shape = supp)) +
  geom_line() +
  geom_point(size = 4)
tg %>% 
  ggplot(aes(dose, length, fill = supp)) +
  geom_line() +
  geom_point(size = 4, shape = 21)
# 左右移动0.2
tg %>% 
  ggplot(aes(dose, length, shape = supp)) + 
  geom_line(position = position_dodge(0.2)) +
  geom_point(position = position_dodge(0.2), size = 4)

# Line tpye ---------------------------------------------------------------

BOD %>% 
  ggplot(aes(Time, demand)) +
  geom_line(linetype = 'dashed', size = 1, colour = 'blue')
tg %>% 
  ggplot(aes(dose, length, colour = supp)) +
  geom_line() +
  scale_color_brewer(palette = 'Set1')
# add group variable
tg %>% 
  ggplot(aes(dose, length, group = supp)) +
  geom_line(colour = 'darkgreen', size = 1.5) +
  geom_point(size = 4, shape = 22, fill = 'white')

# 修改数据标记样式 ----------------------------------------------------------------

tg %>% 
  ggplot(aes(dose, length, fill = supp)) +
  geom_line(position = position_dodge(0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(0.2)) +
  # scale_fill_brewer(palette = 'Set1') +
  scale_fill_manual(values = c('white', 'black'))

# plot area graph ---------------------------------------------------------

sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
sunspotyear %>%
  ggplot(aes(Year, Sunspots)) +
  geom_area(colour = 'black', fill = 'blue', alpha = 0.2)
sunspotyear %>% 
  ggplot(aes(Year, Sunspots)) +
  geom_area(fill = 'blue', alpha = 0.2) +
  geom_line()

# 堆积面积图 -------------------------------------------------------------------

uspopage %>% 
  ggplot(aes(Year, Thousands, fill = AgeGroup, order = desc(AgeGroup))) + 
  geom_area(colour = 'black', size = 0.2, alpha = 0.4) +
  scale_fill_brewer(palette = 'Blues', breaks = rev(levels(uspopage$AgeGroup)))
uspopage %>% 
  ggplot(aes(Year, Thousands, fill = AgeGroup, order = desc(AgeGroup))) +
  geom_area(colour = NA, alpha = .4) +
  scale_fill_brewer(palette = 'Blues') +
  geom_line(position = 'stack', size = .2)

# 百分堆积图 -------------------------------------------------------------------

uspopage_prop <- uspopage %>% 
  ddply('Year', mutate, Percent = Thousands / sum(Thousands) * 100)
uspopage_prop %>% 
  ggplot(aes(Year, Percent, fill = AgeGroup, order = desc(AgeGroup))) +
  geom_area(colour = 'black', size = .2, alpha = .4) +
  scale_fill_brewer(palette = 'Blues')

# 添加置信阈 -------------------------------------------------------------------

clim <- climate %>% 
  filter(Source == 'Berkeley') %>% 
  select(Year, Anomaly10y, Unc10y)
clim %>%
  ggplot(aes(Year, Anomaly10y)) +
  geom_ribbon(aes(ymin = Anomaly10y - Unc10y, ymax = Anomaly10y + Unc10y), alpha = .2) +
  geom_line()
# 虚线表示置信阈
clim %>% 
  ggplot(aes(Year, Anomaly10y)) +
  geom_line(aes(y = Anomaly10y - Unc10y), colour = 'grey50', linetype = 'dotted') +
  geom_line(aes(y = Anomaly10y + Unc10y), colour = 'grey50', linetype = 'dashed') +
  geom_line()
