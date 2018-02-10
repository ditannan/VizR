
# Loading packages --------------------------------------------------------

library('gcookbook')
library('ggplot2')
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

















