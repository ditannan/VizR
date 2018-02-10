
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








