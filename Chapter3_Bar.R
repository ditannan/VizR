# Chapter 3: Bar

# load packages -----------------------------------------------------------

library('gcookbook')


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
diamonds %>% dplyr::glimpse()
# 离散型
ggplot(diamonds, aes(x = cut)) +
  geom_bar() ## equ to: geom_bar(stat = "bin")
diamonds$cut %>% levels()
# 连续性
ggplot(diamonds, aes(x = carat)) + ## geom_histogram(binwidth = 0.2)
  geom_bar()


# 条形图着色 -------------------------------------------------------------------




































