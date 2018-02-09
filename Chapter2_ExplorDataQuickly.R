# Chapter 2: explore data quickly
# load package ------------------------------------------------------------

library('ggplot2')
library("magrittr")

# load data 'pressure'
data("pressure")

# plot 线图、点图 --------------------------------------------------------------

p <- ggplot(pressure, aes(temperature, pressure))
p + geom_line() + geom_point()
# load data 'BOD'
data('BOD')
str(BOD)

# plot bar 直方图 ------------------------------------------------------------

barplot(BOD$demand, names.arg = BOD$Time)

# load data 'mtcars'
data("mtcars")
mtcars %>% str
table(mtcars$cyl) %>% barplot()
# equ to
mtcars %>% ggplot(aes(x = factor(cyl))) + geom_bar()


# plot histogram ----------------------------------------------------------

ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth = 4)


# plot boxplot ------------------------------------------------------------

data("ToothGrowth")
ToothGrowth %T>% str() %>% 
  boxplot(len ~ supp, .)
# equ to 
ggplot(ToothGrowth, aes(supp, len)) + geom_boxplot()

# 双变量
ToothGrowth %>% boxplot(len ~ supp + dose, .)
# equ to
ggplot(ToothGrowth, aes(interaction(supp, dose), len)) + geom_boxplot()
ToothGrowth %$% interaction(supp, dose) %>% table


# plot fun ----------------------------------------------------------------

curve(x^3 - 5*x, from = -4, to = 4)
curve(x^2, add = T, col = 'red')
# my function
myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}
ggplot(data.frame(x = c(0, 20)), aes(x = x)) +
  stat_function(fun = myfun, geom = 'line')




