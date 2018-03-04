## -*- coding: utf-8 -*-
## 2018/03/04
## get started with plotly

# loading packages --------------------------------------------------------
library('plotly')
library('magrittr')

p <- midwest %>% 
  plot_ly(x = ~percollege, color = ~state, type = 'box')

# add line for mean using geom_vline --------------------------------------

set.seed(1234)
dat <- data.frame(
  cond = factor(rep(c('A', 'B'), each = 200)),
  rating = c(rnorm(200), rnorm(200, mean = .8))
)
p <- ggplot(data = dat, aes(rating)) +
  geom_histogram(binwidth = .5, colour = 'black', fill = 'white') +
  geom_vline(aes(xintercept = mean(rating, na.rm = TRUE)), 
             colour = 'red', linetype = 'dashed', size = 1)
p <- ggplotly(p)
# Create a shareable link to your chart
link <- api_create(p, filename = 'vline')


# overlaid histograms with geom_vline -------------------------------------
library(plyr)
cdat <- ddply(dat, "cond", summarise, rating.mean=mean(rating))

# Overlaid histograms with means
p <- ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=rating.mean),
             linetype="dashed", size=1)

p <- ggplotly(p)
# Create a shareable link to your chart
link <- api_create(p, filename = 'abline')
