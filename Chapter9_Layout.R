## -*- coding: utf-8 -*-
## Chapter 9: layout of a plot

# loading packages --------------------------------------------------------
library('gcookbook')
library('ggplot2')
library('magrittr')

p <- heightweight %>% 
  ggplot(aes(ageYear, heightIn)) +
  geom_point()
p + ggtitle('Age and height of school children') + 
  theme_bw() + 
  theme(plot.title = element_text(vjust = -10, hjust = .2))
# Or
p + labs(title = 'Age and height of school children')
# 或者将标题作为文本注释
p + annotate('text', x = mean(range(heightweight$ageYear)), 
             y = Inf, label = 'Age and height', vjust = 2, size = 5, colour = 'darkred')

# 修改文本外观 ------------------------------------------------------------------

p + theme(axis.title.x = element_text(size = 16, lineheight = .9, face = 'bold.italic', colour = 'red'))
p + ggtitle('Age and height of school children') +
  theme(plot.title = element_text(size = rel(1.5), lineheight = .9, face = 'bold.italic', colour = 'darkred'))
# 几何对象则直接设置其文本属性
p + annotate('text', x = 15, y = 53, label = 'Some text', size = 7, colour = 'darkred', fontface = 'bold.italic')
p + geom_text(aes(label = weightLb), size = 3, colour = 'red')