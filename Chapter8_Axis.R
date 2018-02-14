# loading packages --------------------------------------------------------

library('gcookbook')
library('magrittr')
library('ggplot2')

# flip axis -------------------------------------------------------------

PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot() +
  coord_flip()
# flip and reverse the labels of x axis
PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

# set range for continuous axis -------------------------------------------

# xlim(), ylim()
p <- PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot()
p + ylim(0, max(PlantGrowth$weight)) 
## ylim(0, 10) equivalent to scale_y_continuous(limits = c(0, 10))
p + scale_y_continuous(limits = c(5, 6.5))
p + coord_cartesian(ylim = c(5, 6.5))
p + expand_limits(y = 0)