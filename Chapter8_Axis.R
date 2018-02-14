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


