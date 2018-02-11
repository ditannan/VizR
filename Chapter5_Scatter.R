
# loading packages --------------------------------------------------------

library('tidyverse')
library('gcookbook')

# Simple scatter plot -----------------------------------------------------

heightweight %>% 
  ggplot(aes(ageYear, heightIn)) +
  geom_point(shape = 19, size = 1.5, colour = 'blue')
heightweight %>% 
  ggplot(aes(ageYear, heightIn, colour = sex, shape = sex)) +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Set1') +
  scale_shape_manual(values = c(22, 25))
hw <- heightweight
hw$weightGruop <- cut(hw$weightLb, breaks = c(-Inf, 100, Inf), labels = c('< 100', '>= 100'))
hw %>% 
  ggplot(aes(ageYear, heightIn, shape = sex, fill = weightGruop)) +
  geom_point() +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c(NA, 'black'), guide = guide_legend(override.aes = list(shape = 21)))