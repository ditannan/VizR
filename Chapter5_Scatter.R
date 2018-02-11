
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

# mapping continuous variable to colour or shape --------------------------

heightweight %>% 
  ggplot(aes(ageYear, heightIn, colour = weightLb, fill = weightLb)) +
  geom_point(shape = 22, size = 2.5) +
  scale_color_gradient(low = 'black', high = 'white') +
  scale_fill_gradient(low = 'black', high = 'white')
heightweight %>% 
  ggplot(aes(ageYear, heightIn, size = weightLb)) +
  geom_point() +
  scale_size_continuous(range = c(2, 5))
heightweight %>% 
  ggplot(aes(ageYear, heightIn, fill = weightLb)) +
  geom_point(shape = 21, size = 2.5) +
  scale_fill_gradient(low = 'black', high = 'white', breaks = seq(70, 170, by = 20), guide = guide_legend())
  
heightweight %>% 
  ggplot(aes(ageYear, heightIn, size = weightLb, colour = sex)) +
  geom_point(alpha = .5) +
  scale_size_area() +
  scale_color_brewer(palette = 'Set1')











