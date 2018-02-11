
# loading packages --------------------------------------------------------

library('tidyverse')
library('gcookbook')
library('magrittr')

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
hw$weightGruop <- cut(hw$weightLb, breaks = c(-Inf, 100, Inf), 
                      labels = c('< 100', '>= 100'))
hw %>% 
  ggplot(aes(ageYear, heightIn, shape = sex, fill = weightGruop)) +
  geom_point() +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c(NA, 'black'), 
                    guide = guide_legend(override.aes = list(shape = 21)))

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
  scale_fill_gradient(low = 'black', high = 'white', 
                      breaks = seq(70, 170, by = 20), guide = guide_legend())
  
heightweight %>% 
  ggplot(aes(ageYear, heightIn, size = weightLb, colour = sex)) +
  geom_point(alpha = .5) +
  scale_size_area() +
  scale_color_brewer(palette = 'Set1')

# 图形重叠问题 ------------------------------------------------------------------

sp <- diamonds %>% 
  ggplot(aes(carat, price))
sp + geom_point(alpha = .01)
sp + stat_bin2d(bins = 50) +
  scale_fill_gradient(low = 'lightblue', high = 'red', limits = c(0, 6000))
# 六边形
library('hexbin')
sp + stat_binhex() +
  scale_fill_gradient(low = 'lightblue', high = 'red', limits = c(0, 8000))

sp1 <- ChickWeight %>% 
  ggplot(aes(Time, weight))
sp1 + geom_point() + geom_jitter()
sp1 + geom_point(position = position_jitter(width = .5, height = 0))
sp1 + geom_boxplot(aes(group = Time))

# Add smooth line ---------------------------------------------------------

sp2 <- heightweight %>% 
  ggplot(aes(ageYear, heightIn))
sp2 + geom_point(colour = 'grey60') + 
  stat_smooth(method = lm, level = .99, colour = 'black')

# smooth line for logistic 
b <- MASS::biopsy
b$classn <- b %$% if_else(class == 'benign', 0, 1)

b %>% 
  ggplot(aes(V1, classn)) +
  geom_point(shape = 21, size = 1.5) +
  geom_jitter(width = .3, height = .06, alpha = .4)

b %>% 
  ggplot(aes(V1, classn)) +
  geom_point(position = position_jitter(width = .3, height = .06), 
             alpha = .4, shape = 21, size = 1.5) +
  stat_smooth(method = glm, method.args = list(family = binomial))

sps <- heightweight %>% 
  ggplot(aes(ageYear, heightIn, colour = sex)) +
  geom_point(alpha = .4) + 
  scale_color_brewer(palette = 'Set1')
sps + stat_smooth()
sps + stat_smooth(method = lm, se = FALSE, fullrange = TRUE)











