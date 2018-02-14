
# loading packages --------------------------------------------------------

library('gcookbook')
library('ggplot2')
library('magrittr')

# add text annotate -------------------------------------------------------

p <- faithful %>% 
  ggplot(aes(eruptions, waiting)) +
  geom_point()
p + annotate('text', x = 3, y = 48, label = 'Group 1') +
  annotate('text', x = 4.5, y = 66, label = 'Group 2')
p + annotate('text', x = 3, y = 48, label = 'Group 1', family = 'serif',
             fontface = 'italic', colour = 'darkred', size = 3) +
  annotate('text', x = 4.5, y = 66, label = 'Group 2', family = 'serif',
           fontface = 'italic', colour = 'darkred', size = 3)
p + annotate('text', x = 3, y = 48, label = 'Group 1', alpha = .1) +
  geom_text(x = 4.5, y = 66, label = 'Group 2', alpha = .1)
p + annotate('text', x = -Inf, y = Inf, label = 'Upper left', hjust = -.2, vjust = 2) +
  annotate('text', x = mean(range(faithful$eruptions)), y = -Inf, label = 'Bottom middle', vjust = -.4)

# add mathmatical expression ----------------------------------------------

p <- data.frame(x = c(-3, 3)) %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dnorm)
p + annotate('text', x = 2, y = 0.3, parse = TRUE, label = 'frac(1, sqrt(2 * pi)) * e ^ {-x^2 / 2}')
p + annotate('text', x = 0, y = 0.05, parse = TRUE, label = "'Function:' * y == frac(1, sqrt(2 * pi)) * e ^ {- x ^ 2 / 2}")

# add line ----------------------------------------------------------------

p <- heightweight %>% 
  ggplot(aes(x = ageYear, y = heightIn, colour = sex)) +
  geom_point()
# add horizontal line and vertical line 
p + geom_hline(yintercept = 60) +
  geom_vline(xintercept = 14, alpha = .5)
p + geom_abline(intercept = 37.4, slope = 1.75)

library('plyr')
hw_means <- heightweight %>% 
  ddply('sex', summarise, heightIn = mean(heightIn))
hw_means
p + geom_hline(aes(yintercept = heightIn, colour = sex), 
               data = hw_means, linetype = 'dashed', size = 1)