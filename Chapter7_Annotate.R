
# loading packages --------------------------------------------------------

library('gcookbook')
library('ggplot2')
library('magrittr')

p <- faithful %>% 
  ggplot(aes(eruptions, waiting)) +
  geom_point()
p + annotate('text', x = 3, y = 48, label = 'Group 1') +
  annotate('text', x = 4.5, y = 66, label = 'Group 2')
p + annotate('text', x = 3, y = 48, label = 'Group 1', family = 'serif',
             fontface = 'italic', colour = 'darkred', size = 3) +
  annotate('text', x = 4.5, y = 66, label = 'Group 2', family = 'serif',
           fontface = 'italic', colour = 'darkred', size = 3)
