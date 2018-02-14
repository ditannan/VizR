
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

# add segment or arrow ---------------------------------------------------------------

p <- ggplot(subset(climate, Source == 'Berkeley'), aes(Year, Anomaly10y)) +
  geom_line()
p + annotate('segment', x = 1950, xend = 1980, y = -.25, yend = -.25)
library('grid')
p + annotate('segment', x = 1850, xend = 1820, y = -.8, yend = -.95, 
             colour = 'blue', size = 2, arrow = arrow()) +
  annotate('segment', x = 1950, xend = 1980, y = -.25, yend = -.25, 
           arrow = arrow(ends = 'both', angle = 90, length = unit(.2, 'cm')))
# ?arrow

# 添加矩形阴影 ------------------------------------------------------------------

# annotate('rect')
p <- ggplot(subset(climate, Source == 'Berkeley'), aes(Year, Anomaly10y)) +
  geom_line()
p + annotate('rect', xmin = 1950, xmax = 1980, ymin = -1, ymax = 1, alpha = .2, fill = 'blue')

# hightlight some elements ------------------------------------------------

pg <- PlantGrowth
pg$hl <- pg %$% ifelse(group == 'trt2', 'yes', 'no')
pg %>% 
  ggplot(aes(group, weight, fill = hl)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey85', '#FFDDCC'), guide = FALSE)
# equivalent to
pg %>% 
  ggplot(aes(group, weight, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey85', 'grey85', '#FFDDCC'), guide = FALSE)

# add errorbar ------------------------------------------------------------

ce <- cabbage_exp %>% subset(Cultivar == 'c39')
# add error bar for bar
ce %>% 
  ggplot(aes(Date, Weight)) +
  geom_bar(fill = 'white', colour = 'black', stat = 'identity') +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), width = .2)
# add erroe or line
ce %>% 
  ggplot(aes(Date, Weight)) +
  geom_line(aes(group = 1)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), width = .2)
# multiple bar chart
cabbage_exp %>% 
  ggplot(aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), 
                position = position_dodge(.9), width = .2)

pd <- position_dodge(.3)
cabbage_exp %>% 
  ggplot(aes(Date, Weight, colour = Cultivar, group = Cultivar)) +
  geom_errorbar(aes(ymin = Weight -se, ymax = Weight + se), width = .2, position = pd, colour = 'black') +
  geom_line(position = pd) +
  geom_point(position = pd, size = 2.5)
cabbage_exp

# add annotate to facet plot ----------------------------------------------

p <- mpg %>% 
  ggplot(aes(displ, hwy)) +
  geom_point() +
  facet_grid(. ~ drv)
f_labels <- data.frame(drv = c('4', 'f', 'r'),
                       label = c('4wd', 'Front', 'Rear'))
p + geom_text(x = 6, y = 40, aes(label = label), data = f_labels)  
p + annotate('text', x = 6, y = 40, label = 'Label text')

# add mathmatical expression and r^2 to facet plot
lm_labels <- function(dat) {
  mod <- lm(hwy ~ displ, data = dat)
  formula <- sprintf('italic(y) == %.2f %+.2f * italic(x)',
                     round(coef(mod)[1], 2), round(coef(mod)[2], 2))
  r <- cor(dat$displ, dat$hwy)
  r2 <- sprintf('italic(R^2) == %.2f', r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}
labels <- ddply(mpg, 'drv', lm_labels)
labels
p + geom_smooth(method = lm, se = FALSE) +
  geom_text(x = 3, y = 40, aes(label = formula), data = labels, parse = TRUE, hjust = 0) +
  geom_text(x = 3, y = 35, aes(label = r2), data = labels, parse = TRUE, hjust = 0)
# add r2 equivalent to
labels1 <- ddply(mpg, 'drv', summarise, r2 = cor(displ, hwy)^2)
labels1$r2 <- sprintf('italic(R^2) == %.2f', labels1$r2)
p + geom_smooth(method = lm, se = FALSE) +
  geom_text(aes(label = r2), data = labels1, x = 5, y = 35, parse = TRUE, size = 3)