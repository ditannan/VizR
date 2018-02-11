
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

# add smooth line to a existed scatter plot -------------------------------

model <- heightweight %>% 
  lm(heightIn ~ ageYear + I(ageYear^2), .)

rng <- range(heightweight$ageYear, na.rm = TRUE)
predicted <- data.frame(ageYear = seq(rng[1], rng[2], length.out = 100))
predicted$heightIn <- predict(model, predicted)
predicted %>% glimpse()
sp <- heightweight %>% 
  ggplot(aes(ageYear, heightIn)) +
  geom_point(colour = 'grey50')
sp + geom_line(data = predicted, size = 1) +
  stat_smooth(method = lm, se = FALSE, colour = 'red')

# 切割数据集并拟合
## 生成预测数据集函数
predictvals <- function(mdl, xvar, yvar, xrange = NULL, samples = 100, ...) {
  if (is.null(xrange)) {
    if (any(class(mdl) %in% c('lm', 'glm')))
      xrng <- range(mdl$model[[xvar]])
    else if (any(class(mdl) %in% 'loess'))
      xrng <- range(mdl$x)
  }
  newdata <- data.frame(x = seq(xrng[1], xrng[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(mdl, newdata = newdata, ...)
  newdata
}
# lm() loess()
modlinear <- lm(heightIn ~ ageYear, data = heightweight)
modloess <- loess(heightIn ~ ageYear, data = heightweight)
lm_predicted <- predictvals(modlinear, 'ageYear', 'heightIn')
loess_predicted <- predictvals(modloess, 'ageYear', 'heightIn')

sp + geom_line(data = lm_predicted, colour = 'red', size = .8) +
   geom_line(data = loess_predicted, colour = 'blue', size = .8)
fitlogistic <- glm(classn ~ V1, data = b, family = binomial)
glm_predicted <- predictvals(fitlogistic, 'V1', 'classn', type = 'response')

b %>% 
  ggplot(aes(V1, classn)) +
  geom_point(position = position_jitter(width = .3, height = .08), 
             alpha = .4, shape = 21, size = 1.5) +
  geom_line(data = glm_predicted, colour = '#1177FF', size = 1)


# add model-fit-lines -----------------------------------------------------

make_model <- function(data) {
  lm(heightIn ~ ageYear, data)
}
library(plyr)
models <- dlply(heightweight, 'sex', .fun = make_model)
predvals <- ldply(models, .fun = predictvals, xvar = 'ageYear', yvar = 'heightIn')
heightweight %>% 
  ggplot(aes(ageYear, heightIn, colour = sex)) +
  geom_point() + 
  facet_grid(. ~ sex) +
  geom_line(data = predvals)

# add r-square ------------------------------------------------------------

model <- heightweight %>% 
  lm(heightIn ~ ageYear, .)
pred <- predictvals(model, 'ageYear', 'heightIn')
sp <- heightweight %>% 
  ggplot(aes(ageYear, heightIn)) +
  geom_point() +
  geom_line(data = pred)
sp + annotate('text', label = 'r^2 == 0.42', parse = TRUE, x = 16.5, y = 52)

eqn <- as.character(as.expression(
  substitute(italic(y) == a + b * italic(x) * ',' ~~ italic(r)^2 ~ '=' ~ r2,
             list(a = format(coef(model)[1], digits = 3),
                  b = format(coef(model)[2], digits = 3),
                  r2 = format(summary(model)$r.square, digits = 2)
                  )
             )
))
parse(text = eqn)
sp + annotate('text', label = eqn, parse = TRUE, x = Inf, y = -Inf, 
              hjust = 1.1, vjust = -.5)

# add labels to scatter plot ----------------------------------------------

ctries <- countries %>% 
  filter(Year == 2009 & healthexp > 2000)
ct <- ctries %>% 
  ggplot(aes(healthexp, infmortality)) +
  geom_point()
ct + annotate('text', x = 4350, y = 5.4, label = 'Canada') +
  annotate('text', x = 7400, y = 6.8, label = 'USA')
ct + geom_text(aes(y = infmortality + 0.1, label = Name), size = 3, vjust = 0)

# 气泡图 ---------------------------------------------------------------------

cdat <- countries %>% 
  filter(Year == 2009 & Name %in% c('Canada', 'Ireland', 'United Kingdom', 
                                    'United States', 'New Zealand', 'Iceland',
                                    'Japan', 'Luxembourg', 'Netherlands', 'Switzerland'))
p <- cdat %>% 
  ggplot(aes(healthexp, infmortality, size = GDP)) +  ## 映射给直径
  geom_point(shape = 21, colour = 'black', fill = 'cornsilk')
p + scale_size_area(max_size = 15) ## map to area

hec <- HairEyeColor[, , 'Male'] + HairEyeColor[, , 'Female']
hec1 <- reshape2::melt(hec, value.name = 'count')
hec1 %>% 
  ggplot(aes(Eye, Hair)) +
  geom_point(aes(size = count), shape = 21, colour = 'black', fill = 'cornsilk') +
  scale_size_area(max_size = 20, guide = FALSE) +
  geom_text(aes(y = as.numeric(Hair) - sqrt(count) / 22, label = count), 
            vjust = 1, colour = 'grey60', size = 4)

# scatter-matrix plot -----------------------------------------------------

c2009 <- countries %>% 
  filter(Year == 2009) %>% 
  select(Name, GDP, laborrate, healthexp, infmortality)
pairs(c2009[, 2 : 5])






