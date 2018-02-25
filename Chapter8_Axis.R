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

# 反转坐标轴 -------------------------------------------------------------------

PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot() +
  scale_y_reverse() ## equivalent to ylim(6.5, 3.5)
# set range for reversed y
PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot() +
  scale_y_reverse(limits = c(8, 0))

# x轴离散变量顺序 ----------------------------------------------------------------

p <- PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot()
p + scale_x_discrete(limits = c('trt1', 'ctrl', 'trt2'))
# 保留部分组
p + scale_x_discrete(limits = c('trt1', 'trt2'))
# 反转项目顺序
p + scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

# x and y缩放比例 -------------------------------------------------------------

sp <- marathon %>% 
  ggplot(aes(Half, Full)) +
  geom_point()
sp + coord_fixed()
sp + coord_fixed() +
  scale_y_continuous(breaks = seq(0, 420, 30)) +
  scale_x_continuous(breaks = seq(0, 420, 30))
# 缩放为1 : 2的散点图
sp + coord_fixed(ratio = 1 / 2) +
  scale_y_continuous(breaks = seq(0, 420, 30)) +
  scale_x_continuous(breaks = seq(0, 420, 15))

# 设置刻度线位置 -----------------------------------------------------------------

PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(4, 4.25, 4.5, 5, 6, 8))
# 离散型设定breaks和limits
PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot() +
  scale_x_discrete(limits = c('trt2', 'ctrl'), breaks = 'ctrl')

# 移除刻度线和标签 -------------------------------------------------------------------

p <- PlantGrowth %>% 
  ggplot(aes(group, weight)) +
  geom_boxplot()
# 移除标签
p + theme(axis.text.y = element_blank())
# 移除刻度线
p + theme(axis.ticks.x = element_blank())
# 移除刻度线、标签、网格线
p + scale_y_continuous(breaks = NULL)

# 修改刻度标签文本 ----------------------------------------------------------------

hwp <- heightweight %>% 
  ggplot(aes(ageYear, heightIn)) +
  geom_point()
hwp + scale_y_continuous(breaks = c(50, 56, 60, 66, 72), 
                         labels = c('Tiny', 'Really\nshort', 'Short', 'Medium', 'Tallish'))
# 修改数据标签格式
hwp + scale_y_continuous(labels = scales::dollar)
