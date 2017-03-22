x <- seq(-5,5,0.01)
a <- dnorm(x, mean = 0, sd = 1)
X = data.frame(x,a)

library(ggplot2)
library(dplyr)
library(govstyle)

X %>%
  ggplot +
  aes(x = x, y = a) +
  geom_line() +
  theme_gov(base_colour = '#000000') +
  theme(
    plot.background = element_rect(fill = '#ebebebff'),
    axis.line.y = element_blank()
    ) +
  ggtitle('Standard normal distribution') +
  xlab('Value') +
  ylab('Frequency') +
  geom_vline(xintercept = 0)

