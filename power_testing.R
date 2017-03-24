
power.prop.test(
  n = NULL,
  p1 = p[1], 
  p2 = p[1] * 1.1, 
  power = 0.8, 
  alternative = 'one.sided')$n

x = c(2804,6794)
n = c(19063,18747)
p = x/n

prop.test(
  x = x, 
  n = n, 
  alternative = 'less'
  )
