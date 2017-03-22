library(govstyle)
par(bg ='#ebebebff')

# need for arrow()
m1 <- 0  # mu H0
sd1 <- 1.5 # sigma H0
m2 <- 3.5 # mu HA
sd2 <- 1.5 # sigma HA

z_crit <- qnorm(1-(0.05/2), m1, sd1)

# set length of tails
min1 <- m1-sd1*4
max1 <- m1+sd1*4
min2 <- m2-sd2*4
max2 <- m2+sd2*4          
# create x sequence

x <- seq(min(min1,min2), max(max1, max2), .01)

# generate normal dist #1
y1 <- dnorm(x, m1, sd1)
# put in data frame
df1 <- data.frame("x" = x, "y" = y1)
# generate normal dist #2
y2 <- dnorm(x, m2, sd2)
# put in data frame
df2 <- data.frame("x" = x, "y" = y2)

# Alpha polygon
y.poly <- pmin(y1,y2)
poly1 <- data.frame(x=x, y=y.poly)
poly1 <- poly1[poly1$x >= z_crit, ] 
poly1<-rbind(poly1, c(z_crit, 0))  # add lower-left corner

# Beta polygon
poly2 <- df2
poly2 <- poly2[poly2$x <= z_crit,] 
poly2<-rbind(poly2, c(z_crit, 0))  # add lower-left corner

# power polygon; 1-beta
poly3 <- df2
poly3 <- poly3[poly3$x >= z_crit,] 
poly3 <-rbind(poly3, c(z_crit, 0))  # add lower-left corner

# combine polygons. 
poly1$id <- 3 # alpha, give it the highest number to make it the top layer
poly2$id <- 2 # beta
poly3$id <- 1 # power; 1 - beta
poly <- rbind(poly1, poly2, poly3)
poly$id <- factor(poly$id,  labels=c("power","beta","alpha"))


plot(
  seq(-6,10,length = 10),
  seq(-0.05, 0.35,length = 10),
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n",
  ann = FALSE
)

# add polygons
polygon(
  poly3, 
  col = gov_cols['turquoise'],
) # 1-beta

polygon(
  poly2, 
  col = gov_cols['brown']
) # beta

polygon(
  poly1, 
  col = gov_cols['mellow_red'], 
  angle = 0
) # alpha

# add h_a dist
lines(
  df2, 
  lwd = 1
)

# add h_0 dist
lines(
  df1, 
  lwd = 1
)

### annotations
# h_0 title
text(
  m1, 
  0.3, 
  expression(H[0]), 
  cex = 1.5
)

# h_a title
text(
  m2, 
  0.3, 
  expression(H[a]), 
  cex = 1.5
)

# beta annotation
arrows(
  x0 = -1, 
  y0 = 0.045, 
  x1 = 1, 
  y1 = 0.01, 
  lwd = 1, 
  length = 0.15
)

text(
  -1.2, 
  0.045, 
  expression(beta), 
  cex = 1.5
)

# beta annotation
arrows(
  x0 = 4, 
  y0 = -0.01, 
  x1 = 3.5, 
  y1 = 0.01, 
  lwd = 1, 
  length = 0.15
)

text(
  x = 4.1, 
  y = -0.015, 
  expression(alpha/2), 
  cex = 1.5
)

# 1-beta 
arrows(
  x0 = 6, 
  y0 = 0.15, 
  x1 = 5, 
  y1 = 0.1, 
  lwd = 1, 
  length = 0.15
)

text(
  x = 7, 
  y = 0.155, 
  expression(paste(1-beta)), 
  cex = 1.5
)

# show z_crit; start of rejection region
abline(
  v = z_crit
)
# add bottom line
abline(
  h = 0
)