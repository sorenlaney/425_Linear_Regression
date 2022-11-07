library(mosaic)
library(tidyverse)
library(car)

lm.u <- lm(gasbill ~ temp, data=Utilities)
summary(lm.u)
plot(lm.u, which=1)

pred.u <- predict(lm.u, data.frame(temp=30), interval="prediction")
pred.u60 <- predict(lm.u, data.frame(temp=60), interval="prediction")

boxCox(lm.u)


lm.u.ss <- lm(sqrt(sqrt(gasbill)) ~ temp, data=Utilities)
summary(lm.u.ss)
plot(lm.u.ss, which=1)

pred.u.ss <- predict(lm.u.ss, data.frame(temp=30), interval="prediction")^4
pred.u.ss60 <- predict(lm.u.ss, data.frame(temp=60), interval="prediction")^4


plot(gasbill ~ temp, data=Utilities, ylim=c(0,250))
abline(lm.u, col="hotpink")
b <- coef(lm.u.ss)
curve( (b[1] +b[2]*x)^4, add=TRUE, col="skyblue", lwd=2)
abline(v=30, lty=2, col="gray")
lines(c(30,30), c(pred.u.ss[2], pred.u.ss[3]), lwd=3, col="skyblue")
lines(c(30,30), c(pred.u[2], pred.u[3]), lwd=3, col="hotpink")
lines(c(60,60), c(pred.u60[2], pred.u60[3]), lwd=3, col="hotpink")
lines(c(60,60), c(pred.u.ss60[2], pred.u.ss60[3]), lwd=3, col="skyblue")


ggplot(Utilities, aes(x=temp, y=gasbill)) + 
  geom_point() +
  geom_smooth(method="lm", se=T, formula=y~x, color="hotpink") + 
  stat_function(fun=function(x) (b[1] + b[2]*x)^4, color="skyblue", lwd=2) +
  geom_vline(aes(xintercept=30), lty=2, color="gray") + 
  geom_segment(aes(x=30, xend=30, y=pred.u[2], yend=pred.u[3]), lwd=3, color="hotpink") + 
  geom_segment(aes(x=30, xend=30, y=pred.u.ss[2], yend=pred.u.ss[3]), lwd=3, color="skyblue") + 
  geom_segment(aes(x=60, xend=60, y=pred.u60[2], yend=pred.u60[3]), lwd=3, color="hotpink") + 
  geom_segment(aes(x=60, xend=60, y=pred.u.ss60[2], yend=pred.u.ss60[3]), lwd=3, color="skyblue") + 
  ylim(c(0,250))

