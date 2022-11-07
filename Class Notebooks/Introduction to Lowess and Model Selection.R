library(mosaic)
library(tidyverse)

plot(gasbill ~ month, data=Utilities)
lines(lowess(Utilities$month, Utilities$gasbill), col="red")


lm.gas <- lm(gasbill ~ month, data=Utilities)
plot(lm.gas, which=1)

lm.gas.quad <- lm(gasbill ~ month + I(month^2), data=Utilities)
b <- coef(lm.gas.quad)
summary(lm.gas.quad)

ggplot(Utilities, aes(x=month, y=gasbill))+
  geom_point()+
  geom_smooth(method = "loess", formula = y~x, se=T)+
  stat_function(fun = function(x) b[1] + b[2]*x + b[3]*x^2)


loess.gas <- loess(gasbill ~ month, data=Utilities)
summary(loess.gas)


predict(loess.gas, data.frame(month=9))
