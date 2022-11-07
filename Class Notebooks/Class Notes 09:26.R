


library(car)


df_orange <- Orange
df_loblolly <- Loblolly
df_mtcars <- mtcars 


par(mfrow=c(2,1))

plot(circumference ~ age, data = Orange) 
o.lm <- lm(circumference ~ age, data = Orange)
summary(o.lm)
abline(o.lm)

plot(o.lm, which =1)
qqPlot(o.lm$residuals)
plot(o.lm$residuals, col=as.factor(Tree))




View(Loblolly)
?Loblolly

plot(height ~ age, data = Loblolly)
l.lm <- lm(height ~ age, data = Loblolly)
summary(l.lm)
abline(l.lm)


plot(l.lm, which=1)



plot(mpg ~ qsec, data=mtcars)
c.lm <- lm(mpg ~ qsec, data=mtcars)
summary(c.lm)
abline(c.lm)

plot(c.lm, which=1)
