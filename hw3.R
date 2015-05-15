# q1,2,3,4
data(mtcars)
names(mtcars)
unique(mtcars$cyl)

fit1= lm(mpg ~ factor(cyl) + wt, data= mtcars)
summary(fit1)$coef

fit2= lm(mpg ~ factor(cyl), data=mtcars)
summary(fit2)$coef

fit3= lm(mpg ~ wt + factor(cyl) + wt:factor(cyl), data= mtcars)
summary(fit3)$coef
fit3 <- update(fit1, mpg~factor(cyl) + wt + wt*factor(cyl))
summary(fit3)$coef

fit4= lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit4)$coef

# q5,6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
ff= lm(y~x)
hat(x)
hatvalues(ff)
dfbetas(ff)
