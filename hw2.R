# q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
f= lm(y~x)
summary(f)

# q 3,4,5,6,9
data(mtcars)

f1= lm(mpg~wt, data= mtcars)
# f2= lm(mpg~wt-1, data= mtcars)

vals= c(mean(mtcars$wt),3000/1000)
predict(f1, newdata = data.frame(wt= vals))
coef(f1)[2]*2

r1= summary(f1)$r.squared
