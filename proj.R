library(data.table)
library(ggplot2)


# load and prepare data
data(mtcars)
dt= mtcars
dt$car_make= rownames(mtcars)
dt= data.table(dt)
dt[, am:= as.factor(am)]



# basic plots to see whats going on
coplot(mpg ~ cyl | am, data = dt, panel = panel.smooth,
       xlab = "Mileage vs Cylinder count given a transmission mode")

boxplot(mpg ~ am, data= dt, notch= T,
        col=(c("gold","darkgreen")),
        main="Mileage vs Transmission", 
        ylab= "Mileage",
        xlab="Transmission Mode: 0 (auto) and 1 (manual)") 

pairs(mtcars)



# t-test to compare mpg between transmission
result= t.test(mpg ~ am , data= dt, paired= F, var.equal = F)
result



# plot modeled output
plotAnalyze= function(dt, f0, var)
{
  dt= copy(dt)
  setnames(dt, var, 'var')
  dt0= dt[am==0]
  dt1= dt[am==1]
  
  plot(x= dt$var, y= dt$mpg, type='n', frame=F, ylim= c(5,35),
       main = paste('Effect of Transmission after adjusting for', var), 
       ylab= 'Mileage', xlab= var)
  
  abline(h= mean(dt0$mpg), lwd= 2)
  abline(h= mean(dt1$mpg), lwd= 2)
  abline(coef(f0)[1], coef(f0)[2], lwd= 2)
  abline(coef(f0)[1] + coef(f0)[3], coef(f0)[2], lwd= 2)
  
  points(dt0$var, dt0$mpg, pch= 21, col= 'black', bg= 'lightblue', cex= 2)
  points(dt1$var, dt1$mpg, pch= 21, col= 'black', bg= 'salmon', cex= 2)
  legend('topright', legend= c('automatic', 'manual'), 
         cex= 0.8, fill= c('lightblue', 'salmon'))
}



# linear model of mpg
f0= lm(mpg ~ wt + factor(am), data= dt)
summary(f0)
plotAnalyze(dt, f0, 'wt')

f1= lm(mpg ~ hp + factor(am), data= dt)
summary(f1)
plotAnalyze(dt, f1, 'hp')

f2= lm(mpg ~ disp + factor(am), data= dt)
summary(f2)
plotAnalyze(dt, f2, 'disp')

f3= lm(mpg ~ drat + factor(am), data= dt)
summary(f3)
plotAnalyze(dt, f3, 'drat')

plot(predict(f0), resid(f0), pch= 21, col= 'black', bg= 'salmon', cex= 2)
abline(h= 0, lwd= 2, col= 'lightblue')
