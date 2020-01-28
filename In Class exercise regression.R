
bank = read.csv('https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv', header = TRUE)



multi.fit = lm(balance~job+education, data=bank)
summary(multi.fit)

#Multiple Regression Residual Plots
layout(matrix(c(1,2,3,4),2,4,byrow=T))
plot(multi.fit$fitted, rstudent(multi.fit),
     main="Multi Fit Job and Education Residuals",
     xlab="Predictions",ylab="Residuals",
     ylim=c(-2.5,2.5))
abline(h=0, lty=2)
plot(bank$education, multi.fit$resid,
     main="Residuals by Education Level",
     xlab="Education",ylab="Residuals")
abline(h=0,lty=2)
hist(multi.fit$resid,main="Histogram of Residuals")
qqnorm(multi.fit$resid)
qqline(multi.fit$resid)
