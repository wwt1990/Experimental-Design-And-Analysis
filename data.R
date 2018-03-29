# The experiment lasted for 7 weeks. After the feeding trial, fish were challenged
# with Streptococcus iniae and mortality was recorded for within 8 days. 

# The changes in levels of fish blood parameters will give an insight of fish’s health status.
# red blood cell: transport of O2 and CO2 in blood 
# white blood cell: immune function
# hemoglobin hematocrit: a protein for carrying oxygen
# Temperature is one of the main environment conditions which affect the fish reproduction and immune system. 20℃-34℃
# The intake of proteins is conducive for tissue renewal, repair and metabolism in fish . 25%-50%
  


data <- read.table("data.txt", header = TRUE)
data
attach(data)

library(rsm)
y_RBC.fit <- rsm(y_RBC ~ SO(x1, x2)+TWI(x1,x2))
summary(y_RBC.fit)
par(mfrow=c(1,2))
plot(fitted.values(y_RBC.fit), rstudent(y_RBC.fit), xlab = "fitted values", ylab = "Studentized residuals", main = "Residuals vs. Predicted", pch=19)
abline(0,0)
qqnorm(rstudent(y_RBC.fit),xlab = "Residuals", ylab = "Normal % probability", main = "Normal plot of residuals",pch=19)
abline(0,1)
par(mfrow=c(1,1))

y_WBC.fit <- rsm(y_WBC ~ SO(x1, x2)+TWI(x1,x2))
summary(y_WBC.fit)
par(mfrow=c(1,2))
plot(fitted.values(y_WBC.fit), rstudent(y_WBC.fit), xlab = "fitted values", ylab = "Studentized residuals", main = "Residuals vs. Predicted", pch=19)
abline(0,0)
qqnorm(rstudent(y_WBC.fit),xlab = "Residuals", ylab = "Normal % probability", main = "Normal plot of residuals",pch=19)
abline(0,1)
par(mfrow=c(1,1))

y_Hb.fit <- rsm(y_Hb ~ SO(x1, x2)+TWI(x1,x2))
summary(y_Hb.fit)
par(mfrow=c(1,2))
plot(fitted.values(y_Hb.fit), rstudent(y_Hb.fit), xlab = "fitted values", ylab = "Studentized residuals", main = "Residuals vs. Predicted", pch=19)
abline(0,0)
qqnorm(rstudent(y_Hb.fit),xlab = "Residuals", ylab = "Normal % probability", main = "Normal plot of residuals",pch=19)
abline(0,1)
par(mfrow=c(1,1))

y_Mortality.fit <- rsm(y_Mortality ~ SO(x1, x2)+TWI(x1,x2))
summary(y_Mortality.fit)
par(mfrow=c(1,2))
plot(fitted.values(y_Mortality.fit), rstudent(y_Mortality.fit), xlab = "fitted values", ylab = "Studentized residuals", main = "Residuals vs. Predicted", pch=19)
abline(0,0)
qqnorm(rstudent(y_Mortality.fit),xlab = "Residuals", ylab = "Normal % probability", main = "Normal plot of residuals",pch=19)
abline(0,1)
par(mfrow=c(1,1))

contour(y_RBC.fit, ~x1*x2,image=FALSE, xlim=c(-1, 1), ylim=c(-1,1))
#points(x1, x2, pch = 16, col = "red", cex = 2)
contour(y_WBC.fit, ~x1*x2,image=FALSE, add=TRUE, xlim=c(-1, 1), ylim=c(-1,1),col="red")
#points(x1, x2, pch = 16, col = "red", cex = 2)
contour(y_Hb.fit, ~x1*x2,image=FALSE, add=TRUE, xlim=c(-1, 1), ylim=c(-1,1),col="blue")
#points(x1, x2, pch = 16, col = "red", cex = 2)
contour(y_Mortality.fit, ~x1*x2,image=FALSE, add=TRUE, xlim=c(-1, 1), ylim=c(-1,1),col="green")
#points(x1, x2, pch = 16, col = "red", cex = 2)

steepest (y_RBC.fit, dist = seq(0, 5, by = .5), descent = FALSE)
steepest (y_WBC.fit, dist = seq(0, 5, by = .5), descent = FALSE)
steepest (y_Hb.fit, dist = seq(0, 5, by = .5), descent = FALSE)
steepest (y_Mortality.fit, dist = seq(0, 5, by = .5), descent = FALSE)

canonical.path(y_RBC.fit)




sd_RBC.fit<-rsm(sd_RBC ~ SO(x1, x2)+TWI(x1,x2))
summary(sd_RBC.fit)
par(mfrow=c(1,2))
plot(fitted.values(sd_RBC.fit), rstudent(sd_RBC.fit), xlab = "fitted values", ylab = "Studentized residuals", main = "Residuals vs. Predicted", pch=19)
abline(0,0)
qqnorm(rstudent(sd_RBC.fit),xlab = "Residuals", ylab = "Normal % probability", main = "Normal plot of residuals",pch=19)
abline(0,1)
par(mfrow=c(1,1))

#transform
lnsd_RBC.fit<-rsm(I((sd_RBC))^(-1)~SO(x1,x2)+TWI(x1,x2))
summary(lnsd_RBC.fit)

par(mfrow=c(1,2))
qqnorm(rstandard(lnsd_RBC.fit), pch=19)
abline(0,1)
plot(fitted(lnsd_RBC.fit), rstandard(lnsd_RBC.fit), xlab = "transformed variance response", ylab = "standardized residual", pch=19)
abline(0,0)
par(mfrow=c(1,1))





contour(y_RBC.fit, ~x1*x2,image=FALSE, xlim=c(-1, 1), ylim=c(-1,1))
points(x1, x2, pch = 16, col = "red", cex = 2)
contour(lns2.fit, ~x1*x2,image=FALSE, add=TRUE, xlim=c(-1, 1), ylim=c(-1,1),col="red")
points(x1, x2, pch = 16, col = "red", cex = 2)



