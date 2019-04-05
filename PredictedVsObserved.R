

dat <- read.table(file = 'PredictedVsObservedTemps.csv', 
                  header = TRUE,
                  sep = ",")

names(dat)

plot(dat$Measured_1, dat$Predicted_1)
points(dat$Measured_2, dat$Predicted_2, col = 'red')

lm1 <- lm(Predicted_1 ~ Measured_1, data = dat)
summary(lm1)

lm2 <- lm(Predicted_2 ~ Measured_2, data = dat)
summary(lm2)

names(lm1)
var(lm1$residuals)
var(lm2$residuals)
