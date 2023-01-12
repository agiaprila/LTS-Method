data <- read.delim("clipboard")
head(data)
tail(data)
str(datanew)

#MODEL
regModelku <- lm(Y~X1+X2+X3+X4+X5, trans1 = trans1)
summary(regModelku)

#uji asumsi
residual <- residuals(regModelku)
View(residual)

#Normlaitas
qqnorm(residual)
ks.test(residual,'pnorm',0,sd(residual)) # p value > dari alfa  maka normal

#homo kedastisitas
library(lmtest)
bptest(regModelku)

#Autokorelasi
dwtest(regModelku)

#multiko
library(car)
vif(regModelku)

#TRANSDORMASI
datanew <- sqrt(data)
head(datanew)
str(datanew)

#MODEL
model <- lm(Y~X1+X2+X3+X4+X5, data = datanew)
summary(model)

#Ulangi uji asumsi
#uji asumsi
residual <- residuals(model)
View(residual)

#Normlaitas
qqnorm(residual)
ks.test(residual,'pnorm',0,sd(residual)) # p value > dari alfa  maka normal

#homo kedastisitas
library(lmtest)
bptest(regModelku)

#Autokorelasi
dwtest(regModelku)

#multiko
library(car)
vif(regModelku)




