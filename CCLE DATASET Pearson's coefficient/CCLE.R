setwd("C:\\Users\\KIIT\\Desktop\\Dissertation")

getwd()

#Read data

d <- read.csv("CCLE.csv")

#Scatter plot

plot(d$X76GS,d$KS,xlab = "EMT score(76GS)", ylab = "EMT score (KS)" ,col = "red" ,pch =1 ,cex =1)


#Default model

cor(d$X76GS,d$KS , method = "pearson")

#response/dependent
#x= samples and y=EMT.score

reg <- lm(X76GS ~ KS, data = d)

#Get the summary of regression
#Summary will depects what are the value of coefficients,t-test,R squared values


summary(reg)

#lm(formula = X76GS ~ KS, data = d)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.5095 -1.1585  0.1871  0.2161  5.9106 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  -0.1652     0.7244  -0.228  0.82475   
#KS           -0.9823     0.2157  -4.555  0.00138 **
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.402 on 9 degrees of freedom
#Multiple R-squared:  0.6974,	Adjusted R-squared:  0.6638 
#F-statistic: 20.74 on 1 and 9 DF,  p-value: 0.001377

#R squard value describes goot fit for the data for EMT score and samples
#t-test for CCLE  data 0.82475 ,which is not satistically significant p value<0.05

#confidence interval for estimated co efficients:

confint(reg, level = 0.95)

#               2.5 %     97.5 %
#(Intercept) -1.803862  1.4735493
#KS          -1.470126 -0.4943826

#Overlay the regression line-------

abline(reg ,col = "red" ,lwd = 2)

#Read data 

#EMT score (76GS) vs EMT score (MLR)

#Scatter plot

plot(d$X76GS,d$MLR,xlab="EMT score (76GS)" ,ylab = "EMT score(MLR)",col = "red" ,pch =1, cex = 2)

cor(d$X76GS,d$MLR , method = "pearson")

#> cor(d$X76GS,d$MLR , method = "pearson")
#[1] 0.05837639

reg <- lm(X76GS ~ MLR, data = d)

summary(reg)

#lm(formula = X76GS ~ MLR, data = d)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.8356 -1.2874  0.5179  0.5355  8.0466 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)
#(Intercept)  -0.4168     1.8007  -0.231    0.822
#MLR           0.3516     2.0045   0.175    0.865

#Residual standard error: 4.36 on 9 degrees of freedom
#Multiple R-squared:  0.003408,	Adjusted R-squared:  -0.1073 
#F-statistic: 0.03078 on 1 and 9 DF,  p-value: 0.8646

confint(reg, level = 0.95)

#2.5 %   97.5 %
#(Intercept) -4.490247 3.656645
#MLR         -4.182836 4.886126

abline(reg ,col = "red" ,lwd = 2)


#Plot for EMT (MLR) and EMT score (KS)

plot(d$MLR,d$KS, xlab = "EMT score(MLR)", ylab = "EMT score (KS)" ,col = "red" ,pch =1 ,cex =1)

cor(d$MLR,d$KS , method = "pearson")

#[1] -0.2070254

reg <- lm(MLR ~ KS, data = d)

summary(reg)

#Call:
#lm(formula = MLR ~ KS, data = d)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-0.6092 -0.5799 -0.5317  0.5814  0.9610 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.61539    0.21388   2.877   0.0183 *
#  KS          -0.04042    0.06368  -0.635   0.5413  
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7093 on 9 degrees of freedom
#Multiple R-squared:  0.04286,	Adjusted R-squared:  -0.06349 
#F-statistic: 0.403 on 1 and 9 DF,  p-value: 0.5413

confint(reg, level = 0.95)
#                 2.5 %    97.5 %
#(Intercept)  0.1315577 1.0992256
#KS          -0.1844695 0.1036223

abline(reg ,col = "red" ,lwd = 2)

#CCLE EMT score value of ABS_Coef_Var of 76GS and ABS_Coef_Var of  KS mentioned in given paper

plot(d$ABS_Coef_Var,d$ABS_Coef_Var.1,xlab = "EMT score(76GS)", ylab = "EMT score (KS)" ,col = "red" ,pch =1 ,cex =1)

cor(d$X76GS,d$KS , method = "pearson")

#[1] -0.8351126

reg <- lm(X76GS ~ KS, data = d)

summary(reg)

#Call:
#  lm(formula = X76GS ~ KS, data = d)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.5095 -1.1585  0.1871  0.2161  5.9106 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  -0.1652     0.7244  -0.228  0.82475   
#KS           -0.9823     0.2157  -4.555  0.00138 **
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.402 on 9 degrees of freedom
#Multiple R-squared:  0.6974,	Adjusted R-squared:  0.6638 
#F-statistic: 20.74 on 1 and 9 DF,  p-value: 0.001377

confint(reg, level = 0.100)

#                2.5 %     97.5 %
#(Intercept) -1.803862  1.4735493
#KS          -1.470126 -0.4943826

abline(reg ,col = "red" ,lwd = 2)
