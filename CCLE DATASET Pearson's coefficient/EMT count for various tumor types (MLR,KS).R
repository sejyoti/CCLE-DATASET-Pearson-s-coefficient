setwd("C:\\Users\\KIIT\\Desktop\\Dissertation")

getwd()

#Read the data

d <- read.csv("E_M score in various tumor type.csv")


##Scatter plot
plot(d$Cancercelltypes,d$E.M_Count,xlab = "EMT score(MLR)", ylab = "EMT score (KS)" ,col = "red" ,pch =1 ,cex =1)


#Default model

cor(d$Cancercelltypes,d$EMT , method = "pearson")

# cor(d$Cancercelltypes,d$E.M_Count , method = "pearson")
#[1] 0.7604246

#response/dependent
#x= samples and y=EMT.score

reg <- lm(Cancercelltypes~ EMT , data = d)

#Get the summary of regression


summary(reg)
#lm(formula = Cancercelltypes ~ EMT, data = d)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-25.201  -9.346  -4.961   4.751  38.974 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  14.3081     3.0172   4.742 3.70e-05 ***
#  EMT           1.9058     0.2791   6.828 7.41e-08 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 14.8 on 34 degrees of freedom
#Multiple R-squared:  0.5782,	Adjusted R-squared:  0.5658 
#F-statistic: 46.62 on 1 and 34 DF,  p-value: 7.405e-08

confint(reg, level = 0.95)

#  (Intercept) 8.176348 20.439772
#EMT         1.338566  2.473129

#Overlay the regression line-------

abline(reg ,col = "red" ,lwd = 2)








