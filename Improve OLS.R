#Linh Anh Le & Jackson Amodeo
#11/14/2019
#improving OLS

#chosen regression model
m8 <- lm(tot_erm ~ welc_school+af_trmt+interact+white+black+hispanic+mob_rate+att_rate+LEP+IEP+LI,datatot)
summary(m8)
par(mfrow=c(2,2))
plot(m8)
install.packages("coefplot")
library(coefplot)
coefplot(m8, intercept=FALSE, outerCI=1.96)

#correct heteroskedasticity problem
install.packages('lmtest')
install.packages('sandwich')
library(lmtest) 
library(sandwich)
coeftest(m8, vcov = vcovHC(m8, type="HC1"))

#delete outliers
data1 = datatot[!datatot$ID == "150162990252160",]
#similar regression like m8 but on data1
m8.1 <- lm(tot_erm ~ welc_school+af_trmt+interact+white+black+hispanic+mob_rate+att_rate+LEP+IEP+LI,data1)
summary(m8.1)

#create new variable as a combination of black and hispanic
datatot$bl_hs <- datatot$black+datatot$hispanic
m10 <- lm(tot_erm ~ welc_school+af_trmt+interact+white+bl_hs+mob_rate+att_rate+LEP+IEP+LI,datatot)
summary(m10)

par(mfrow=c(2,2))
plot(m8.1)

plot(m8)

m8.2 <- lm(tot_erm ~ welc_school+af_trmt+interact+white+black+mob_rate+att_rate+LEP+IEP+LI,datatot)
summary(m8.2)

library('car')
vif(m8.2)

#create new variable for minority
datatot$minority <- datatot$black + datatot$hispanic
m8c <- lm(tot_erm ~ welc_school+af_trmt+interact+white+minority+mob_rate+att_rate+LEP+IEP+LI,datatot)
summary(m8c)

#Jackson's code for VIF and Anova F-test

#rechecking SE and Rsq for each variable
#reg <- lm(tot_erm ~ white , x)
#summary(reg)
reg5 <- lm(tot_erm ~ white + black + hispanic +
             mob_rate + att_rate + IEP + LEP + LI +
             welc_school + af_trmt + interact, x)
summary(reg5)
#reg 5 but without outlier "Disney" school -> Robustnes section
# Also, split data, 
library("dplyr")
x<-x%>%filter(ID!="150162990252160")
reg5 <- lm(tot_erm ~ white + black + hispanic +
             mob_rate + att_rate + IEP + LEP + LI +
             welc_school + af_trmt + interact, datatot)
summary(reg5)
#install.packages("ggfortify")
library(ggfortify)
autoplot(reg5)
#F-test for sign of black + hispanic b/c suspct colinearity
reg6 <- lm(tot_erm ~ white + black + 
             mob_rate + att_rate + IEP + LEP + LI +
             welc_school + af_trmt + interact, datatot)
summary(reg6)
anova(reg6, reg5)
reg7 <- lm(tot_erm ~ white + hispanic + 
             mob_rate + att_rate + IEP + LEP + LI +
             welc_school + af_trmt + interact, x)
anova(reg7, reg5)
#compare the two models, romoving black, removing hispanic
#make table of values and compare... multicolinearity
#VIF: Black and hispanic were >10 VIF, library("car")
#vif(data_tot) -> black now less than 10
install.packages("car")
library("car") #testing anova between groups
vif(reg6)
vif(reg5)
vif(reg7)
summary(reg6)
summary(reg7)