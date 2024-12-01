
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
