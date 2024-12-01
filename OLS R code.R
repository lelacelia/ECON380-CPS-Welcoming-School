#Linh Anh Le
#11/9/18
#OLS regression


###
#create an interaction variable between the welc_school and the af_trmt variables
datatot <- read_excel("D:/Downloads/ECON 380/Final project data/datatot.xlsx", 
                      +     col_types = c("skip", "text", "numeric", 
                                          +         "numeric", "text", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric"))

#generate interaction variable
datatot$interaction <- datatot$welc_school * datatot$af_trmt

#add some values to white and hispanic ratio
#import dataset with 2018
datatot2018 <- read_excel("D:/Downloads/ECON 380/Final project data/datatot2018.xlsx")

#import dataset without 2018
datatotno2018 <- read_excel("D:/Downloads/ECON 380/Final project data/datatotno2018.xlsx", 
                            +     col_types = c("skip", "text", "numeric", 
                                                +         "numeric", "text", "numeric", "numeric", 
                                                +         "numeric", "numeric", "numeric", 
                                                +         "numeric", "numeric", "numeric", 
                                                +         "numeric", "numeric", "numeric", 
                                                +         "numeric", "numeric", "numeric", 
                                                +         "numeric", "numeric", "numeric", 
                                                +         "numeric", "numeric", "numeric", 
                                                +         "numeric"))
datatot <- merge(datatot2018,datatotno2018, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size","welc_school","af_trmt","interact"), all=TRUE)

#save the merged data set
library(xlsx)
write.xlsx(datatot,"D:/Downloads/ECON 380/Final project data/datatot.xlsx")

### ### ####
#OLS analysis
#Correlation checking
att_rate_vs_mobility_rate <- cor(datatot$att_rate,datatot$mob_rate,use = "complete.obs")
att_rate_vs_parinv <- cor(datatot$par_inv,datatot$att_rate,use = "complete.obs")
parinv_vs_mobility_rate <- cor(datatot$par_inv,datatot$mob_rate,use = "complete.obs")
white_vs_black <- cor(datatot$white,datatot$black,use = "complete.obs")
white_vs_hispanic <- cor(datatot$white,datatot$hispanic,use = "complete.obs")
black_vs_hispanic <- cor(datatot$black,datatot$hispanic,use = "complete.obs") ###very high
black_vs_mobility <- cor(datatot$black,datatot$mob_rate,use = "complete.obs")
hispanic_vs_mobility <- cor(datatot$hispanic,datatot$mob_rate,use = "complete.obs")
hispanic_vs_attrate <- cor(datatot$hispanic,datatot$att_rate,use = "complete.obs")
hispanic_vs_LEP <- cor(datatot$hispanic,datatot$LEP,use = "complete.obs")

library(ggplot2)
ggplot(datatot, aes(x=black, y=hispanic)) + geom_point(shape=18, color="blue")  + ggtitle("Graph: Correlation between the proportion of black and hispanic students")+
  geom_smooth(method=lm,color="darkred")

ggplot(datatot, aes(x=LEP, y=hispanic)) + geom_point(shape=18, color="blue")  + ggtitle("Graph: Correlation between the proportion of LEP and hispanic students")+
  geom_smooth(method=lm,color="darkred")

#regression models
m1 <- lm(tot_erm ~ welc_school+af_trmt+interact, datatot)
summary (m1)

m2 <- lm(tot_erm ~ welc_school+af_trmt+interact+black+hispanic, datatot)
summary (m2)

m3 <- lm(tot_erm ~ welc_school+af_trmt+interact+white, datatot)
summary(m3)

m4 <- lm(tot_erm ~ welc_school+af_trmt+interact+hispanic, datatot)
summary(m4)

m5 <- lm(tot_erm ~ welc_school+af_trmt+interact+black+white+hispanic, datatot)
summary(m5)

m6 <- lm(tot_erm ~ welc_school+af_trmt+interact+black+white+hispanic+par_inv,datatot)
summary(m6)

m7 <- lm(tot_erm ~ welc_school+af_trmt+interact+black+white+hispanic+par_inv+LI+IEP+LEP+att_rate+mob_rate,datatot)
summary(m7)

m8 <- lm(tot_erm ~ welc_school+af_trmt+interact+black+white+hispanic+LI+IEP+LEP+att_rate+mob_rate,datatot)
summary(m8)

m9 <- lm(tot_erm ~ welc_school+af_trmt+interact+black+white+hispanic+IEP+LEP+att_rate+mob_rate,datatot)
summary(m9)

m10 <- lm(tot_erm ~ welc_school+af_trmt+interact+black+hispanic,datatot)
summary(m10)

par(mfrow=c(2,2))
plot(m8)

#break

par(mfrow=c(2,2))
plot(m7)

#testing multicollinearity
install.packages('VIF')
library (VIF)
vif(m8)

round(cor(datatot))

df <- subset(datatot,select=-c(ID,name))
round(cor(df))

transform(datatot, tot_erm = as.numeric(tot_erm))
transform(datatot, black = as.numeric(black))
transform(datatot, white = as.numeric(white))
transform(datatot, hispanic = as.numeric(hispanic))
transform(datatot, LI = as.numeric(LI))
transform(datatot, IEP = as.numeric(IEP))
transform(datatot, LEP = as.numeric(LEP))
transform(datatot, par_inv = as.numeric(par_inv))
transform(datatot, mob_rate = as.numeric(mob_rate))
transform(datatot, att_rate = as.numeric(att_rate))

round(cor(datatot))

install.packages('car')
library(car)
vif(m8)

mean(vif(m8))

vif(m7)

#create bar graph for VIF
library(ggplot2)
VIF <- c(2.32, 2.394, 3.507,31.046,2.855,14.669,1.894,1.305,6.396,1.362,1.74) 
Independent_variables <- c("Welcoming school(dummy)", "After treatment(dummy)", "Interaction","Black","White","Hispanic","Low income","Special needs", "English learner", "Attendance rate", "Mobility rate")
df<-data.frame(Independent_variables, VIF)
p1<-ggplot(df,aes(x=Independent_variables,y=VIF))+geom_bar(stat="identity",fill = "steelblue4")
print(p1+ coord_flip()+ggtitle("Graph: Variation Inflation Factors for independent variables in model 5")+geom_text(aes(label=VIF,hjust=ifelse(VIF < max(df$VIF) / 1.5, -0.1, 1.1)), vjust=0.5, size=3)+
        theme_minimal())
summary(m8)

#create coefficient graph
install.packages('arm')
library(arm)
coefplot(m8, intercept=FALSE)
warning()
