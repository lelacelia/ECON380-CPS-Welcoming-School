#jackson amodeo 11/10/19
#regraphing enrollment over time with year 2018 for welcoming school
library("ggplot2")
library("dplyr")
#idk help
enrolYW[1:10,2]="Welcoming"
enrolYW[11:20,2]="NonWelcoming"
ggplot(data=enrolYW, aes(x=year,y=tot_erm,group=welc_school))+
  geom_line(aes(linetype=welc_school))+
  geom_point()+ theme(legend.position = "top")+
  ggtitle("             Graph:Mean Total Enrollment 2009-2018") #lots of space to center the title

reg <- lm(tot_erm ~ white + black + hispanic +
            par_inv + mob_rate + att_rate + LEP + IEP +
            LI + Kg_size + Gr1_size + Gr2_size + Gr3_size +
            Gr4_size + Gr5_size + Gr6_size + Gr7_size +
            Gr8_size + welc_school + af_trmt + interact, x)
summary(reg)
#remove class size, LEP, and LI
reg1 <- lm(tot_erm ~ white + black + hispanic +
             mob_rate + att_rate + IEP +LEP+LI+
             welc_school + af_trmt + interact, x)
summary(reg1)
summary(x$Gr5_size)

#running econometric error tests
install.packages("tidyverse")
install.packages("broom")
library(tidyverse)
library(broom)
theme_set(theme_classic())
#start test
model.diag.metrics <- augment(reg1)
head(model.diag.metrics)
#visualize with residual error graph
ggplot(model.diag.metrics, aes(white + black + hispanic +
                                 par_inv + mob_rate + att_rate + IEP +
                                 welc_school + af_trmt + interact, tot_erm)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = white + black + hispanic +
                     par_inv + mob_rate + att_rate + IEP +
                     welc_school + af_trmt + interact, yend = .fitted), 
               color = "red", size = 0.3)
#diagnostic plots
install.packages("ggfortify")
library(ggfortify)
autoplot(reg1)

#rechecking SE and Rsq for each variable
#reg <- lm(tot_erm ~ white , x)
#summary(reg)
reg5 <- lm(tot_erm ~ white + black + hispanic +
             mob_rate + att_rate + IEP + LEP + LI +
             welc_school + af_trmt + interact, x)
summary(reg5)