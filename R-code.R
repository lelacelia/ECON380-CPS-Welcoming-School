#author: Linh Anh Le
#ECON-380
#Date: 11/2/2019
#purpose: remove unnecessary variables, rename variables for every schools in Illinois during 2013-2015

#### #### ####
#DATA CLEANING FOR SY1314
#open original data set of SY1314
rc14 <- read.csv("D:/Downloads/ECON 380/Final project data/rc14.txt", header=FALSE, sep=";")
#remove unnecessart variables
analysis14 <- rc14[,c("V1","V4","V891","V14","V15","V16","V126","V138","V70","V338","V46","V50","V54")]
#rename variables
install.packages("plyr")
library(plyr)
analysis14 <- rename(analysis14, c("V1"="ID", "V4"="name","V891"="prof_rate","V14"="white","V15"="black","V16"="hispanic","V126"="mob_rate","V138"="dropout_rate","V70"="att_rate","V338"="class_size","V46"="LEP","V50"="IEP","V54"="LI"))

#### #### ####
#DATA CLEANING FOR SY1415
#open original data set of SY1415
analysis15 <- rc15[,c("V1","V4","V468")]


