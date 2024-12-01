#Jackson
#Date: Last updated 11/4/2019

#rc09 <- read.csv("C:/Users/Jackson/Desktop/Seniorthesis/rc09.txt", header=FALSE, sep=";")
#rc10 <- read.csv("C:/Users/Jackson/Desktop/Seniorthesis/rc10.txt", header=FALSE, sep=";")
#rc11u <- read.csv("C:/Users/Jackson/Desktop/Seniorthesis/rc11u.txt", header=FALSE, sep=";")
#rc12 <- read.csv("C:/Users/Jackson/Desktop/Seniorthesis/rc12.txt", header=FALSE, sep=";")
#rc13 <- read.csv("C:/Users/Jackson/Desktop/Seniorthesis/rc13.txt", header=FALSE, sep=";")
#DATA CLEANING 
#open original 
install.packages('Rtools')
#remove unnecessary variables
analysis09 <- rc09[,c("V1","V3","V19","V13","V14","V15","V105","V53","V41","V45","V49","V193","V197","V201","V205","V209","V213","V217","V221","V225")]
rm(analysis4)
rm(analysis4_done)
#create new variable for year
analysis09$year <- 2009 
analysis09$LI <- NA

analysis09 <- analysis09[c("V1","year","V19","V3","V13","V14","V15","V49","V105","V53","V41","V45","LI","V193","V197","V201","V205","V209","V213","V217","V221","V225")]
#rename variables
library(plyr)
analysis09<- rename(analysis09,c("V1"="ID","V3"="name","V19"="tot_erm","V13"="white","V14"="black","V15"="hispanic","V105"="mob_rate","V53"="att_rate","V41"="LEP","V45"="IEP","V49"="par_inv","V193"="Kg_size","V197"="Gr1_size","V201"="Gr2_size","V205"="Gr3_size","V209"="Gr4_size","V213"="Gr5_size","V217"="Gr6_size","V221"="Gr7_size","V225"="Gr8_size"))
#filter the schools I want

analysis09 <- subset(analysis09, analysis09$ID == "150162990252506" | analysis09$ID == "150162990252714" | analysis09$ID == "150162990252802" | analysis09$ID == "150162990252230" | analysis09$ID == "150162990252072" | analysis09$ID == "150162990252127" | analysis09$ID == "150162990252352" | analysis09$ID == "150162990252498" | analysis09$ID == "150162990252825" | analysis09$ID == "150162990252605" | analysis09$ID == "150162990252187" | analysis09$ID == "150162990252960" | analysis09$ID == "150162990252195" | analysis09$ID == "150162990252257" | analysis09$ID == "150162990252180" | analysis09$ID == "150162990252276" | analysis09$ID == "150162990252154" | analysis09$ID == "150162990252288" | analysis09$ID == "150162990252492" | analysis09$ID == "150162990252620" | analysis09$ID == "150162990252155" | analysis09$ID == "150162990252637" | analysis09$ID == "150162990252105" | analysis09$ID == "150162990252487" | analysis09$ID == "150162990252902" | analysis09$ID == "150162990252429" | analysis09$ID == "150162990252354" | analysis09$ID == "150162990252222" | analysis09$ID == "150162990252783" | analysis09$ID == "150162990252454" | analysis09$ID == "150162990252386" | analysis09$ID == "150162990252402" | analysis09$ID == "150162990252290" | analysis09$ID == "150162990252175" | analysis09$ID == "150162990252767" | analysis09$ID == "150162990252204" | analysis09$ID == "150162990252799" | analysis09$ID == "150162990252093" | analysis09$ID == "150162990252921" | analysis09$ID == "150162990252129" | analysis09$ID == "150162990252349" | analysis09$ID == "150162990252344" | analysis09$ID == "150162990252092" | analysis09$ID == "150162990252169" | analysis09$ID == "150162990252069" | analysis09$ID == "150162990252246" | analysis09$ID == "150162990252954" | analysis09$ID == "150162990252951" | analysis09$ID == "150162990252392" | analysis09$ID == "150162990252892" | analysis09$ID == "150162990252704" | analysis09$ID == "150162990252941" | analysis09$ID == "150162990252475" | analysis09$ID == "150162990252903" | analysis09$ID == "150162990252766" | analysis09$ID == "150162990252542" | analysis09$ID == "150162990252462" | analysis09$ID == "150162990252110" | analysis09$ID == "150162990252324" | analysis09$ID == "150162990252844" | analysis09$ID == "150162990252945" | analysis09$ID == "150162990252387" | analysis09$ID == "150162990252334" | analysis09$ID == "150162990252353" | analysis09$ID == "150162990252110" | analysis09$ID == "150162990252128" | analysis09$ID == "150162990252809" | analysis09$ID == "150162990252083" | analysis09$ID == "150162990252828" | analysis09$ID == "150162990252331" | analysis09$ID == "150162990252368" | analysis09$ID == "150162990252937" | analysis09$ID == "150162990252507" | analysis09$ID == "150162990252179" | analysis09$ID == "150162990252179" | analysis09$ID == "150162990252079" | analysis09$ID == "150162990252138" | analysis09$ID == "150162990252829" | analysis09$ID == "150162990252123" | analysis09$ID == "150162990252266" | analysis09$ID == "150162990252160" | analysis09$ID == "150162990252223" | analysis09$ID == "150162990252804" | analysis09$ID == "150162990252501" | analysis09$ID == "150162990252501") 
missing_scl <- data.frame("150162990252960",2009,NA,"South Shore Fine Arts Elem Sch",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
names(missing_scl) <- c ("ID", "year","tot_erm", "name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size")
analysis09 <- rbind(analysis09,missing_scl)

#DATA CLEANING FOR SY0910
#open original data set of SY0910

#remove unnecessary variables
analysis10 <- rc10[,c("V1","V3","V13","V14","V15","V109","V19","V57","V41","V45","V49","V53","V197","V201","V205","V209","V213","V217","V221","V225","V229")]
#create new variable for year
analysis10$year <- 2010
analysis10 <- analysis10[c("V1","year","V19","V3","V13","V14","V15","V53","V109","V57","V41","V45","V49","V197","V201","V205","V209","V213","V217","V221","V225","V229")]
#rename variables
library(plyr)
analysis10<- rename(analysis10,c("V1"="ID","V3"="name","V19"="tot_erm","V13"="white","V14"="black","V15"="hispanic","V109"="mob_rate","V57"="att_rate","V41"="LEP","V45"="IEP","V49"="LI","V53"="par_inv","V197"="Kg_size","V201"="Gr1_size","V205"="Gr2_size","V209"="Gr3_size","V213"="Gr4_size","V217"="Gr5_size","V221"="Gr6_size","V225"="Gr7_size","V229"="Gr8_size"))
#filter the schools I want
analysis10 <- subset(analysis10, analysis10$ID == "150162990252506" | analysis10$ID == "150162990252714" | analysis10$ID == "150162990252802" | analysis10$ID == "150162990252230" | analysis10$ID == "150162990252072" | analysis10$ID == "150162990252127" | analysis10$ID == "150162990252352" | analysis10$ID == "150162990252498" | analysis10$ID == "150162990252825" | analysis10$ID == "150162990252605" | analysis10$ID == "150162990252187" | analysis10$ID == "150162990252960" | analysis10$ID == "150162990252195" | analysis10$ID == "150162990252257" | analysis10$ID == "150162990252180" | analysis10$ID == "150162990252276" | analysis10$ID == "150162990252154" | analysis10$ID == "150162990252288" | analysis10$ID == "150162990252492" | analysis10$ID == "150162990252620" | analysis10$ID == "150162990252155" | analysis10$ID == "150162990252637" | analysis10$ID == "150162990252105" | analysis10$ID == "150162990252487" | analysis10$ID == "150162990252902" | analysis10$ID == "150162990252429" | analysis10$ID == "150162990252354" | analysis10$ID == "150162990252222" | analysis10$ID == "150162990252783" | analysis10$ID == "150162990252454" | analysis10$ID == "150162990252386" | analysis10$ID == "150162990252402" | analysis10$ID == "150162990252290" | analysis10$ID == "150162990252175" | analysis10$ID == "150162990252767" | analysis10$ID == "150162990252204" | analysis10$ID == "150162990252799" | analysis10$ID == "150162990252093" | analysis10$ID == "150162990252921" | analysis10$ID == "150162990252129" | analysis10$ID == "150162990252349" | analysis10$ID == "150162990252344" | analysis10$ID == "150162990252092" | analysis10$ID == "150162990252169" | analysis10$ID == "150162990252069" | analysis10$ID == "150162990252246" | analysis10$ID == "150162990252954" | analysis10$ID == "150162990252951" | analysis10$ID == "150162990252392" | analysis10$ID == "150162990252892" | analysis10$ID == "150162990252704" | analysis10$ID == "150162990252941" | analysis10$ID == "150162990252475" | analysis10$ID == "150162990252903" | analysis10$ID == "150162990252766" | analysis10$ID == "150162990252542" | analysis10$ID == "150162990252462" | analysis10$ID == "150162990252110" | analysis10$ID == "150162990252324" | analysis10$ID == "150162990252844" | analysis10$ID == "150162990252945" | analysis10$ID == "150162990252387" | analysis10$ID == "150162990252334" | analysis10$ID == "150162990252353" | analysis10$ID == "150162990252110" | analysis10$ID == "150162990252128" | analysis10$ID == "150162990252809" | analysis10$ID == "150162990252083" | analysis10$ID == "150162990252828" | analysis10$ID == "150162990252331" | analysis10$ID == "150162990252368" | analysis10$ID == "150162990252937" | analysis10$ID == "150162990252507" | analysis10$ID == "150162990252179" | analysis10$ID == "150162990252179" | analysis10$ID == "150162990252079" | analysis10$ID == "150162990252138" | analysis10$ID == "150162990252829" | analysis10$ID == "150162990252123" | analysis10$ID == "150162990252266" | analysis10$ID == "150162990252160" | analysis10$ID == "150162990252223" | analysis10$ID == "150162990252804" | analysis10$ID == "150162990252501" | analysis10$ID == "150162990252501") 

#DATA CLEANING FOR SY1016
#open original data set of SY1016

#remove unnecessary variables
analysis11 <- rc11u[,c("V1","V3","V20","V13","V14","V15","V117","V61","V45","V49","V53","V57","V209","V213","V217","V221","V225","V229","V233","V237","V241")]
#create new variable for year
analysis11$year <- 2011
analysis11 <- analysis11[c("V1","year","V20","V3","V13","V14","V15","V57","V49","V117","V61","V45","V53","V209","V213","V217","V221","V225","V229","V233","V237","V241")]
#rename variables
library(plyr)
analysis11<- rename(analysis11,c("V1"="ID","V3"="name","V20"="tot_erm","V13"="white","V14"="black","V15"="hispanic","V117"="mob_rate","V61"="att_rate","V45"="LEP","V49"="IEP","V53"="LI","V57"="par_inv","V209"="Kg_size","V213"="Gr1_size","V217"="Gr2_size","V221"="Gr3_size","V225"="Gr4_size","V229"="Gr5_size","V233"="Gr6_size","V237"="Gr7_size","V241"="Gr8_size"))
#filter the schools I want
analysis11 <- subset(analysis11, analysis11$ID == "150162990252506" | analysis11$ID == "150162990252714" | analysis11$ID == "150162990252802" | analysis11$ID == "150162990252230" | analysis11$ID == "150162990252072" | analysis11$ID == "150162990252127" | analysis11$ID == "150162990252352" | analysis11$ID == "150162990252498" | analysis11$ID == "150162990252825" | analysis11$ID == "150162990252605" | analysis11$ID == "150162990252187" | analysis11$ID == "150162990252960" | analysis11$ID == "150162990252195" | analysis11$ID == "150162990252257" | analysis11$ID == "150162990252180" | analysis11$ID == "150162990252276" | analysis11$ID == "150162990252154" | analysis11$ID == "150162990252288" | analysis11$ID == "150162990252492" | analysis11$ID == "150162990252620" | analysis11$ID == "150162990252155" | analysis11$ID == "150162990252637" | analysis11$ID == "150162990252105" | analysis11$ID == "150162990252487" | analysis11$ID == "150162990252902" | analysis11$ID == "150162990252429" | analysis11$ID == "150162990252354" | analysis11$ID == "150162990252222" | analysis11$ID == "150162990252783" | analysis11$ID == "150162990252454" | analysis11$ID == "150162990252386" | analysis11$ID == "150162990252402" | analysis11$ID == "150162990252290" | analysis11$ID == "150162990252175" | analysis11$ID == "150162990252767" | analysis11$ID == "150162990252204" | analysis11$ID == "150162990252799" | analysis11$ID == "150162990252093" | analysis11$ID == "150162990252921" | analysis11$ID == "150162990252129" | analysis11$ID == "150162990252349" | analysis11$ID == "150162990252344" | analysis11$ID == "150162990252092" | analysis11$ID == "150162990252169" | analysis11$ID == "150162990252069" | analysis11$ID == "150162990252246" | analysis11$ID == "150162990252954" | analysis11$ID == "150162990252951" | analysis11$ID == "150162990252392" | analysis11$ID == "150162990252892" | analysis11$ID == "150162990252704" | analysis11$ID == "150162990252941" | analysis11$ID == "150162990252475" | analysis11$ID == "150162990252903" | analysis11$ID == "150162990252766" | analysis11$ID == "150162990252542" | analysis11$ID == "150162990252462" | analysis11$ID == "150162990252110" | analysis11$ID == "150162990252324" | analysis11$ID == "150162990252844" | analysis11$ID == "150162990252945" | analysis11$ID == "150162990252387" | analysis11$ID == "150162990252334" | analysis11$ID == "150162990252353" | analysis11$ID == "150162990252110" | analysis11$ID == "150162990252128" | analysis11$ID == "150162990252809" | analysis11$ID == "150162990252083" | analysis11$ID == "150162990252828" | analysis11$ID == "150162990252331" | analysis11$ID == "150162990252368" | analysis11$ID == "150162990252937" | analysis11$ID == "150162990252507" | analysis11$ID == "150162990252179" | analysis11$ID == "150162990252179" | analysis11$ID == "150162990252079" | analysis11$ID == "150162990252138" | analysis11$ID == "150162990252829" | analysis11$ID == "150162990252123" | analysis11$ID == "150162990252266" | analysis11$ID == "150162990252160" | analysis11$ID == "150162990252223" | analysis11$ID == "150162990252804" | analysis11$ID == "150162990252501" | analysis11$ID == "150162990252501") 

#### 
#merge data set 2010, 2010
test_merge <- merge(newdata, newdata2, by=c("ID"), all=TRUE)
test_merge2 <- merge(newdata,newdata2, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
mean(test_merge2$tot_erm[test_merge2$year=="2010"])
mean(test_merge2$tot_erm[test_merge2$year=="2009"])   

#convert tot_erm to numeric
test_merge2 <- transform(test_merge2, tot_erm = as.numeric(tot_erm))
mean(test_merge2$tot_erm[test_merge2$year == "2009"])
mean(test_merge2$tot_erm[test_merge2$year == "2010"])

#DATA CLEANING FOR SY1112
#open original data set of SY1112

#remove unnecessary variables
analysis12 <- rc12[,c("V1","V4","V21","V14","V15","V16","V118","V62","V46","V50","V54","V66","V266","V270","V274","V278","V282","V286","V290","V294","V298")]
#rename variables)]
#create new variable for year
analysis12$year <- 2012
analysis12 <- analysis12[c("V1","year","V21","V4","V14","V15","V16","V66","V118","V62","V46","V50","V54","V266","V270","V274","V278","V282","V286","V290","V294","V298")]
#rename variables
library(plyr)
analysis12<- rename(analysis12,c("V1"="ID","V4"="name","V21"="tot_erm","V14"="white","V15"="black","V16"="hispanic","V118"="mob_rate","V62"="att_rate","V46"="LEP","V50"="IEP","V54"="LI","V66"="par_inv","V266"="Kg_size","V270"="Gr1_size","V274"="Gr2_size","V278"="Gr3_size","V282"="Gr4_size","V286"="Gr5_size","V290"="Gr6_size","V294"="Gr7_size","V298"="Gr8_size"))
#filter out only the school needed
analysis12 <- subset(analysis12, analysis12$ID == "150162990252506" | analysis12$ID == "150162990252714" | analysis12$ID == "150162990252802" | analysis12$ID == "150162990252230" | analysis12$ID == "150162990252072" | analysis12$ID == "150162990252127" | analysis12$ID == "150162990252352" | analysis12$ID == "150162990252498" | analysis12$ID == "150162990252825" | analysis12$ID == "150162990252605" | analysis12$ID == "150162990252187" | analysis12$ID == "150162990252960" | analysis12$ID == "150162990252195" | analysis12$ID == "150162990252257" | analysis12$ID == "150162990252180" | analysis12$ID == "150162990252276" | analysis12$ID == "150162990252154" | analysis12$ID == "150162990252288" | analysis12$ID == "150162990252492" | analysis12$ID == "150162990252620" | analysis12$ID == "150162990252155" | analysis12$ID == "150162990252637" | analysis12$ID == "150162990252105" | analysis12$ID == "150162990252487" | analysis12$ID == "150162990252902" | analysis12$ID == "150162990252429" | analysis12$ID == "150162990252354" | analysis12$ID == "150162990252222" | analysis12$ID == "150162990252783" | analysis12$ID == "150162990252454" | analysis12$ID == "150162990252386" | analysis12$ID == "150162990252402" | analysis12$ID == "150162990252290" | analysis12$ID == "150162990252175" | analysis12$ID == "150162990252767" | analysis12$ID == "150162990252204" | analysis12$ID == "150162990252799" | analysis12$ID == "150162990252093" | analysis12$ID == "150162990252921" | analysis12$ID == "150162990252129" | analysis12$ID == "150162990252349" | analysis12$ID == "150162990252344" | analysis12$ID == "150162990252092" | analysis12$ID == "150162990252169" | analysis12$ID == "150162990252069" | analysis12$ID == "150162990252246" | analysis12$ID == "150162990252954" | analysis12$ID == "150162990252951" | analysis12$ID == "150162990252392" | analysis12$ID == "150162990252892" | analysis12$ID == "150162990252704" | analysis12$ID == "150162990252941" | analysis12$ID == "150162990252475" | analysis12$ID == "150162990252903" | analysis12$ID == "150162990252766" | analysis12$ID == "150162990252542" | analysis12$ID == "150162990252462" | analysis12$ID == "150162990252110" | analysis12$ID == "150162990252324" | analysis12$ID == "150162990252844" | analysis12$ID == "150162990252945" | analysis12$ID == "150162990252387" | analysis12$ID == "150162990252334" | analysis12$ID == "150162990252353" | analysis12$ID == "150162990252110" | analysis12$ID == "150162990252128" | analysis12$ID == "150162990252809" | analysis12$ID == "150162990252083" | analysis12$ID == "150162990252828" | analysis12$ID == "150162990252331" | analysis12$ID == "150162990252368" | analysis12$ID == "150162990252937" | analysis12$ID == "150162990252507" | analysis12$ID == "150162990252179" | analysis12$ID == "150162990252179" | analysis12$ID == "150162990252079" | analysis12$ID == "150162990252138" | analysis12$ID == "150162990252829" | analysis12$ID == "150162990252123" | analysis12$ID == "150162990252266" | analysis12$ID == "150162990252160" | analysis12$ID == "150162990252223" | analysis12$ID == "150162990252804" | analysis12$ID == "150162990252501" | analysis12$ID == "150162990252501") 


##DATA CLEANING FOR SY1213
#open original data set of SY1213

#remove unnecessary variables
analysis13 <- rc13[,c("V1","V4","V21","V14","V15","V16","V126","V70","V46","V50","V54","V66","V278","V282","V286","V290","V294","V298","V302","V306","V310")]
#create new variable for year
analysis13$year <- 2013
analysis13$par_inv <- NA
analysis13 <- analysis13[,c("V1","year","V21","V4","V14","V15","V16","V66","V126","V70","V46","V50","V54","V278","V282","V286","V290","V294","V298","V302","V306","V310")]
library(plyr)
analysis13 <- rename(analysis13,c("V1"="ID","V4"="name","V21"="tot_erm","V14"="white","V15"="black","V16"="hispanic","V126"="mob_rate","V70"="att_rate","V46"="LEP","V50"="IEP","V54"="LI","V66"="par_inv","V278"="Kg_size","V282"="Gr1_size","V286"="Gr2_size","V290"="Gr3_size","V294"="Gr4_size","V298"="Gr5_size","V302"="Gr6_size","V306"="Gr7_size","V310"="Gr8_size")
)
#filter out only the school needed
analysis13 <- subset(analysis13, analysis13$ID == "150162990252506" | analysis13$ID == "150162990252714" | analysis13$ID == "150162990252802" | analysis13$ID == "150162990252230" | analysis13$ID == "150162990252072" | analysis13$ID == "150162990252127" | analysis13$ID == "150162990252352" | analysis13$ID == "150162990252498" | analysis13$ID == "150162990252825" | analysis13$ID == "150162990252605" | analysis13$ID == "150162990252187" | analysis13$ID == "150162990252960" | analysis13$ID == "150162990252195" | analysis13$ID == "150162990252257" | analysis13$ID == "150162990252180" | analysis13$ID == "150162990252276" | analysis13$ID == "150162990252154" | analysis13$ID == "150162990252288" | analysis13$ID == "150162990252492" | analysis13$ID == "150162990252620" | analysis13$ID == "150162990252155" | analysis13$ID == "150162990252637" | analysis13$ID == "150162990252105" | analysis13$ID == "150162990252487" | analysis13$ID == "150162990252902" | analysis13$ID == "150162990252429" | analysis13$ID == "150162990252354" | analysis13$ID == "150162990252222" | analysis13$ID == "150162990252783" | analysis13$ID == "150162990252454" | analysis13$ID == "150162990252386" | analysis13$ID == "150162990252402" | analysis13$ID == "150162990252290" | analysis13$ID == "150162990252175" | analysis13$ID == "150162990252767" | analysis13$ID == "150162990252204" | analysis13$ID == "150162990252799" | analysis13$ID == "150162990252093" | analysis13$ID == "150162990252921" | analysis13$ID == "150162990252129" | analysis13$ID == "150162990252349" | analysis13$ID == "150162990252344" | analysis13$ID == "150162990252092" | analysis13$ID == "150162990252169" | analysis13$ID == "150162990252069" | analysis13$ID == "150162990252246" | analysis13$ID == "150162990252954" | analysis13$ID == "150162990252951" | analysis13$ID == "150162990252392" | analysis13$ID == "150162990252892" | analysis13$ID == "150162990252704" | analysis13$ID == "150162990252941" | analysis13$ID == "150162990252475" | analysis13$ID == "150162990252903" | analysis13$ID == "150162990252766" | analysis13$ID == "150162990252542" | analysis13$ID == "150162990252462" | analysis13$ID == "150162990252110" | analysis13$ID == "150162990252324" | analysis13$ID == "150162990252844" | analysis13$ID == "150162990252945" | analysis13$ID == "150162990252387" | analysis13$ID == "150162990252334" | analysis13$ID == "150162990252353" | analysis13$ID == "150162990252110" | analysis13$ID == "150162990252128" | analysis13$ID == "150162990252809" | analysis13$ID == "150162990252083" | analysis13$ID == "150162990252828" | analysis13$ID == "150162990252331" | analysis13$ID == "150162990252368" | analysis13$ID == "150162990252937" | analysis13$ID == "150162990252507" | analysis13$ID == "150162990252179" | analysis13$ID == "150162990252179" | analysis13$ID == "150162990252079" | analysis13$ID == "150162990252138" | analysis13$ID == "150162990252829" | analysis13$ID == "150162990252123" | analysis13$ID == "150162990252266" | analysis13$ID == "150162990252160" | analysis13$ID == "150162990252223" | analysis13$ID == "150162990252804" | analysis13$ID == "150162990252501" | analysis13$ID == "150162990252501")


#convert to numeric
analysis09 <- transform(analysis09, tot_erm = as.numeric(tot_erm))
analysis10 <- transform(analysis10, tot_erm = as.numeric(tot_erm))
analysis11 <- transform(analysis11, tot_erm = as.numeric(tot_erm))
analysis12 <- transform(analysis12, tot_erm = as.numeric(tot_erm))
analysis13 <- transform(analysis13, tot_erm = as.numeric(tot_erm))

#merge data sets of different years
data1 <- merge(analysis09, analysis10, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data1 <- merge(data1,analysis11, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data1 <- merge(data1,analysis12, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data1 <- merge(data1,analysis13, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data2 <- data1
#create dummy variables
#create a subset of welcoming school
#data_welc1 <- subset1(data1, data$ID=="150162990252506" | data$ID=="150162990252714" | data$ID=="150162990252802" | data$ID=="150162990252230" | data$ID=="150162990252072" | data$ID=="150162990252127" | data$ID=="150162990252352" | data$ID=="150162990252498" | data$ID=="150162990252825" | data$ID=="150162990252605" | data$ID=="150162990252187" | data$ID=="150162990252960" | data$ID=="150162990252195" | data$ID=="150162990252257" | data$ID=="150162990252180" | data$ID=="150162990252276" | data$ID=="150162990252154" | data$ID=="150162990252288" | data$ID=="150162990252492" | data$ID=="150162990252620" | data$ID=="150162990252155" | data$ID=="150162990252637" | data$ID=="150162990252105" | data$ID=="150162990252487" | data$ID=="150162990252902" | data$ID=="150162990252429" | data$ID=="150162990252354" | data$ID=="150162990252222" | data$ID=="150162990252783" | data$ID=="150162990252454" | data$ID=="150162990252386" | data$ID=="150162990252402" | data$ID=="150162990252290" | data$ID=="150162990252175" | data$ID=="150162990252767" | data$ID=="150162990252204" | data$ID=="150162990252799" | data$ID=="150162990252093" | data$ID=="150162990252921" | data$ID=="150162990252129" | data$ID=="150162990252349" | data$ID=="150162990252344" | data$ID=="150162990252092" | data$ID=="150162990252169" | data$ID=="150162990252069" | data$ID=="150162990252246") 
#data_welc1$welc_school1 <- 1
#create a subset of non-welcoming school
#data_nonwelc1 <- subset1(data1, data$ID=="150162990252954" | data$ID=="150162990252951" | data$ID=="150162990252392" | data$ID=="150162990252892" | data$ID=="150162990252704" | data$ID=="150162990252941" | data$ID=="150162990252475" | data$ID=="150162990252903" | data$ID=="150162990252766" | data$ID=="150162990252542" | data$ID=="150162990252462" | data$ID=="150162990252110" | data$ID=="150162990252324" | data$ID=="150162990252844" | data$ID=="150162990252945" | data$ID=="150162990252387" | data$ID=="150162990252334" | data$ID=="150162990252353" | data$ID=="150162990252128" | data$ID=="150162990252809" | data$ID=="150162990252083" | data$ID=="150162990252828" | data$ID=="150162990252331" | data$ID=="150162990252368" | data$ID=="150162990252937" | data$ID=="150162990252507" | data$ID=="150162990252179" | data$ID=="150162990252079" | data$ID=="150162990252138" | data$ID=="150162990252829" | data$ID=="150162990252123" | data$ID=="150162990252266" | data$ID=="150162990252160" | data$ID=="150162990252223" | data$ID=="150162990252804" | data$ID=="150162990252501")
#data_nonwelc1$welc_school1 <- 0

#merge welcoming and non-welcoming schools with a dummy
#dataset1 <- merge(data_nonwelc1,data_welc1, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size","welc_school"), all=TRUE)
#dataset1$af_trmt =0

#install.packages('xlsx')
library(xlsx)
write.xlsx(dataset1,"C:/Users/Jackson/Desktop/Seniorthesis/dataset1.xlsx")

#t-test stuff
#t.test(dataset1$Var1~dataset1$Var2, mu=0,alt="two.sided",conf=.95,paired=F)
#just use t-value and mean for each
welc<-datatot[which(datatot$welc_school=="1"),"tot_erm"]
matc<-datatot[which(datatot$welc_school=="0"),"tot_erm"]
t.test(welc,matc, mu=0,alt="two.sided",conf=.95,paired=F)


t.test(dataset1$white~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)


#merge all data
datatot=merge(LAdataset,dataset1, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size","welc_school","af_trmt"), all=TRUE)
summary(datatot)


#boxplots
boxplot(datatot$tot_erm,ylab="Number of Students Enrolled per school", main="Enrollment rate")
boxplot(datatot$par_inv,ylab="Parental Involvement Percent", main="Parental Involvement")
boxplot(datatot$mob_rate,ylab="Mobility Rate", main="Mobility Rate")
boxplot(datatot$att_rate,ylab="Attendance Rate", main="Attendance Rate")
#line graph
summary(datatot$tot_erm,datatot$year=2009,datatot$welc_school=1)
subset09=subset(datatot$tot_erm)
#summary(datatot$year==2009,datatot$welc_school==1,datatot$tot_erm)
#mean(datatot$year==2009,datatot$welc_school==1,datatot$tot_erm)
install.packages("dplyr")
library(dplyr)

x=datatot%>% 
  filter(year==2009)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x1=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2010)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x2=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2011)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x3=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2012)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x4=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2013)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x5=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2014)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x6=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2015)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x7=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2016)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x8=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2017)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x9=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2018)%>%filter(welc_school==1)%>%filter(is.na(tot_erm)==FALSE)
x10=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2009)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x11=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2010)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x12=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2011)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x13=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2012)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x14=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2013)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x15=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2014)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x16=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2015)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x17=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2016)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x18=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2017)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x19=mean(x$tot_erm)
x=datatot%>% 
  filter(year==2018)%>%filter(welc_school==0)%>%filter(is.na(tot_erm)==FALSE)
x20=mean(x$tot_erm)
#line graph
#install.packages("ggplot2")
library("ggplot2")
library("dplyr")
#idk help
enrolYW[1:10,2]="Welcoming"
enrolYW[10:20,2]="NonWelcoming"
ggplot(data=enrolYW, aes(x=year,y=tot_erm,group=welc_school))+
  geom_line(aes(linetype=welc_school))+
  geom_point()+ theme(legend.position = "top")+
  ggtitle("Graph:Enrollment Over Time")

#ttest all variables, welc vs non welc

t.test(dataset1$white~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$black~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$hispanic~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$att_rate~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$tot_erm~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$par_inv~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$mob_rate~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$LEP~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$IEP~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$LI~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$Kg_size~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$Gr1_size~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$Gr8_size~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)
t.test(dataset1$~dataset1$welc_school, mu=0,alt="two.sided",conf=.95,paired=F)

#ISAT data
#SCHOOL COMPOSITE PERCENT FOR MEETS & EXCEEDS(read,math,sci)
isat09 <- rc09
isat09 <- subset(isat09, isat09$V1 == "150162990252506" | isat09$V1 == "150162990252714" | isat09$V1 == "150162990252802" | isat09$V1 == "150162990252230" | isat09$V1 == "150162990252072" | isat09$V1 == "150162990252127" | isat09$V1 == "150162990252352" | isat09$V1 == "150162990252498" | isat09$V1 == "150162990252825" | isat09$V1 == "150162990252605" | isat09$V1 == "150162990252187" | isat09$V1 == "150162990252960" | isat09$V1 == "150162990252195" | isat09$V1 == "150162990252257" | isat09$V1 == "150162990252180" | isat09$V1 == "150162990252276" | isat09$V1 == "150162990252154" | isat09$V1 == "150162990252288" | isat09$V1 == "150162990252492" | isat09$V1 == "150162990252620" | isat09$V1 == "150162990252155" | isat09$V1 == "150162990252637" | isat09$V1 == "150162990252105" | isat09$V1 == "150162990252487" | isat09$V1 == "150162990252902" | isat09$V1 == "150162990252429" | isat09$V1 == "150162990252354" | isat09$V1 == "150162990252222" | isat09$V1 == "150162990252783" | isat09$V1 == "150162990252454" | isat09$V1 == "150162990252386" | isat09$V1 == "150162990252402" | isat09$V1 == "150162990252290" | isat09$V1 == "150162990252175" | isat09$V1 == "150162990252767" | isat09$V1 == "150162990252204" | isat09$V1 == "150162990252799" | isat09$V1 == "150162990252093" | isat09$V1 == "150162990252921" | isat09$V1 == "150162990252129" | isat09$V1 == "150162990252349" | isat09$V1 == "150162990252344" | isat09$V1 == "150162990252092" | isat09$V1 == "150162990252169" | isat09$V1 == "150162990252069" | isat09$V1 == "150162990252246" | isat09$V1 == "150162990252954" | isat09$V1 == "150162990252951" | isat09$V1 == "150162990252392" | isat09$V1 == "150162990252892" | isat09$V1 == "150162990252704" | isat09$V1 == "150162990252941" | isat09$V1 == "150162990252475" | isat09$V1 == "150162990252903" | isat09$V1 == "150162990252766" | isat09$V1 == "150162990252542" | isat09$V1 == "150162990252462" | isat09$V1 == "150162990252110" | isat09$V1 == "150162990252324" | isat09$V1 == "150162990252844" | isat09$V1 == "150162990252945" | isat09$V1 == "150162990252387" | isat09$V1 == "150162990252334" | isat09$V1 == "150162990252353" | isat09$V1 == "150162990252110" | isat09$V1 == "150162990252128" | isat09$V1 == "150162990252809" | isat09$V1 == "150162990252083" | isat09$V1 == "150162990252828" | isat09$V1 == "150162990252331" | isat09$V1 == "150162990252368" | isat09$V1 == "150162990252937" | isat09$V1 == "150162990252507" | isat09$V1 == "150162990252179" | isat09$V1 == "150162990252179" | isat09$V1 == "150162990252079" | isat09$V1 == "150162990252138" | isat09$V1 == "150162990252829" | isat09$V1 == "150162990252123" | isat09$V1 == "150162990252266" | isat09$V1 == "150162990252160" | isat09$V1 == "150162990252223" | isat09$V1 == "150162990252804" | isat09$V1 == "150162990252501" | isat09$V1 == "150162990252501") 
isat10 <- rc10
isat10 <- subset(isat10, isat10$V1 == "150162990252506" | isat10$V1 == "150162990252714" | isat10$V1 == "150162990252802" | isat10$V1 == "150162990252230" | isat10$V1 == "150162990252072" | isat10$V1 == "150162990252127" | isat10$V1 == "150162990252352" | isat10$V1 == "150162990252498" | isat10$V1 == "150162990252825" | isat10$V1 == "150162990252605" | isat10$V1 == "150162990252187" | isat10$V1 == "150162990252960" | isat10$V1 == "150162990252195" | isat10$V1 == "150162990252257" | isat10$V1 == "150162990252180" | isat10$V1 == "150162990252276" | isat10$V1 == "150162990252154" | isat10$V1 == "150162990252288" | isat10$V1 == "150162990252492" | isat10$V1 == "150162990252620" | isat10$V1 == "150162990252155" | isat10$V1 == "150162990252637" | isat10$V1 == "150162990252105" | isat10$V1 == "150162990252487" | isat10$V1 == "150162990252902" | isat10$V1 == "150162990252429" | isat10$V1 == "150162990252354" | isat10$V1 == "150162990252222" | isat10$V1 == "150162990252783" | isat10$V1 == "150162990252454" | isat10$V1 == "150162990252386" | isat10$V1 == "150162990252402" | isat10$V1 == "150162990252290" | isat10$V1 == "150162990252175" | isat10$V1 == "150162990252767" | isat10$V1 == "150162990252204" | isat10$V1 == "150162990252799" | isat10$V1 == "150162990252093" | isat10$V1 == "150162990252921" | isat10$V1 == "150162990252129" | isat10$V1 == "150162990252349" | isat10$V1 == "150162990252344" | isat10$V1 == "150162990252092" | isat10$V1 == "150162990252169" | isat10$V1 == "150162990252069" | isat10$V1 == "150162990252246" | isat10$V1 == "150162990252954" | isat10$V1 == "150162990252951" | isat10$V1 == "150162990252392" | isat10$V1 == "150162990252892" | isat10$V1 == "150162990252704" | isat10$V1 == "150162990252941" | isat10$V1 == "150162990252475" | isat10$V1 == "150162990252903" | isat10$V1 == "150162990252766" | isat10$V1 == "150162990252542" | isat10$V1 == "150162990252462" | isat10$V1 == "150162990252110" | isat10$V1 == "150162990252324" | isat10$V1 == "150162990252844" | isat10$V1 == "150162990252945" | isat10$V1 == "150162990252387" | isat10$V1 == "150162990252334" | isat10$V1 == "150162990252353" | isat10$V1 == "150162990252110" | isat10$V1 == "150162990252128" | isat10$V1 == "150162990252809" | isat10$V1 == "150162990252083" | isat10$V1 == "150162990252828" | isat10$V1 == "150162990252331" | isat10$V1 == "150162990252368" | isat10$V1 == "150162990252937" | isat10$V1 == "150162990252507" | isat10$V1 == "150162990252179" | isat10$V1 == "150162990252179" | isat10$V1 == "150162990252079" | isat10$V1 == "150162990252138" | isat10$V1 == "150162990252829" | isat10$V1 == "150162990252123" | isat10$V1 == "150162990252266" | isat10$V1 == "150162990252160" | isat10$V1 == "150162990252223" | isat10$V1 == "150162990252804" | isat10$V1 == "150162990252501" | isat10$V1 == "150162990252501") 
isat11 <- rc11u
isat11 <- subset(isat11, isat11$V1 == "150162990252506" | isat11$V1 == "150162990252714" | isat11$V1 == "150162990252802" | isat11$V1 == "150162990252230" | isat11$V1 == "150162990252072" | isat11$V1 == "150162990252127" | isat11$V1 == "150162990252352" | isat11$V1 == "150162990252498" | isat11$V1 == "150162990252825" | isat11$V1 == "150162990252605" | isat11$V1 == "150162990252187" | isat11$V1 == "150162990252960" | isat11$V1 == "150162990252195" | isat11$V1 == "150162990252257" | isat11$V1 == "150162990252180" | isat11$V1 == "150162990252276" | isat11$V1 == "150162990252154" | isat11$V1 == "150162990252288" | isat11$V1 == "150162990252492" | isat11$V1 == "150162990252620" | isat11$V1 == "150162990252155" | isat11$V1 == "150162990252637" | isat11$V1 == "150162990252105" | isat11$V1 == "150162990252487" | isat11$V1 == "150162990252902" | isat11$V1 == "150162990252429" | isat11$V1 == "150162990252354" | isat11$V1 == "150162990252222" | isat11$V1 == "150162990252783" | isat11$V1 == "150162990252454" | isat11$V1 == "150162990252386" | isat11$V1 == "150162990252402" | isat11$V1 == "150162990252290" | isat11$V1 == "150162990252175" | isat11$V1 == "150162990252767" | isat11$V1 == "150162990252204" | isat11$V1 == "150162990252799" | isat11$V1 == "150162990252093" | isat11$V1 == "150162990252921" | isat11$V1 == "150162990252129" | isat11$V1 == "150162990252349" | isat11$V1 == "150162990252344" | isat11$V1 == "150162990252092" | isat11$V1 == "150162990252169" | isat11$V1 == "150162990252069" | isat11$V1 == "150162990252246" | isat11$V1 == "150162990252954" | isat11$V1 == "150162990252951" | isat11$V1 == "150162990252392" | isat11$V1 == "150162990252892" | isat11$V1 == "150162990252704" | isat11$V1 == "150162990252941" | isat11$V1 == "150162990252475" | isat11$V1 == "150162990252903" | isat11$V1 == "150162990252766" | isat11$V1 == "150162990252542" | isat11$V1 == "150162990252462" | isat11$V1 == "150162990252110" | isat11$V1 == "150162990252324" | isat11$V1 == "150162990252844" | isat11$V1 == "150162990252945" | isat11$V1 == "150162990252387" | isat11$V1 == "150162990252334" | isat11$V1 == "150162990252353" | isat11$V1 == "150162990252110" | isat11$V1 == "150162990252128" | isat11$V1 == "150162990252809" | isat11$V1 == "150162990252083" | isat11$V1 == "150162990252828" | isat11$V1 == "150162990252331" | isat11$V1 == "150162990252368" | isat11$V1 == "150162990252937" | isat11$V1 == "150162990252507" | isat11$V1 == "150162990252179" | isat11$V1 == "150162990252179" | isat11$V1 == "150162990252079" | isat11$V1 == "150162990252138" | isat11$V1 == "150162990252829" | isat11$V1 == "150162990252123" | isat11$V1 == "150162990252266" | isat11$V1 == "150162990252160" | isat11$V1 == "150162990252223" | isat11$V1 == "150162990252804" | isat11$V1 == "150162990252501" | isat11$V1 == "150162990252501") 
isat12 <- rc12
isat12 <- subset(isat12, isat12$V1 == "150162990252506" | isat12$V1 == "150162990252714" | isat12$V1 == "150162990252802" | isat12$V1 == "150162990252230" | isat12$V1 == "150162990252072" | isat12$V1 == "150162990252127" | isat12$V1 == "150162990252352" | isat12$V1 == "150162990252498" | isat12$V1 == "150162990252825" | isat12$V1 == "150162990252605" | isat12$V1 == "150162990252187" | isat12$V1 == "150162990252960" | isat12$V1 == "150162990252195" | isat12$V1 == "150162990252257" | isat12$V1 == "150162990252180" | isat12$V1 == "150162990252276" | isat12$V1 == "150162990252154" | isat12$V1 == "150162990252288" | isat12$V1 == "150162990252492" | isat12$V1 == "150162990252620" | isat12$V1 == "150162990252155" | isat12$V1 == "150162990252637" | isat12$V1 == "150162990252105" | isat12$V1 == "150162990252487" | isat12$V1 == "150162990252902" | isat12$V1 == "150162990252429" | isat12$V1 == "150162990252354" | isat12$V1 == "150162990252222" | isat12$V1 == "150162990252783" | isat12$V1 == "150162990252454" | isat12$V1 == "150162990252386" | isat12$V1 == "150162990252402" | isat12$V1 == "150162990252290" | isat12$V1 == "150162990252175" | isat12$V1 == "150162990252767" | isat12$V1 == "150162990252204" | isat12$V1 == "150162990252799" | isat12$V1 == "150162990252093" | isat12$V1 == "150162990252921" | isat12$V1 == "150162990252129" | isat12$V1 == "150162990252349" | isat12$V1 == "150162990252344" | isat12$V1 == "150162990252092" | isat12$V1 == "150162990252169" | isat12$V1 == "150162990252069" | isat12$V1 == "150162990252246" | isat12$V1 == "150162990252954" | isat12$V1 == "150162990252951" | isat12$V1 == "150162990252392" | isat12$V1 == "150162990252892" | isat12$V1 == "150162990252704" | isat12$V1 == "150162990252941" | isat12$V1 == "150162990252475" | isat12$V1 == "150162990252903" | isat12$V1 == "150162990252766" | isat12$V1 == "150162990252542" | isat12$V1 == "150162990252462" | isat12$V1 == "150162990252110" | isat12$V1 == "150162990252324" | isat12$V1 == "150162990252844" | isat12$V1 == "150162990252945" | isat12$V1 == "150162990252387" | isat12$V1 == "150162990252334" | isat12$V1 == "150162990252353" | isat12$V1 == "150162990252110" | isat12$V1 == "150162990252128" | isat12$V1 == "150162990252809" | isat12$V1 == "150162990252083" | isat12$V1 == "150162990252828" | isat12$V1 == "150162990252331" | isat12$V1 == "150162990252368" | isat12$V1 == "150162990252937" | isat12$V1 == "150162990252507" | isat12$V1 == "150162990252179" | isat12$V1 == "150162990252179" | isat12$V1 == "150162990252079" | isat12$V1 == "150162990252138" | isat12$V1 == "150162990252829" | isat12$V1 == "150162990252123" | isat12$V1 == "150162990252266" | isat12$V1 == "150162990252160" | isat12$V1 == "150162990252223" | isat12$V1 == "150162990252804" | isat12$V1 == "150162990252501" | isat12$V1 == "150162990252501") 
isat13 <- rc13
isat13 <- subset(isat13, isat13$V1 == "150162990252506" | isat13$V1 == "150162990252714" | isat13$V1 == "150162990252802" | isat13$V1 == "150162990252230" | isat13$V1 == "150162990252072" | isat13$V1 == "150162990252127" | isat13$V1 == "150162990252352" | isat13$V1 == "150162990252498" | isat13$V1 == "150162990252825" | isat13$V1 == "150162990252605" | isat13$V1 == "150162990252187" | isat13$V1 == "150162990252960" | isat13$V1 == "150162990252195" | isat13$V1 == "150162990252257" | isat13$V1 == "150162990252180" | isat13$V1 == "150162990252276" | isat13$V1 == "150162990252154" | isat13$V1 == "150162990252288" | isat13$V1 == "150162990252492" | isat13$V1 == "150162990252620" | isat13$V1 == "150162990252155" | isat13$V1 == "150162990252637" | isat13$V1 == "150162990252105" | isat13$V1 == "150162990252487" | isat13$V1 == "150162990252902" | isat13$V1 == "150162990252429" | isat13$V1 == "150162990252354" | isat13$V1 == "150162990252222" | isat13$V1 == "150162990252783" | isat13$V1 == "150162990252454" | isat13$V1 == "150162990252386" | isat13$V1 == "150162990252402" | isat13$V1 == "150162990252290" | isat13$V1 == "150162990252175" | isat13$V1 == "150162990252767" | isat13$V1 == "150162990252204" | isat13$V1 == "150162990252799" | isat13$V1 == "150162990252093" | isat13$V1 == "150162990252921" | isat13$V1 == "150162990252129" | isat13$V1 == "150162990252349" | isat13$V1 == "150162990252344" | isat13$V1 == "150162990252092" | isat13$V1 == "150162990252169" | isat13$V1 == "150162990252069" | isat13$V1 == "150162990252246" | isat13$V1 == "150162990252954" | isat13$V1 == "150162990252951" | isat13$V1 == "150162990252392" | isat13$V1 == "150162990252892" | isat13$V1 == "150162990252704" | isat13$V1 == "150162990252941" | isat13$V1 == "150162990252475" | isat13$V1 == "150162990252903" | isat13$V1 == "150162990252766" | isat13$V1 == "150162990252542" | isat13$V1 == "150162990252462" | isat13$V1 == "150162990252110" | isat13$V1 == "150162990252324" | isat13$V1 == "150162990252844" | isat13$V1 == "150162990252945" | isat13$V1 == "150162990252387" | isat13$V1 == "150162990252334" | isat13$V1 == "150162990252353" | isat13$V1 == "150162990252110" | isat13$V1 == "150162990252128" | isat13$V1 == "150162990252809" | isat13$V1 == "150162990252083" | isat13$V1 == "150162990252828" | isat13$V1 == "150162990252331" | isat13$V1 == "150162990252368" | isat13$V1 == "150162990252937" | isat13$V1 == "150162990252507" | isat13$V1 == "150162990252179" | isat13$V1 == "150162990252179" | isat13$V1 == "150162990252079" | isat13$V1 == "150162990252138" | isat13$V1 == "150162990252829" | isat13$V1 == "150162990252123" | isat13$V1 == "150162990252266" | isat13$V1 == "150162990252160" | isat13$V1 == "150162990252223" | isat13$V1 == "150162990252804" | isat13$V1 == "150162990252501" | isat13$V1 == "150162990252501") 

isattot=c(isat09$V700,isat10$V704,isat11$V802,isat12$V859,isat13$V875)
library(psych)
describe(isattot)
library(xlsx)
write.xlsx(isattot,"C:/Users/Jackson/Desktop/Seniorthesis/isattot.xlsx")




