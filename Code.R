#authors: Linh Anh Le
#ECON-380
#Date: 11/2/2019
#purpose: remove unnecessary variables, rename variables for every schools in Illinois during 2013-2015, summary statistics


#### #### ####
#DATA CLEANING FOR SY1314
#open original data set of SY1314
rc14 <- read.csv("D:/Downloads/ECON 380/Final project data/rc14.txt", header=FALSE, sep=";")
#remove unnecessary variables
analysis14 <- rc14[,c("V1","V4","V21","V14","V15","V16","V126","V70","V46","V50","V54","V66","V298","V302","V306","V310","V314","V318","V322","V326","V330")]
rm(analysis4)
rm(analysis4_done)
#create new variable for year
analysis14$year <- 2014 
analysis14 <- analysis14[c("V1","year","V21","V4","V14","V15","V16","V126","V70","V46","V50","V54","V66","V298","V302","V306","V310","V314","V318","V322","V326","V330")]
#rename variables
library(plyr)
analysis14<- rename(analysis14,c("V1"="ID","V4"="name","V21"="tot_erm","V14"="white","V15"="black","V16"="hispanic","V126"="mob_rate","V70"="att_rate","V46"="LEP","V50"="IEP","V54"="LI","V66"="par_inv","V298"="Kg_size","V302"="Gr1_size","V306"="Gr2_size","V310"="Gr3_size","V314"="Gr4_size","V318"="Gr5_size","V322"="Gr6_size","V326"="Gr7_size","V330"="Gr8_size"))
#filter the schools I want
analysis14 <- subset(analysis14, analysis14$ID=="150162990252506" | analysis14$ID=="150162990252714" | analysis14$ID=="150162990252802" | analysis14$ID=="150162990252230" | analysis14$ID=="150162990252072" | analysis14$ID=="150162990252127" | analysis14$ID=="150162990252352" | analysis14$ID=="150162990252498" | analysis14$ID=="150162990252825" | analysis14$ID=="150162990252605" | analysis14$ID=="150162990252187" | analysis14$ID=="150162990252960" | analysis14$ID=="150162990252195" | analysis14$ID=="150162990252257" | analysis14$ID=="150162990252180" | analysis14$ID=="150162990252276" | analysis14$ID=="150162990252154" | analysis14$ID=="150162990252288" | analysis14$ID=="150162990252492" | analysis14$ID=="150162990252620" | analysis14$ID=="150162990252155" | analysis14$ID=="150162990252637" | analysis14$ID=="150162990252105" | analysis14$ID=="150162990252487" | analysis14$ID=="150162990252902" | analysis14$ID=="150162990252429" | analysis14$ID=="150162990252354" | analysis14$ID=="150162990252222" | analysis14$ID=="150162990252783" | analysis14$ID=="150162990252454" | analysis14$ID=="150162990252386" | analysis14$ID=="150162990252402" | analysis14$ID=="150162990252290" | analysis14$ID=="150162990252175" | analysis14$ID=="150162990252767" | analysis14$ID=="150162990252204" | analysis14$ID=="150162990252799" | analysis14$ID=="150162990252093" | analysis14$ID=="150162990252921" | analysis14$ID=="150162990252129" | analysis14$ID=="150162990252349" | analysis14$ID=="150162990252344" | analysis14$ID=="150162990252092" | analysis14$ID=="150162990252169" | analysis14$ID=="150162990252069" | analysis14$ID=="150162990252246" | analysis14$ID=="150162990252954" | analysis14$ID=="150162990252951" | analysis14$ID=="150162990252392" | analysis14$ID=="150162990252892" | analysis14$ID=="150162990252704" | analysis14$ID=="150162990252941" | analysis14$ID=="150162990252475" | analysis14$ID=="150162990252903" | analysis14$ID=="150162990252766" | analysis14$ID=="150162990252542" | analysis14$ID=="150162990252462" | analysis14$ID=="150162990252110" | analysis14$ID=="150162990252324" | analysis14$ID=="150162990252844" | analysis14$ID=="150162990252945" | analysis14$ID=="150162990252387" | analysis14$ID=="150162990252334" | analysis14$ID=="150162990252353" | analysis14$ID=="150162990252128" | analysis14$ID=="150162990252809" | analysis14$ID=="150162990252083" | analysis14$ID=="150162990252828" | analysis14$ID=="150162990252331" | analysis14$ID=="150162990252368" | analysis14$ID=="150162990252937" | analysis14$ID=="150162990252507" | analysis14$ID=="150162990252179" | analysis14$ID=="150162990252079" | analysis14$ID=="150162990252138" | analysis14$ID=="150162990252829" | analysis14$ID=="150162990252123" | analysis14$ID=="150162990252266" | analysis14$ID=="150162990252160" | analysis14$ID=="150162990252223" | analysis14$ID=="150162990252804" | analysis14$ID=="150162990252501")

#DATA CLEANING FOR SY1415
#open original data set of SY1415
rc15 <- read.csv("D:/Downloads/ECON 380/Final project data/rc15.txt", header=FALSE, sep=";")
#remove unnecessary variables
analysis15 <- rc15[,c("V1","V4","V21","V14","V15","V16","V126","V70","V46","V50","V54","V66","V298","V302","V306","V310","V314","V318","V322","V326","V330")]
#create new variable for year
analysis15$year <- 2015
analysis15 <- analysis15[c("V1","year","V21","V4","V14","V15","V16","V126","V70","V46","V50","V54","V66","V298","V302","V306","V310","V314","V318","V322","V326","V330")]
#rename variables
library(plyr)
analysis15<- rename(analysis15,c("V1"="ID","V4"="name","V21"="tot_erm","V14"="white","V15"="black","V16"="hispanic","V126"="mob_rate","V70"="att_rate","V46"="LEP","V50"="IEP","V54"="LI","V66"="par_inv","V298"="Kg_size","V302"="Gr1_size","V306"="Gr2_size","V310"="Gr3_size","V314"="Gr4_size","V318"="Gr5_size","V322"="Gr6_size","V326"="Gr7_size","V330"="Gr8_size"))
#filter the schools I want
analysis15 <- subset(analysis15, analysis15$ID=="150162990252506" | analysis15$ID=="150162990252714" | analysis15$ID=="150162990252802" | analysis15$ID=="150162990252230" | analysis15$ID=="150162990252072" | analysis15$ID=="150162990252127" | analysis15$ID=="150162990252352" | analysis15$ID=="150162990252498" | analysis15$ID=="150162990252825" | analysis15$ID=="150162990252605" | analysis15$ID=="150162990252187" | analysis15$ID=="150162990252960" | analysis15$ID=="150162990252195" | analysis15$ID=="150162990252257" | analysis15$ID=="150162990252180" | analysis15$ID=="150162990252276" | analysis15$ID=="150162990252154" | analysis15$ID=="150162990252288" | analysis15$ID=="150162990252492" | analysis15$ID=="150162990252620" | analysis15$ID=="150162990252155" | analysis15$ID=="150162990252637" | analysis15$ID=="150162990252105" | analysis15$ID=="150162990252487" | analysis15$ID=="150162990252902" | analysis15$ID=="150162990252429" | analysis15$ID=="150162990252354" | analysis15$ID=="150162990252222" | analysis15$ID=="150162990252783" | analysis15$ID=="150162990252454" | analysis15$ID=="150162990252386" | analysis15$ID=="150162990252402" | analysis15$ID=="150162990252290" | analysis15$ID=="150162990252175" | analysis15$ID=="150162990252767" | analysis15$ID=="150162990252204" | analysis15$ID=="150162990252799" | analysis15$ID=="150162990252093" | analysis15$ID=="150162990252921" | analysis15$ID=="150162990252129" | analysis15$ID=="150162990252349" | analysis15$ID=="150162990252344" | analysis15$ID=="150162990252092" | analysis15$ID=="150162990252169" | analysis15$ID=="150162990252069" | analysis15$ID=="150162990252246" | analysis15$ID=="150162990252954" | analysis15$ID=="150162990252951" | analysis15$ID=="150162990252392" | analysis15$ID=="150162990252892" | analysis15$ID=="150162990252704" | analysis15$ID=="150162990252941" | analysis15$ID=="150162990252475" | analysis15$ID=="150162990252903" | analysis15$ID=="150162990252766" | analysis15$ID=="150162990252542" | analysis15$ID=="150162990252462" | analysis15$ID=="150162990252110" | analysis15$ID=="150162990252324" | analysis15$ID=="150162990252844" | analysis15$ID=="150162990252945" | analysis15$ID=="150162990252387" | analysis15$ID=="150162990252334" | analysis15$ID=="150162990252353" | analysis15$ID=="150162990252128" | analysis15$ID=="150162990252809" | analysis15$ID=="150162990252083" | analysis15$ID=="150162990252828" | analysis15$ID=="150162990252331" | analysis15$ID=="150162990252368" | analysis15$ID=="150162990252937" | analysis15$ID=="150162990252507" | analysis15$ID=="150162990252179" | analysis15$ID=="150162990252079" | analysis15$ID=="150162990252138" | analysis15$ID=="150162990252829" | analysis15$ID=="150162990252123" | analysis15$ID=="150162990252266" | analysis15$ID=="150162990252160" | analysis15$ID=="150162990252223" | analysis15$ID=="150162990252804" | analysis15$ID=="150162990252501")

#DATA CLEANING FOR SY1516
#open original data set of SY1516
rc16 <- read.csv("D:/Downloads/ECON 380/Final project data/rc16.txt", header=FALSE, sep=";")
#remove unnecessary variables
analysis16 <- rc16[,c("V1","V4","V21","V14","V15","V16","V126","V70","V46","V50","V54","V66","V410","V414","V418","V422","V426","V430","V434","V438","V442")]
#create new variable for year
analysis16$year <- 2016
analysis16 <- analysis16[c("V1","year","V21","V4","V14","V15","V16","V126","V70","V46","V50","V54","V66","V410","V414","V418","V422","V426","V430","V434","V438","V442")]
#rename variables
library(plyr)
analysis16<- rename(analysis16,c("V1"="ID","V4"="name","V21"="tot_erm","V14"="white","V15"="black","V16"="hispanic","V126"="mob_rate","V70"="att_rate","V46"="LEP","V50"="IEP","V54"="LI","V66"="par_inv","V410"="Kg_size","V414"="Gr1_size","V418"="Gr2_size","V422"="Gr3_size","V426"="Gr4_size","V430"="Gr5_size","V434"="Gr6_size","V438"="Gr7_size","V442"="Gr8_size"))
#filter the schools I want
analysis16<- subset(analysis16,analysis16$ID=="150162990252506" | analysis16$ID=="150162990252714" | analysis16$ID=="150162990252802" | analysis16$ID=="150162990252230" | analysis16$ID=="150162990252072" | analysis16$ID=="150162990252127" | analysis16$ID=="150162990252352" | analysis16$ID=="150162990252498" | analysis16$ID=="150162990252825" | analysis16$ID=="150162990252605" | analysis16$ID=="150162990252187" | analysis16$ID=="150162990252960" | analysis16$ID=="150162990252195" | analysis16$ID=="150162990252257" | analysis16$ID=="150162990252180" | analysis16$ID=="150162990252276" | analysis16$ID=="150162990252154" | analysis16$ID=="150162990252288" | analysis16$ID=="150162990252492" | analysis16$ID=="150162990252620" | analysis16$ID=="150162990252155" | analysis16$ID=="150162990252637" | analysis16$ID=="150162990252105" | analysis16$ID=="150162990252487" | analysis16$ID=="150162990252902" | analysis16$ID=="150162990252429" | analysis16$ID=="150162990252354" | analysis16$ID=="150162990252222" | analysis16$ID=="150162990252783" | analysis16$ID=="150162990252454" | analysis16$ID=="150162990252386" | analysis16$ID=="150162990252402" | analysis16$ID=="150162990252290" | analysis16$ID=="150162990252175" | analysis16$ID=="150162990252767" | analysis16$ID=="150162990252204" | analysis16$ID=="150162990252799" | analysis16$ID=="150162990252093" | analysis16$ID=="150162990252921" | analysis16$ID=="150162990252129" | analysis16$ID=="150162990252349" | analysis16$ID=="150162990252344" | analysis16$ID=="150162990252092" | analysis16$ID=="150162990252169" | analysis16$ID=="150162990252069" | analysis16$ID=="150162990252246" | analysis16$ID=="150162990252954" | analysis16$ID=="150162990252951" | analysis16$ID=="150162990252392" | analysis16$ID=="150162990252892" | analysis16$ID=="150162990252704" | analysis16$ID=="150162990252941" | analysis16$ID=="150162990252475" | analysis16$ID=="150162990252903" | analysis16$ID=="150162990252766" | analysis16$ID=="150162990252542" | analysis16$ID=="150162990252462" | analysis16$ID=="150162990252110" | analysis16$ID=="150162990252324" | analysis16$ID=="150162990252844" | analysis16$ID=="150162990252945" | analysis16$ID=="150162990252387" | analysis16$ID=="150162990252334" | analysis16$ID=="150162990252353" | analysis16$ID=="150162990252128" | analysis16$ID=="150162990252809" | analysis16$ID=="150162990252083" | analysis16$ID=="150162990252828" | analysis16$ID=="150162990252331" | analysis16$ID=="150162990252368" | analysis16$ID=="150162990252937" | analysis16$ID=="150162990252507" | analysis16$ID=="150162990252179" | analysis16$ID=="150162990252079" | analysis16$ID=="150162990252138" | analysis16$ID=="150162990252829" | analysis16$ID=="150162990252123" | analysis16$ID=="150162990252266" | analysis16$ID=="150162990252160" | analysis16$ID=="150162990252223" | analysis16$ID=="150162990252804" | analysis16$ID=="150162990252501")


#DATA CLEANING FOR SY1617
#open original data set of SY1617
rc17 <- read.csv("D:/Downloads/ECON 380/Final project data/rc17.txt", header=FALSE, sep=";")
#remove unnecessary variables
analysis17 <- rc17[,c("V1","V4","V21","V14","V15","V16","V126","V70","V46","V50","V54","V66","V454","V458","V462","V466","V470","V474","V478","V482","V486")]
#rename variables)]
#create new variable for year
analysis17$year <- 2017
analysis17 <- analysis17[c("V1","year","V21","V4","V14","V15","V16","V126","V70","V46","V50","V54","V66","V454","V458","V462","V466","V470","V474","V478","V482","V486")]
#rename variables
library(plyr)
analysis17<- rename(analysis17,c("V1"="ID","V4"="name","V21"="tot_erm","V14"="white","V15"="black","V16"="hispanic","V126"="mob_rate","V70"="att_rate","V46"="LEP","V50"="IEP","V54"="LI","V66"="par_inv","V454"="Kg_size","V458"="Gr1_size","V462"="Gr2_size","V466"="Gr3_size","V470"="Gr4_size","V474"="Gr5_size","V478"="Gr6_size","V482"="Gr7_size","V486"="Gr8_size"))
#filter out only the school needed
analysis17<- subset(analysis17,analysis17$ID=="150162990252506" | analysis17$ID=="150162990252714" | analysis17$ID=="150162990252802" | analysis17$ID=="150162990252230" | analysis17$ID=="150162990252072" | analysis17$ID=="150162990252127" | analysis17$ID=="150162990252352" | analysis17$ID=="150162990252498" | analysis17$ID=="150162990252825" | analysis17$ID=="150162990252605" | analysis17$ID=="150162990252187" | analysis17$ID=="150162990252960" | analysis17$ID=="150162990252195" | analysis17$ID=="150162990252257" | analysis17$ID=="150162990252180" | analysis17$ID=="150162990252276" | analysis17$ID=="150162990252154" | analysis17$ID=="150162990252288" | analysis17$ID=="150162990252492" | analysis17$ID=="150162990252620" | analysis17$ID=="150162990252155" | analysis17$ID=="150162990252637" | analysis17$ID=="150162990252105" | analysis17$ID=="150162990252487" | analysis17$ID=="150162990252902" | analysis17$ID=="150162990252429" | analysis17$ID=="150162990252354" | analysis17$ID=="150162990252222" | analysis17$ID=="150162990252783" | analysis17$ID=="150162990252454" | analysis17$ID=="150162990252386" | analysis17$ID=="150162990252402" | analysis17$ID=="150162990252290" | analysis17$ID=="150162990252175" | analysis17$ID=="150162990252767" | analysis17$ID=="150162990252204" | analysis17$ID=="150162990252799" | analysis17$ID=="150162990252093" | analysis17$ID=="150162990252921" | analysis17$ID=="150162990252129" | analysis17$ID=="150162990252349" | analysis17$ID=="150162990252344" | analysis17$ID=="150162990252092" | analysis17$ID=="150162990252169" | analysis17$ID=="150162990252069" | analysis17$ID=="150162990252246" | analysis17$ID=="150162990252954" | analysis17$ID=="150162990252951" | analysis17$ID=="150162990252392" | analysis17$ID=="150162990252892" | analysis17$ID=="150162990252704" | analysis17$ID=="150162990252941" | analysis17$ID=="150162990252475" | analysis17$ID=="150162990252903" | analysis17$ID=="150162990252766" | analysis17$ID=="150162990252542" | analysis17$ID=="150162990252462" | analysis17$ID=="150162990252110" | analysis17$ID=="150162990252324" | analysis17$ID=="150162990252844" | analysis17$ID=="150162990252945" | analysis17$ID=="150162990252387" | analysis17$ID=="150162990252334" | analysis17$ID=="150162990252353" | analysis17$ID=="150162990252128" | analysis17$ID=="150162990252809" | analysis17$ID=="150162990252083" | analysis17$ID=="150162990252828" | analysis17$ID=="150162990252331" | analysis17$ID=="150162990252368" | analysis17$ID=="150162990252937" | analysis17$ID=="150162990252507" | analysis17$ID=="150162990252179" | analysis17$ID=="150162990252079" | analysis17$ID=="150162990252138" | analysis17$ID=="150162990252829" | analysis17$ID=="150162990252123" | analysis17$ID=="150162990252266" | analysis17$ID=="150162990252160" | analysis17$ID=="150162990252223" | analysis17$ID=="150162990252804" | analysis17$ID=="150162990252501")

##DATA CLEANING FOR SY1718
#open original data set of SY1617
library(readxl)
rc18 <- read_excel("D:/Downloads/ECON 380/Final project data/rc18.xlsx")
#remove unnecessary variables
analysis18 <- rc18[,c("RCDTS","School Name","Student Enrollment - Total","Student Enrollment - White %","Student Enrollment - Black or African American %","Student Enrollment - Hispanic or Latino %","Student Mobility Rate","Student Attendance Rate","Student Enrollment - EL %","Student Enrollment - IEP %","Student Enrollment - Low Income %","Avg Class Size - Kindergarten","Avg Class Size - 1","Avg Class Size - 2","Avg Class Size - 3","Avg Class Size - 4","Avg Class Size - 5","Avg Class Size - 6","Avg Class Size - 7","Avg Class Size - 8")]
#create new variable for year
analysis18$year <- 2018
analysis18$par_inv <- NA
analysis18 <- analysis18[,c("RCDTS","year","Student Enrollment - Total","School Name","Student Enrollment - White %","Student Enrollment - Black or African American %","Student Enrollment - Hispanic or Latino %","Student Mobility Rate","Student Attendance Rate","Student Enrollment - EL %","Student Enrollment - IEP %","Student Enrollment - Low Income %","par_inv","Avg Class Size - Kindergarten","Avg Class Size - 1","Avg Class Size - 2","Avg Class Size - 3","Avg Class Size - 4","Avg Class Size - 5","Avg Class Size - 6","Avg Class Size - 7","Avg Class Size - 8")]
analysis18 <- rename(analysis18,c("RCDTS"="ID","Student Enrollment - Total"="tot_erm","School Name"="name","Student Enrollment - White %"="white","Student Enrollment - Black or African American %"="black","Student Enrollment - Hispanic or Latino %"="hispanic","Student Mobility Rate"="mob_rate","Student Attendance Rate"="att_rate","Student Enrollment - EL %"="LEP","Student Enrollment - IEP %"="IEP","Student Enrollment - Low Income %"="LI","Avg Class Size - Kindergarten"="Kg_size","Avg Class Size - 1"="Gr1_size","Avg Class Size - 2"="Gr2_size","Avg Class Size - 3"="Gr3_size","Avg Class Size - 4"="Gr4_size","Avg Class Size - 5"="Gr5_size","Avg Class Size - 6"="Gr6_size","Avg Class Size - 7"="Gr7_size","Avg Class Size - 8"="Gr8_size"))
#filter the schools needed
library(plyr)
analysis18<- subset(analysis18,analysis18$ID=="150162990252506" | analysis18$ID=="150162990252714" | analysis18$ID=="150162990252802" | analysis18$ID=="150162990252230" | analysis18$ID=="150162990252072" | analysis18$ID=="150162990252127" | analysis18$ID=="150162990252352" | analysis18$ID=="150162990252498" | analysis18$ID=="150162990252825" | analysis18$ID=="150162990252605" | analysis18$ID=="150162990252187" | analysis18$ID=="150162990252960" | analysis18$ID=="150162990252195" | analysis18$ID=="150162990252257" | analysis18$ID=="150162990252180" | analysis18$ID=="150162990252276" | analysis18$ID=="150162990252154" | analysis18$ID=="150162990252288" | analysis18$ID=="150162990252492" | analysis18$ID=="150162990252620" | analysis18$ID=="150162990252155" | analysis18$ID=="150162990252637" | analysis18$ID=="150162990252105" | analysis18$ID=="150162990252487" | analysis18$ID=="150162990252902" | analysis18$ID=="150162990252429" | analysis18$ID=="150162990252354" | analysis18$ID=="150162990252222" | analysis18$ID=="150162990252783" | analysis18$ID=="150162990252454" | analysis18$ID=="150162990252386" | analysis18$ID=="150162990252402" | analysis18$ID=="150162990252290" | analysis18$ID=="150162990252175" | analysis18$ID=="150162990252767" | analysis18$ID=="150162990252204" | analysis18$ID=="150162990252799" | analysis18$ID=="150162990252093" | analysis18$ID=="150162990252921" | analysis18$ID=="150162990252129" | analysis18$ID=="150162990252349" | analysis18$ID=="150162990252344" | analysis18$ID=="150162990252092" | analysis18$ID=="150162990252169" | analysis18$ID=="150162990252069" | analysis18$ID=="150162990252246" | analysis18$ID=="150162990252954" | analysis18$ID=="150162990252951" | analysis18$ID=="150162990252392" | analysis18$ID=="150162990252892" | analysis18$ID=="150162990252704" | analysis18$ID=="150162990252941" | analysis18$ID=="150162990252475" | analysis18$ID=="150162990252903" | analysis18$ID=="150162990252766" | analysis18$ID=="150162990252542" | analysis18$ID=="150162990252462" | analysis18$ID=="150162990252110" | analysis18$ID=="150162990252324" | analysis18$ID=="150162990252844" | analysis18$ID=="150162990252945" | analysis18$ID=="150162990252387" | analysis18$ID=="150162990252334" | analysis18$ID=="150162990252353" | analysis18$ID=="150162990252128" | analysis18$ID=="150162990252809" | analysis18$ID=="150162990252083" | analysis18$ID=="150162990252828" | analysis18$ID=="150162990252331" | analysis18$ID=="150162990252368" | analysis18$ID=="150162990252937" | analysis18$ID=="150162990252507" | analysis18$ID=="150162990252179" | analysis18$ID=="150162990252079" | analysis18$ID=="150162990252138" | analysis18$ID=="150162990252829" | analysis18$ID=="150162990252123" | analysis18$ID=="150162990252266" | analysis18$ID=="150162990252160" | analysis18$ID=="150162990252223" | analysis18$ID=="150162990252804" | analysis18$ID=="150162990252501")
#filter out only the school needed

#convert to numeric
analysis14 <- transform(analysis14, tot_erm = as.numeric(tot_erm))
analysis15 <- transform(analysis15, tot_erm = as.numeric(tot_erm))
analysis16 <- transform(analysis16, tot_erm = as.numeric(tot_erm))
analysis17 <- transform(analysis17, tot_erm = as.numeric(tot_erm))

#merge data sets of different years
data <- merge(analysis14, analysis15, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data <- merge(data, analysis16, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data <- merge(data,analysis17, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data <- merge(data,analysis18, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size"), all=TRUE)
data2 <- data

#create dummy variables
#create a subset of welcoming school
data_welc <- subset(data, data$ID=="150162990252506" | data$ID=="150162990252714" | data$ID=="150162990252802" | data$ID=="150162990252230" | data$ID=="150162990252072" | data$ID=="150162990252127" | data$ID=="150162990252352" | data$ID=="150162990252498" | data$ID=="150162990252825" | data$ID=="150162990252605" | data$ID=="150162990252187" | data$ID=="150162990252960" | data$ID=="150162990252195" | data$ID=="150162990252257" | data$ID=="150162990252180" | data$ID=="150162990252276" | data$ID=="150162990252154" | data$ID=="150162990252288" | data$ID=="150162990252492" | data$ID=="150162990252620" | data$ID=="150162990252155" | data$ID=="150162990252637" | data$ID=="150162990252105" | data$ID=="150162990252487" | data$ID=="150162990252902" | data$ID=="150162990252429" | data$ID=="150162990252354" | data$ID=="150162990252222" | data$ID=="150162990252783" | data$ID=="150162990252454" | data$ID=="150162990252386" | data$ID=="150162990252402" | data$ID=="150162990252290" | data$ID=="150162990252175" | data$ID=="150162990252767" | data$ID=="150162990252204" | data$ID=="150162990252799" | data$ID=="150162990252093" | data$ID=="150162990252921" | data$ID=="150162990252129" | data$ID=="150162990252349" | data$ID=="150162990252344" | data$ID=="150162990252092" | data$ID=="150162990252169" | data$ID=="150162990252069" | data$ID=="150162990252246") 
data_welc$welc_school <- 1
#create a subste of non-welcoming school
data_nonwelc <- subset(data, data$ID=="150162990252954" | data$ID=="150162990252951" | data$ID=="150162990252392" | data$ID=="150162990252892" | data$ID=="150162990252704" | data$ID=="150162990252941" | data$ID=="150162990252475" | data$ID=="150162990252903" | data$ID=="150162990252766" | data$ID=="150162990252542" | data$ID=="150162990252462" | data$ID=="150162990252110" | data$ID=="150162990252324" | data$ID=="150162990252844" | data$ID=="150162990252945" | data$ID=="150162990252387" | data$ID=="150162990252334" | data$ID=="150162990252353" | data$ID=="150162990252128" | data$ID=="150162990252809" | data$ID=="150162990252083" | data$ID=="150162990252828" | data$ID=="150162990252331" | data$ID=="150162990252368" | data$ID=="150162990252937" | data$ID=="150162990252507" | data$ID=="150162990252179" | data$ID=="150162990252079" | data$ID=="150162990252138" | data$ID=="150162990252829" | data$ID=="150162990252123" | data$ID=="150162990252266" | data$ID=="150162990252160" | data$ID=="150162990252223" | data$ID=="150162990252804" | data$ID=="150162990252501")
data_nonwelc$welc_school <- 0

#merge welcoming and non-welcoming schools with a dummy
dataset <- merge(data_nonwelc,data_welc, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size","welc_school"), all=TRUE)

#create dummy for year after the school closure
dataset$af_trmt <- 1 

#export data set
install.packages('xlsx')
library(xlsx)
write.xlsx(dataset, "D:/Downloads/ECON 380/Final project data/dataset.xlsx")
write.xlsx(datatot,"D:/Downloads/ECON 380/Final project data/datatot.xlsx")

#merge before and treatement dataset for a completed dataset
dataset1 <- read_excel("D:/Downloads/ECON 380/Final project data/dataset1.xlsx"
                       +     col_types = c("skip", "text", "numeric", 
                                           +         "numeric", "text", "numeric", "numeric", 
                                           +         "numeric", "numeric", "numeric", 
                                           +         "numeric", "numeric", "numeric", 
                                           +         "numeric", "numeric", "numeric", 
                                           +         "numeric", "numeric", "numeric", 
                                           +         "numeric", "numeric", "numeric", 
                                           +         "numeric", "numeric", "numeric"))
datatot <- merge(dataset,dataset1, by = c("ID","year","tot_erm","name","white","black","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI","Kg_size","Gr1_size","Gr2_size","Gr3_size","Gr4_size","Gr5_size","Gr6_size","Gr7_size","Gr8_size","welc_school","af_trmt"), all=TRUE)

  
#### #### ####
#DESCRIPTIVE ANALYSIS

#summary statistic
summary(datatot$tot_erm)
install.packages('psych')
library(psych)
describe(datatot$tot_erm)
describe(datatot$white)
describe(datatot$black)
describe(datatot$hispanic)
describe(datatot$par_inv)
describe(datatot$mob_rate)
describe(datatot$att_rate)
describe(datatot$LEP)
describe(datatot$IEP)
describe(datatot$LI)
describe(datatot$Kg_size)
describe(datatot$Gr1_size)
describe(datatot$Gr2_size)
describe(datatot$Gr3_size)
describe(datatot$Gr4_size)
describe(datatot$Gr5_size)
describe(datatot$Gr6_size)
describe(datatot$Gr7_size)
describe(datatot$Gr8_size)

#calculate correlation coefficients, use = "complete.obs" is used to ignore the NA observation 
datatot<- transform(datatot, black = as.numeric(black))
tot_erm_vs_white <- cor(datatot$tot_erm, datatot$white, use = "complete.obs")
tot_erm_vs_black <- cor(datatot$tot_erm, datatot$black, use = "complete.obs")
tot_erm_vs_hispanic <- cor(datatot$tot_erm, datatot$hispanic, use = "complete.obs")
tot_erm_vs_LEP <- cor(datatot$tot_erm,datatot$LEP, use = "complete.obs")
tot_erm_vs_LI <- cor(datatot$tot_erm,datatot$LI, use = "complete.obs")
tot_erm_vs_IEP <- cor(datatot$tot_erm,datatot$IEP, use = "complete.obs")
tot_erm_vs_af_trmt <- cor(datatot$tot_erm,datatot$af_trmt, use = "complete.obs")
tot_erm_vs_welc_school <- cor(datatot$tot_erm,datatot$welc_school, use = "complete.obs")
tot_erm_vs_par_inv <- cor(datatot$tot_erm,datatot$par_inv,use = "complete.obs")
tot_erm_vs_Kg <- cor(datatot$tot_erm,datatot$Kg_size,use = "complete.obs")
tot_erm_vs_Gr1 <- cor(datatot$tot_erm,datatot$Gr1_size,use = "complete.obs")
tot_erm_vs_Gr2 <- cor(datatot$tot_erm,datatot$Gr2_size,use = "complete.obs")
tot_erm_vs_Gr3 <- cor(datatot$tot_erm,datatot$Gr3_size,use = "complete.obs")
tot_erm_vs_Gr4 <- cor(datatot$tot_erm,datatot$Gr4_size,use = "complete.obs")
tot_erm_vs_Gr5 <- cor(datatot$tot_erm,datatot$Gr5_size,use = "complete.obs")
tot_erm_vs_Gr6 <- cor(datatot$tot_erm,datatot$Gr6_size,use = "complete.obs")
tot_erm_vs_Gr7 <- cor(datatot$tot_erm,datatot$Gr7_size,use = "complete.obs")
tot_erm_vs_Gr8 <- cor(datatot$tot_erm,datatot$Gr8_size,use = "complete.obs")

#create bar chart for racial variables with total enrollment
correlation_race <- c(tot_erm_vs_white, tot_erm_vs_black,tot_erm_vs_hispanic)
race <- c("white","black","hispanic")
# barplot
df0<-data.frame(race,correlation_race)
p1 <- ggplot(df0,aes(x=race,y=correlation_race))+geom_bar(stat="identity",fill = "steelblue4")
print(p1 + ggtitle("Graph: Correlation between school enrollment and race"))+
  coord_flip()

#create bar chart for total enrollment with class size
correlation <- c(tot_erm_vs_Kg,tot_erm_vs_Gr1,tot_erm_vs_Gr2,tot_erm_vs_Gr3,tot_erm_vs_Gr4,tot_erm_vs_Gr5,tot_erm_vs_Gr6,tot_erm_vs_Gr7,tot_erm_vs_Gr8)
grade_level <- c("KG","Gr1","Gr2","Gr3","Gr4","Gr5","Gr6","Gr7","Gr8")
barplot(correlalation,names.arg = gr_lv, ylab = "correlation", xlab ="grade level",col="light blue", main = "Graph: Correlation between total enrollment 
        and class size",space = 1)
df<-data.frame(grade_level,correlation)
p<-ggplot(df,aes(x=grade_level,y=correlation))+geom_bar(stat="identity",fill = "steelblue4")
print(p+ ggtitle("Graph: Correlation between school enrollment and class size"))+
  coord_flip()

#create bar chart for total enrollment with demographics and parental involvment
library(ggplot2)
correlation_demo <- c(tot_erm_vs_IEP,tot_erm_vs_LI,tot_erm_vs_LEP,tot_erm_vs_par_inv)
other_factors <- c("IEP", "Low income", "English Learner","Parental Involement")
df1<-data.frame(other_factors,correlation_demo)
p1<-ggplot(df1,aes(x=other_factors,y=correlation_demo))+geom_bar(stat="identity",fill = "steelblue4")
print(p1+ ggtitle("Graph: Correlation of school enrollment, with 
      demographics and parental involvement"))+
  coord_flip()


#calculate correlation coefficients for welcoming schools and non-welcoming schools
datatot_welc <- subset(datatot,datatot$welc_school == 1)
tot_erm_vs_black1 <- cor(datatot_welc$tot_erm, datatot_welc$black, use = "complete.obs")
tot_erm_vs_hispanic1 <- cor(datatot_welc$tot_erm, datatot_welc$hispanic, use = "complete.obs")
tot_erm_vs_LEP1 <- cor(datatot_welc$tot_erm,datatot_welc$LEP, use = "complete.obs")
tot_erm_vs_LI1 <- cor(datatot_welc$tot_erm,datatot_welc$LI, use = "complete.obs")
tot_erm_vs_IEP1 <- cor(datatot_welc$tot_erm,datatot_welc$IEP, use = "complete.obs")
tot_erm_vs_par_inv1 <- cor(datatot_welc$tot_erm,datatot_welc$par_inv,use = "complete.obs")


datatot_nonwelc <-subset(datatot,datatot$welc_school == 0)
cor(datatot_nonwelc$tot_erm,datatot_nonwelc$LI, use = "complete.obs")
cor(datatot_nonwelc$tot_erm,datatot_nonwelc$IEP, use = "complete.obs")


#t-test for welcoming schools 5 years before and after the school closure
t.test(datatot_welc$tot_erm~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$white~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$black~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$hispanic~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$par_inv~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$mob_rate~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$att_rate~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$LEP~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$IEP~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$LI~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Kg_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr1_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr2_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr3_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr4_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr5_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr6_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr7_size~datatot_welc$af_trmt, mu=0)
t.test(datatot_welc$Gr8_size~datatot_welc$af_trmt, mu=0)

#t-test for non-welcoming schools 5 years before and after the school closure
t.test(datatot_nonwelc$tot_erm~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$white~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$black~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$hispanic~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$par_inv~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$mob_rate~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$att_rate~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$LEP~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$IEP~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$LI~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Kg_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr1_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr2_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr3_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr4_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr5_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr6_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr7_size~datatot_nonwelc$af_trmt, mu=0)
t.test(datatot_nonwelc$Gr8_size~datatot_nonwelc$af_trmt, mu=0)

#create horizontal bar for race
tstat_race_welc <- data.frame("race" = rep(c("white","black","hispanic")), mean = c(1.51,83.02,13.56,1.62,83.45,14.14), Time=rep(c("Before", "Post"), each=3))
ggplot(data=tstat_race_welc, aes(x=race, y=mean, fill=Time)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("brown2","brown4")) +
  ggtitle("Graph: Average racial composition changes for Welcoming schools 
    before and after school closure")

tstat_race_nonwelc <- data.frame("race" = rep(c("white","black","hispanic")), "mean" = c(3.43,76.35,16.07,5.02,75.43,14.44), "Time"=rep(c("Before", "Post"), each=3))
ggplot(data=tstat_race_nonwelc, aes(x=race, y=mean, fill=Time)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("darkolivegreen2","darkolivegreen4")) +
  ggtitle("Graph: Average racial composition changes for Non-welcoming schools 
    before and after school closure")

#change in mobility rate average
tstat_enrollment <- data.frame("schools"= rep(c("welcoming schools","non-welcoming schools")), "mean"=c(), "Time"= rep(c("Before","Post"),each=2))
ggplot(data=tstat_mobrate, aes(x=Time, y=mean, group=schools)) +
  geom_line()+
  geom_point()
ggplot(data=tstat_mobrate, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point()
# Change line types and point shapes
ggplot(data=tstat_mobrate, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point(aes(shape=schools))+
  ggtitle("Graph: Average mobility rate changes for all schools 
    before and after school closure")

#change in IEP
tstat_LEP <- data.frame("schools"= rep(c("welcoming schools","non-welcoming schools")), "mean"=c(5.259389, 7.338384, 7.012222, 9.380255), "Time"= rep(c("Before","Post"),each=2))
ggplot(data=tstat_LEP, aes(x=Time, y=mean, group=schools)) +
  geom_line()+
  geom_point()
ggplot(data=tstat_LEP, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point() +
  ggtitle("Graph: Average proportion of LEP students changes for all schools 
    before and after school closure")

#change in class size all grade levels welcoming school94.27663	95.63522
tstat__welc_demo <- data.frame("factors" = rep(c("par_inv","att_rate","IEP","LI")), mean = c(87.87445, 74.65385, 93.55415,94.40652,29.65764,	16.12609, 94.27663,95.63522), Time=rep(c("Before", "Post"), each=4))
ggplot(data= tstat__welc_demo, aes(x=factors, y=mean, fill=Time)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("blue2","blue4")) +
  ggtitle("Graph: Student's background and parental involvement for 
  Welcoming schools before and after school closure")

tstat_nonwelc_demo <- data.frame("factors" = rep(c("par_inv","att_rate","IEP","LI")), mean = c(87.86441, 80.96838, 93.88333, 94.86500,29.65764,	16.12609, 94.27663,95.63522, 26.92722,	12.78324), Time=rep(c("Before", "Post"), each=4))
ggplot(data= tstat_nonwelc_demo, aes(x=factors, y=mean, fill=Time)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("blue2","blue4")) +
  ggtitle("Graph: Student's background and parental involvement for 
  non welcoming schools before and after school closure")

#change in class size all grade levels welcoming school
tstat__welc_demo <- data.frame("grade" = rep(c("KD","Gr1","Gr2","Gr3","Gr4","Gr5","Gr6","Gr7","Gr8")), mean = c(23.61161, 22.97321, 22.80134, 22.86771, 23.51509, 22.86256, 23.56214, 22.43981, 23.23155, 21.81174, 21.26696, 21.20478,22.14913,22.55111,22.44178,22.02133,22.30889,22.70044), Time=rep(c("Before", "Post"), each=9))
ggplot(data= tstat__welc_demo, aes(x=grade, y=mean, fill=Time)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("blue1","blue4")) +
  ggtitle("Graph: Student's background and parental involvement for Welcoming schools 
    before and after school closure")

#change in class size all grade levels non welcoming school
tstat__nonwelc_grade <- data.frame("grade" = rep(c("KD","Gr1","Gr2","Gr3","Gr4","Gr5","Gr6","Gr7","Gr8")), mean = c(23.99438,23.99056,24.04889,24.15056,24.32458,23.96292,24.67330,24.05153,24.99438,22.38944,23.18389,22.25389,23.49722,23.48833,23.40667,22.43167,21.93588,23.03588), Time=rep(c("Before", "Post"), each=9))
ggplot(data= tstat__welc_grade, aes(x=grade, y=mean, fill=Time)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("lightsalmon1","lightsalmon4")) +
  ggtitle("Graph: Average class size changes for Non-welcoming schools 
    before and after school closure")

#bar graph for school list
schools_list <- data.frame("school"= c("welcoming schools","non-welcoming schools"), "count"=c(56,44))
bp<- ggplot(schools_list, aes(x="", y=count, fill=school))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Graph: Number of welcoming schools 
  and non-welcoming schools")
print(bp)
pie <- bp + coord_polar("y")
print(pie)

mean(datatot$tot_erm[datatot$year==2009&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2010&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2011&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2012&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2013&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2014&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2015&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2016&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2017&datatot$welc_school==1])
mean(datatot$tot_erm[datatot$year==2018&datatot$welc_school==1])

mean(datatot$tot_erm[datatot$year==2009&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2010&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2011&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2012&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2013&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2014&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2015&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2016&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2017&datatot$welc_school==0])
mean(datatot$tot_erm[datatot$year==2018&datatot$welc_school==0])

###t-test for welcoming vs non-welcoming schools before treatment
#create subset for before treatment
datatot0 <- subset(datatot,datatot$af_trmt == 0)
#calculate t-test
t.test(datatot0$tot_erm~datatot0$welc_school, mu=0)
t.test(datatot0$white~datatot0$welc_school, mu=0)
t.test(datatot0$black~datatot0$welc_school, mu=0)
t.test(datatot0$hispanic~datatot0$welc_school, mu=0)
t.test(datatot0$par_inv~datatot0$welc_school, mu=0)
t.test(datatot0$mob_rate~datatot0$welc_school, mu=0)
t.test(datatot0$att_rate~datatot0$welc_school, mu=0)
t.test(datatot0$LEP~datatot0$welc_school, mu=0)
t.test(datatot0$IEP~datatot0$welc_school, mu=0)
t.test(datatot0$LI~datatot0$welc_school, mu=0)
t.test(datatot0$Kg_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr1_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr2_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr3_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr4_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr5_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr6_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr7_size~datatot0$welc_school, mu=0)
t.test(datatot0$Gr8_size~datatot0$welc_school, mu=0)

### t-test for after treatment
#subset
datatot1 <- subset(datatot,datatot$af_trmt == 1)
#t-test
t.test(datatot1$tot_erm~datatot1$welc_school, mu=0)

rm(tstat_nonwelc_demo)
#bargraph for total enrollment, white, mob_rate before treatment
before_tot_erm <- data.frame(tot_erm =c("Welcoming", "Non-Welcoming"), mean = c(327.42, 450.6556))
ggplot(before_tot_erm, aes(x=tot_erm, y=mean)) +
  geom_bar(stat="identity", width = 0.9, fill = "salmon") + 
  ggtitle("Graph: Comparing Welcoming and 
  Non-welcoming schools before treatment") + 
  geom_text(aes(label=mean), vjust=4, color="black", size=3)

before_others <- data.frame("factors" = rep(c("white", "mob_rate")), mean = c(1.51, 28.85,3.43, 22.9), group=rep(c("Welc", "Non-welc"), each=2))
ggplot(data= before_others, aes(x=factors, y=mean, fill=group)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("lightblue2","lightblue4", "salmon1", "salmon4")) +
  ggtitle("Graph: Comparing Welcoming and 
  Non-welcoming schools before treatment")

##bargraph for KG_size , Gr1, Gr2, Gr3, Gr7, Gr8 before treatment
before_size <- data.frame("factors" = rep(c("KG size", "Gr1 size", "Gr2 size", "Gr3 size", "Gr7 size", "Gr8 size")), mean = c(23.61161,22.97321,22.80134,22.86771,22.43981,23.23155, 23.994,23.99, 24.049,24.15,24.051,	24.994), group=rep(c("Welc", "Non-welc"), each=6))
ggplot(data= before_size, aes(x=factors, y=mean, fill=group)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_fill_manual(values=c("salmon1", "salmon4")) +
  ggtitle("Graph: Comparing Welcoming and 
  Non-welcoming schools before treatment")

###Line graph for change before and after treatment
tstat_enrollment <- data.frame("schools"= rep(c("welcoming schools","non-welcoming schools")), "mean"=c(327.4279, 450.6556,448.8913,419.8333), "Time"= rep(c("Before","Post"),each=2))
ggplot(data=tstat_enrollment, aes(x=Time, y=mean, group=schools)) +
  geom_line()+
  geom_point()
ggplot(data=tstat_enrollment, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point() +
  ggtitle("Graph: Average total enrollment")

tstat_parental_involvement <- data.frame("schools"= rep(c("welcoming schools","non-welcoming schools")), "mean"=c(93.55415,87.86441,94.40652,80.96838), "Time"= rep(c("Before","Post"),each=2))
ggplot(data=tstat_parental_involvement, aes(x=Time, y=mean, group=schools)) +
  geom_line()+
  geom_point()
ggplot(data=tstat_parental_involvement, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point() +
  ggtitle("Graph: Average parental involvment")

tstat_att_rate <- data.frame("schools"= rep(c("welcoming schools","non-welcoming schools")), "mean"=c(93.55415,93.88333,	94.40652,94.86500), "Time"= rep(c("Before","Post"),each=2))
ggplot(data=tstat_att_rate, aes(x=Time, y=mean, group=schools)) +
  geom_line()+
  geom_point()
ggplot(data=tstat_att_rate, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point() +
  ggtitle("Graph: Average attendance rate")


tstat_LI <- data.frame("schools"= rep(c("welcoming schools","non-welcoming schools")), "mean"=c(93.55415,93.88333,	94.40652,	94.86500), "Time"= rep(c("Before","Post"),each=2))
ggplot(data=tstat_LI, aes(x=Time, y=mean, group=schools)) +
  geom_line()+
  geom_point()
ggplot(data=tstat_LI, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point() +
  ggtitle("Graph: Average Low income rate")

tstat_IEP <- data.frame("schools"= rep(c("welcoming schools","non-welcoming schools")), "mean"=c(29.65764, 26.92722,16.12609,12.78324), "Time"= rep(c("Before","Post"),each=2))
ggplot(data=tstat_IEP, aes(x=Time, y=mean, group=schools)) +
  geom_line()+
  geom_point()
ggplot(data=tstat_IEP, aes(x=Time, y=mean, group=schools)) +
  geom_line(aes(linetype=schools))+
  geom_point() +
  ggtitle("Graph: Average English learner rate")


###Pie chart
Rsquared <- data.frame(variance= c("explained","not explained"), value=c(34.3,65.7))
pie <- ggplot(Rsquared, aes(x="", y=value, fill=variance))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Graph: Explained vs Unexplained data based on R-squared")
pie <- pie + coord_polar("y")
print(pie)

install.packages("corrplot")
library(corrplot)
cor(datatot)
corrplot(datatot, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corset <- c("tot_erm","black","white","hispanic","par_inv","mob_rate","att_rate","LEP","IEP","LI")
newdata <- datatot[corset]
cor <- cor(newdata, use = "complete.obs")
newdata <- as.matrix(newdata)
corrplot(corrgram(newdata))
corrplot(cor, method = "number",type="upper")
