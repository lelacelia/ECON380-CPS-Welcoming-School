#Linh Anh Le
#Graph for PowerPoint

#Criteria to choose schools
#import data set with welcoming schools and their availability
library(readxl)
install.packages("forcats")
install.packages("hrbrthemes")
install.packages("kableExtra")
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(kableExtra)

tmp <- CPS_Welcoming_schools %>%
  arrange(desc(Avai)) %>%
  mutate(Name=factor(Name, Name))

empty_bar=10

# Add lines to the initial tmpset
to_add = matrix(NA, empty_bar, ncol(tmp))
colnames(to_add) = colnames(tmp)
tmp=rbind(tmp, to_add)
tmp$id=seq(1, nrow(tmp))

label_tmp=tmp
number_of_bar=nrow(label_tmp)
angle= 90 - 360 * (label_tmp$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_tmp$hjust<-ifelse( angle < -90, 1, 0)
label_tmp$angle<-ifelse(angle < -90, angle+180, angle)
label_tmp$Name <- paste(label_tmp$Name, " (", label_tmp$Availability,")", sep="")


p <- ggplot(tmp, aes(x=as.factor(id), y=Avai)) + 
  geom_bar(stat="identity", fill=alpha("blue", 0.2)) + 
  ylim(-90,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")     # This remove unnecessary margin around plot
  ) +
  coord_polar(start = 0) +
  geom_text(tmp = label_tmp, aes(x=id, y=Avai+10, label=Name ), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE ) 
p

#count of closed schools in the past
df <- data.frame(School_year = c("2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2012-13"), 
                 closed_school = c(1,3,2,6,3,3,47))
ggplot(df, aes(x=School_year, y=closed_school, group=1)) +
  geom_line(aes(linetype="solid"))+
  geom_point()

df1 <- data.frame(
  Group = c("Higher Performance", "Equal Performance"),
  Performance = c(55.6, 44.4)
)
bp<- ggplot(df1, aes(x="", y=Performance, fill=Group))+
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = Performance), size = 3.5, hjust = 0.5, vjust = 5, position =     "stack")+
  labs(title = "Comparing Welcoming vs Closed Schools performance")

bp

install.packages("wesanderson")
library(wesanderson)


sc <- ggplot(datatot, aes(x=black, y=hispanic)) +
      geom_point(color = "dark green")+
  labs(title = "Graph: Proportion of black and hispanic students") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
sc


sc +scale_color_manual(values=wes_palette(n=3, name="GrandBudapest"))

pie <- data.frame(Variance = c("Explained", "Unexplained"), value= c())
