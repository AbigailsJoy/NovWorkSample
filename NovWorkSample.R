library(tidyverse)
library(ggplot2)
library(dplyr)

data<-read.csv("AcmeCorpData.csv",header=TRUE)
years<- read.csv("YearsCounts.csv",header=TRUE)

purple<- rgb(.91,.27,.88,1,"purple")
red<- rgb(1,.02,0,1,"red")

active_associates <- filter(data,is.na(data$Term_Year))

agebylevel<- ggplot(data=active_associates,mapping=aes(Job_Level,Age,fill=red))+geom_boxplot()
agebylevel

Execs<- filter(data,Job_Level=="Executive")
Managers<- filter(active_associates,Job_Level=="Management")
IndCon <- filter(active_associates,Job_Level=="Individual Contributor")

hist(Execs$Age)
shapiro.test(Execs$Age)
hist(Managers$Age)
shapiro.test(Managers$Age)
hist(IndCon$Age)
shapiro.test(IndCon$Age)

active_associates$Job_Level <- as.factor(active_associates$Job_Level)
ageanova <- aov(Age~Job_Level,data=active_associates)
summary(ageanova)
