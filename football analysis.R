data <- read.csv("~/Desktop/data.csv")


#removing unnecesssay features

data$ID<-NULL
data$ï..<-NULL
data$Photo<-NULL
data$Flag<-NULL
data$Club.Logo<-NULL
data$Special<-NULL


library(caret)
library(dplyr)
library(tidyr)

#Changing wages and value to the right form

data<-data%>%separate(Value,c("col","Value"),sep="¬")
data$col<-NULL
data$Value<-gsub("M","",data$Value)
names(data)[7]<-"Value in millions"

data<-data%>%separate(Wage,c("col","Wage"),sep="¬")
data$col<-NULL
data$Wage<-gsub("K","",data$Wage)
names(data)[8]<-"Wages  in Ks"

#removing NA values

football<-data[-which(is.na(data$Weak.Foot)),]

#removing more unrequired features

football$Real.Face<-NULL
football$Real.Face<-NULL
football$Jersey.Number<-NULL
football$Joined<-NULL
football$Loaned.From<-NULL
football_cleaned$Contract.Valid.Until<-NULL

football_cleaned<-football[,-(19:44)]

#Changing release clause to the right form

football_cleaned<-football_cleaned%>%separate(Release.Clause,c("col","Release.Clause in millions"),sep="¬")
football_cleaned$col<-NULL
football_cleaned$`Release.Clause in millions`<-gsub("M","",football_cleaned$`Release.Clause in millions`)

#converting feet to meters

football_cleaned$Height<-gsub("'",".",football_cleaned$Height)
football_cleaned$Height<-as.numeric(football_cleaned$Height)
football_cleaned$Height<-football_cleaned$Height*0.3048

#converting pounds to kg
football_cleaned$Weight<-gsub("lbs","",football_cleaned$Weight)
football_cleaned$Weight<-as.numeric(football_cleaned$Weight)
football_cleaned$Weight<-football_cleaned$Weight*0.454

names(football_cleaned)[17]<-"Weight in kg"
names(football_cleaned)[16]<-"Height in meters"

football_cleaned$`Wages  in Ks`<-as.numeric(football_cleaned$`Wages  in Ks`)
football_cleaned$`Value in millions`<-as.numeric(football_cleaned$`Value in millions`)

#feature extraction

#calculating the players BMI
football_cleaned$`Height in meters`<-as.numeric(football_cleaned$`Height in meters`)

football_cleaned$BMI<-football_cleaned$`Weight in kg`/football_cleaned$`Height in meters`^2

#calculating potential capability of each player

football_cleaned$Potential_Capacity<-football_cleaned$Potential-football_cleaned$Overall


#analysing the data

library(ggplot2)

hist(log(football_cleaned$Age))
#the average range of a players career is between 18-33

table(football_cleaned$Preferred.Foot)
ggplot(football_cleaned,aes(Age))+geom_density(aes(color=Preferred.Foot))
#more right footers and they have a higher average age compared to left footers


body<-subset(football_cleaned,Body.Type==c("Lean","Normal","Stocky"))
ggplot(body,aes(Body.Type,Age))+geom_boxplot()
#players body changes as they grow from a lean type to a normal/stocky type

#to check the proportion of body type for each postion
stocky<-body%>%group_by(Position,Body.Type)%>%summarise(total=n())%>%mutate(freq=total/sum(total))

stocky<-as.data.frame(stocky)

ggplot(stocky,aes(Body.Type,freq))+geom_point()+facet_grid(~Position)

#LS as the highest proportion of stocky players


#Creating a category for forwards,defense,midfielders and goalie

a<-football_cleaned$Position
football_cleaned$play<-ifelse(a=="CAM","MID",ifelse(a=="CDM","MID",ifelse(a=="CM","MID",ifelse(a=="LAM","MID",ifelse(a=="LCM","MID",ifelse(a=="LDM","MID",ifelse(a=="LM","MID",ifelse(a=="RAM","MID",ifelse(a=="RCM","MID",ifelse(a=="RDM","MID",ifelse(a=="RM","MID",ifelse(a=="CB","DEF",ifelse(a=="LB","DEF",ifelse(a=="LCB","DEF",ifelse(a=="LWB","DEF",ifelse(a=="RB","DEF",ifelse(a=="RCB","DEF",ifelse(a=="RWB","DEF",ifelse(a=="CF","FOR",ifelse(a=="LF","FOR",ifelse(a=="LS","FOR",ifelse(a=="LW","FOR",ifelse(a=="RF","FOR",ifelse(a=="RS","FOR",ifelse(a=="RW","FOR",ifelse(a=="ST","FOR",a))))))))))))))))))))))))))
football_cleaned<-subset(football_cleaned,play!="")
#potential capacity along with the age

potential<-football_cleaned%>%select(Name,Potential_Capacity,Age,Overall,Body.Type,BMI,Work.Rate,Weak.Foot,Skill.Moves,`Wages  in Ks`,Club,`Value in millions`,Position,play,Nationality)%>%group_by(Name,Potential_Capacity,Age,Overall,Body.Type,BMI,Work.Rate,Weak.Foot,Skill.Moves,`Wages  in Ks`,Club,`Value in millions`,Position,play,Nationality)%>%summarise(value=Potential_Capacity>4)%>%arrange(Age)
potential<-potential%>%arrange(desc(Potential_Capacity))
potential<-as.data.frame(potential)

potential<-subset(potential,value==TRUE & Age<=22)

potential$value<-NULL
potential$`Value in millions`<-NULL


normalize<-cbind(potential[1:4],potential[10:14])

#normalizing
normalize$Potential_Capacity_nor<-(potential$Potential_Capacity-mean(potential$Potential_Capacity))/sd(potential$Potential_Capacity)
normalize$Age_nor<-(normalize$Age-mean(normalize$Age))/sd(normalize$Age)
normalize$Overall_nor<-(normalize$Overall-mean(normalize$Overall))/sd(normalize$Overall)

#determining a score
normalize$score<-(normalize$Potential_Capacity_nor*1000)-(normalize$Age_nor*100)+(normalize$Overall_nor*10)


final<-normalize%>%select(Name,Age,Club,Position,play,Nationality,Potential_Capacity,Overall,score)%>%group_by(play)%>%arrange(desc(score))%>%top_n(5)

final<-as.data.frame(final)

#the potential players

goalie<-subset(final,play=="GK")
defense<-subset(final,play=="DEF")
midfield<-subset(final,play=="MID")
forward<-subset(final,play=="FOR")


