#library the tidyverse, readxl

library(readxl)
library(tidyverse)

#then load the data

titanic <- read_excel(“titanic.xlsx”)
View(titanic)

#  1.	Explore the data a bit

head(titanic)
tail(titanic)

class(titanic)
dim(titanic)
names(titanic)

str(titanic)


library(dplyr)
glimpse(titanic)
summary(titanic)

#no need to gather or spread

#a.	What variables are present, what type are they? 891 obs and 12 variables; number variables - age. fare; chr variables - name, sex ,ticket, cabin, embarked. 
  #need to change survived, pclass. parch to chr variables

class("Survived")

titanic$Survived <- as.character(titanic$Survived)
as.character('PassengerID')
as.character('Pclass')
as.character('Parch')
as.factor('Embarked')

#b.	How much missing data ? Age is missing 177 observations

library(stringr)

is.na(titanic)
any(is.na(titanic))
colSums(is.na(titanic))
summary(titanic)
complete.cases(titanic)
na.omit(titanic)
print(titanic)
str(titanic)


titanic[!complete.cases(titanic),]

#c.	How many passengers of different types? 3 classes (216, 184, 491); mean age is 29.7

hist(titanic$Age)
summary(titanic$Age)

table(titanic$Pclass)

#d.	How many male/female, how many survived/died? 342 of 549 survived; 314 F, 577 M


table(titanic$Survived)
table(titanic$Sex)
  
#  2.	Filter to select only males, look at survival rate
    
Males <- titanic %>%
  filter(Sex == "male") %>%
  select(Survived) %>%
  summarize(nsurvived = sum(Survived), total=n(), survival_rate=(nsurvived/total)*100)

Males

titanic %>%
  add_count(Sex)

titanic %>%
  add_tally(total=n)


#3.	Filter to select only females, look at survival rate
Females <- titanic %>%
  filter(Sex == "female")

table(Females$Survived)

#4.	Arrange by fare, look at survival rate in head and tail

titanicfare <- titanic %>%
  arrange(Fare) %>%

summary(titanic$Fare)

View(titanicfare)

head(titanicfare, n=20) 
  
tail(titanicfare, n=20) 

library(forcats)

fare_survival <- titanic %>%
  mutate(fare5 = case_when(Fare == 0 ~ "freeloader", 
                           Fare < 10 ~ "Cheap", 
                           Fare < 20 ~ "Average", 
                           Fare < 100 ~ "Expensive", 
                           Fare >=100 ~ "Royal"))

FS1 <- fare_survival %>%
  group_by(fare5) %>%
    summarize(nsurvived = sum(Survived), total=n(), survival_rate=(nsurvived/total)*100)

ggplot(FS1, aes(x=fct_reorder(fare5, survival_rate), y=survival_rate)) +
  geom_bar(stat = "identity")

#5.	Parch = number of parents and children, SibSp = number of siblings and spouses. Use mutate to create a new variable called family_size = Parch + SibSp

class("Parch")
as.numeric("Parch")
class("SibSp")
as.numeric("SibSp")

titanic2 <- titanic %>%
  mutate(family_size = Parch + SibSp)

str(titanic2)

#6.	Look at family size vs survival

Fam <- table(titanic2$family_size, titanic2$Survived)
Fam
summary(Fam)

#7.	To make it easier, use mutate to replace Survived = 0 with No, and Survived =1 with Yes
titanic2$Survived[titanic2$Survived == 1] <- "Yes"
titanic2$Survived[titanic2$Survived == 0] <- "No"

Fam <- table(titanic2$family_size, titanic2$Survived)
Fam
summary(Fam)

#8.	Bar Plot how many male vs female passengers
counts <- table(titanic2$Sex)
barplot(counts, main = "Sex Proportion", xlab = "Sex", col=c("darkblue", "red"))

#9.	Scatter plot age vs fare, with color = sex, and facet by survived

library(ggplot2)

titanic3 <- ggplot() + geom_point(data = titanic2, aes(x=Age, y=Fare, color = Sex))
titanic3

titanic3 + facet_grid(rows = vars(Survived))
titanic3 + facet_grid(col = vars(Survived))


#10.	Plot a stacked bar (fill= Survived), with x = Sex

counts <- table(titanic2$Sex)
barplot(counts, main = "Sex Proportion", xlab = "Sex", col=c("darkblue", "red"))

titanic4 <- ggplot() + geom_bar(data=titanic2, aes(x=Sex, fill=Survived))

titanic5 <- ggplot(aes(x=Age, y=Fare, color=Embarked)) + geom_bar(position = "filled")

#11.	Group by sex, then mutate to get mean_fare and pct_survived


ggplot(titanic2, aes(x=Sex, y=Fare)) +
  geom_point()

EX <- titanic2 %>%
  group_by(Sex) %>%
  summarise(Fare = mean(Fare))

ggplot(EX, aes(x=Sex, y=Fare)) +
  geom_bar(stat = "identity")

install.packages("usethis")
usethis::use_git()


  
  