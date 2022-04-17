library(ggplot2)
library(tidyverse)
library(stringr)
library(lubridate)

setwd("C:/Nabil Awan/Nabil_UW-Madison/SEMESTERS_Courses_TAships/Fall 2021/TA/STAT 240/Discussions/Pre-midterm discussion")

nfl <- read.csv("nfl-passing-2019-weeks-1-6.csv", header = T)
dim(nfl)
# [1] 216  24
head(nfl)

police <- read.csv("Police_Incident_Reports.csv", header = T)
dim(police)
# [1] 12567    11
head(police)

#########################################
## Q-6: geom_col() vs. geom_bar() example
#########################################

table(nfl$Pos)
summary(nfl$Age)

?geom_bar

# geom_col()
# Step-1: Data preparation
nfl_1 <- nfl %>% 
  group_by(Pos) %>% 
  summarise(AvgAge = mean(Age))
nfl_1
# Step-2: Using geom_col()
ggplot(data = nfl_1, mapping = aes(x=Pos, y=AvgAge))+
geom_col()

## geom_bar()
# Default (plots counts)
ggplot(data = nfl, mapping = aes(x=Pos))+
  geom_bar()
# Modification (plots any statistic): option-1
ggplot(data = nfl, mapping = aes(x=Pos, y=Age))+
  geom_bar(stat = "summary", fun = "mean")
# Modification (plots any statistic): option-2
ggplot(data = nfl, mapping = aes(x=Pos, y=Age))+
  stat_summary(fun = "mean", geom = "bar")
# Adding the correct axes names
ggplot(data = nfl, mapping = aes(x=Pos, y=Age))+
  geom_bar(stat = "summary", fun = "mean")+
  labs(x = "Position", y = "Avg. age")


################################
## Q-7: Separating by sep="////"
################################

nfl %>%
  separate(Player, into= c("Name","NameID"), sep="\\\\")

# Alternative: using str_split
?str_split
?seq
Name_NameID <- unlist(str_split(string = nfl$Player, pattern = "\\\\"))
nfl$Name <- Name_NameID[seq(from = 1, to = length(Name_NameID), by = 2)]
nfl$NameID <- Name_NameID[seq(from = 2, to = length(Name_NameID), by = 2)]


#############################
## Q-8: police incidenct data
#############################

police %>%
  mutate(wday = wday(IncidentDate, label=TRUE)) %>%
  filter(IncidentType == "Robbery") %>%
  ggplot(aes(x=wday)) +
  geom_bar()


############################
## Q-9: NFL passing data set
############################

nfl$Age

nfl9 = nfl %>%
  ## A
  filter(Week == 1) %>%
  select(Name,Age,Date) %>%
  ## convert=TRUE means the new variables are numbers and not strings
  separate(Age, into = c("age_years","age_days"),
           convert=TRUE) %>%
  #mutate(d = Date - age_days) %>%
  mutate(d = ymd(Date) - age_days) %>%
  ## B
  mutate(s = str_c(year(d) - age_years,"-",month(d),"-",day(d))) %>%
  ## C
  mutate(x = ymd(s)) %>%
  select(Name,x)
