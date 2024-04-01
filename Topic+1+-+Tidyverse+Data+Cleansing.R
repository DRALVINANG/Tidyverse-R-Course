#-------------------------------------------------------------------------------
#Topic 1 - Tidyverse Data Cleansing 
#-------------------------------------------------------------------------------
#1. Load Libraries 

library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

#-------------------------------------------------------------------------------
#2. Import data
dengue <- read_csv("dengue.csv")
dengue_xls <- read_excel("dengue.xlsx")

#-------------------------------------------------------------------------------
#3. Select data
dengue %>%
  select(year,number)

#-------------------------------------------------------------------------------
#4. Filter data
dengue %>%
  filter(year==2018)

#-------------------------------------------------------------------------------
#5. Filter data based on multiple conditions
dengue %>%
  filter(year==2017 | year==2018 )

dengue %>%
  filter(year==2018,type_dengue=='Dengue' )

dengue %>%
  filter(year==2018) %>%
  filter(type_dengue=='Dengue')

#-------------------------------------------------------------------------------
#6. Missing values
dengue %>%
  filter(is.na(number))

dengue %>%
  filter(!complete.cases(.))

dengue %>%
  filter(complete.cases(.))

#-------------------------------------------------------------------------------
#7. Mutate data
dengue %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek))

#-------------------------------------------------------------------------------
#8. Plot Data
dengue %>%
  filter(complete.cases(.)) %>%
  filter(type_dengue=='Dengue') %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek)) %>%
  select(date,number) %>%
  plot()

#-------------------------------------------------------------------------------
#9. Activity - Plot Vaccination Data
vaccination <- read_excel("vaccination.xlsx")

vaccination %>%
  filter(complete.cases(.)) %>%
  filter(vaccination_type=='Poliomyelitis') %>%
  mutate(date = ymd(paste0(year,"-01-01"))) %>%
  mutate(doses = no_of_doses_in_thousands) %>%
  select(date,doses) %>%
  plot()

#-------------------------------------------------------------------------------
#10. Save data (Write CSV) 
dengue_filtered <- dengue %>%
  filter(complete.cases(.)) %>%
  filter(type_dengue=='Dengue') %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek)) %>%
  select(date,number)

write_csv(dengue_filtered, path = "dengue_filtered.csv")

#-------------------------------------------------------------------------------
#11a. Tibble
df <- tibble(
  'male' = c(2.3,3.5,4.6,3.2,2.5),
  'female' = c(1.3,2.6,1.7,1.9,2.1)
)
df

#11b. Tibble Sleep Data
sleep
as_tibble(sleep)

#-------------------------------------------------------------------------------
# 12. Activity: Tibble (Heart Data)
heart<- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')

names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart <- as_tibble(heart)

#-------------------------------------------------------------------------------
# 13. Gather
detergent = data.frame(
  'A'= c(15, 12, 10, 6),
  'B' = c(18, 14, 18, 12),
  'C'= c(10, 9, 7, 5))

detergent <- as_tibble(detergent)
detergent %>%
  gather(brand, effect)

#-------------------------------------------------------------------------------
#14. Activity: Gather - Shampoo
shampoo = data.frame(
  'A'=c(36.6,39.2,30.4,37.1,34.1),
  'B' = c(17.5,20.6,18.7,25.7,22.0),
  'C'=c(15.0,10.4,18.9,10.5,15.2))

shampoo <- as_tibble(shampoo)
shampoo %>%
  gather(brand, effect)

#-------------------------------------------------------------------------------
#15. Data Joins

df1 = data_frame(name=c('Ally','Steve','John'),age=c(45,46,47))
df2 = data_frame(name=c('Ally','Belinda','John'),age=c(45,48,47))

left_join(df1,df2,by='name')
right_join(df1,df2,by='name')
inner_join(df1,df2,by='name')
full_join(df1,df2,by='name')

#-------------------------------------------------------------------------------
#16. Group By
sleep %>% 
  group_by(group) %>%
  summarize(avg_extra=mean(extra))

#-------------------------------------------------------------------------------
#THE END
#-------------------------------------------------------------------------------
