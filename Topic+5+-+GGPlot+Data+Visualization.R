#-------------------------------------------------------------------------------
#Topic 5 - GGPlot Data Visualization
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
#2.  Read the college dataset
college <- read_csv('college.csv')

#-------------------------------------------------------------------------------
#3.  Take a look at the data
summary(college)

#-------------------------------------------------------------------------------
#4. MUTATE = Convert state, region, 
# highest_degree, control, and gender to factors

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region), city=as.factor(city),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender))

#-------------------------------------------------------------------------------
#5. Take a look at the data AGAIN
summary(college)

#-------------------------------------------------------------------------------
#6. Let's build a simple scatterplot with 
# tuition on the x-axis and average SAT score on the y axis
ggplot(data=college) + geom_point(size=3) + aes(x=tuition, y=sat_avg)

#-------------------------------------------------------------------------------
#7.  Let's try representing a different dimension.  
# What if we want to differentiate public vs. private schools?
# We can do this using the shape attribute
ggplot(data=college) + geom_point(size=3) + aes(x=tuition, y=sat_avg, shape=control, color=control)

#-------------------------------------------------------------------------------
#8. Add More Attributes
ggplot(data=college) + geom_point() + aes(x=tuition, y=sat_avg, color=control, size=undergrads)

#-------------------------------------------------------------------------------
#9.  And, lastly, let's add some transparency so we can see through those points a bit
# Experiment with the alpha value a bit.
ggplot(data=college) + geom_point() + aes(x=tuition, y=sat_avg, color=control, size=undergrads,alpha=1/100)

#-------------------------------------------------------------------------------
#10. Activity: Scatter Plot

#10a. Import Data
vaccination <- read_excel("vaccination.xlsx")

#10b. MUTATE
vaccine <- vaccination %>%
  filter(complete.cases(.)) %>%
  mutate(date = ymd(paste0(year,"-01-01"))) %>%
  mutate(doses = no_of_doses_in_thousands) %>%
  mutate(type = vaccination_type) %>%
  select(date,type, doses)

#10c. Plot
ggplot(data=vaccine) + geom_point(size=3) + aes(x=date, y=doses, color=type)

#-------------------------------------------------------------------------------
#11. Line Plot
ggplot(data=college) + geom_line() + geom_point() + aes(x=tuition, y=sat_avg, shape=control, color=control)

#-------------------------------------------------------------------------------
#12. Add Fitting Line
ggplot(data=college) + geom_smooth() + geom_point() + aes(x=tuition, y=sat_avg, shape=control, color=control)
ggplot(data=college) + geom_smooth(se=FALSE) + geom_point() + aes(x=tuition, y=sat_avg, shape=control, color=control)

#-------------------------------------------------------------------------------
#13. Bar Plot = How many schools are in each region?
ggplot(data=college) + geom_bar() + aes(x=region)
ggplot(data=college) + geom_col() + aes(x=region, y = sat_avg)

#-------------------------------------------------------------------------------
#14. Stacked Barplot
ggplot(data=college) + geom_bar() + aes(x=region, fill=control)

#-------------------------------------------------------------------------------
#15. Activity: Barplot (vaccine data)
ggplot(data=vaccine) + geom_col() + aes(x=type,y=doses,fill=type)

#-------------------------------------------------------------------------------
#16. How about average tuition by region?
#16a. First, I'll use some dplyr to create the right tibble
college %>%
  group_by(region) %>%
  summarize(average_tuition=mean(tuition))

#16b. Piping data to ggplot2
college %>%
  group_by(region) %>%
  summarize(avg_tuition=mean(tuition)) %>%
  ggplot() + geom_col() + aes(x=region, y=avg_tuition)

#-------------------------------------------------------------------------------
#17. Activity: Data Piping (vaccine)
vaccine %>%
  group_by(type) %>%
  summarize(avg_dose=mean(doses)) %>%
  ggplot() + geom_col() + aes(x=type, y=avg_dose)

#-------------------------------------------------------------------------------
#18. Histogram
ggplot(data=college) + geom_histogram(binwidth=1000) + aes(x=undergrads)

#-------------------------------------------------------------------------------
#19. Activity: Histogram  (Dengue)
dengue <- read_csv("dengue.csv")
dengue %>%
  filter(type_dengue=='Dengue') %>%
  ggplot() + geom_histogram(binwidth =100) + aes(x=number)

#-------------------------------------------------------------------------------
#20. Boxplot
ggplot(data=college) + geom_boxplot(fill='green') + aes(x=control, y=tuition)

#-------------------------------------------------------------------------------
#21. Boxplot with Jitter
ggplot(data=college) + geom_boxplot(fill='red') + geom_jitter(col="blue") + aes(x=control, y=tuition)

#-------------------------------------------------------------------------------
#22. Activity: Boxplot (Vaccination)
vaccination <- read_excel("vaccination.xlsx")

vaccine <- vaccination %>%
  filter(complete.cases(.)) %>%
  mutate(date = ymd(paste0(year,"-01-01"))) %>%
  mutate(doses = no_of_doses_in_thousands) %>%
  mutate(type = vaccination_type) %>%
  select(date,type, doses)
ggplot(data=vaccine) + geom_boxplot(fill='red') + aes(x=type,y=doses)

#-------------------------------------------------------------------------------
#THE END
#-------------------------------------------------------------------------------
