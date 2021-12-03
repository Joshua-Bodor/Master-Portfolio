library(dplyr)
library(ggplot2)

setwd("D:/Documents/WGU_MSDA/OEM2")

data <- read.csv(file="medical_clean.csv", stringsAsFactors = TRUE)

data_sel <- select(data, Customer_id, ReAdmis, State, Area, Population, Children, 
                   Age, Income, Marital, Gender, Soft_drink, HighBlood, Stroke, 
                   Overweight, Arthritis, Diabetes, Hyperlipidemia, BackPain, 
                   Anxiety, Allergic_rhinitis, Reflux_esophagitis, Asthma, 
                   VitD_levels, Complication_risk, Doc_visits, Initial_admin, 
                   Services, Initial_days, Full_meals_eaten)
summary(data_sel)

data_sel$Meals_per_day <- data_sel$Full_meals_eaten / data_sel$Initial_days

data_sel_cat <- select(data_sel,Customer_id, ReAdmis, State, Area, Marital, 
                       Gender, Soft_drink, HighBlood, Stroke, Overweight, 
                       Arthritis, Diabetes, Hyperlipidemia, BackPain, Anxiety, 
                       Allergic_rhinitis, Reflux_esophagitis, Asthma, 
                       Complication_risk, Initial_admin, Services)

data_sel_num <- select(data_sel, Customer_id, ReAdmis, Population, Children, Age,
                       Income, VitD_levels, Doc_visits, Initial_days, 
                       Meals_per_day)


for (i in 3:21) {
  cont_tbl <- table(data_sel_cat[,i], data_sel_cat$ReAdmis)
  cat(colnames(data_sel_cat)[i], "\n")
  print(chisq.test(cont_tbl, simulate.p.value = TRUE))
}


for (i in 3:10) {
  cat(colnames(data_sel_num)[i], "\n")
  print(t.test(data_sel_num[,i]~data_sel_num$ReAdmis))
}

summary(data_sel$Age)
summary(data_sel$Income)

boxplot(data_sel$Age, xlab="Age")
boxplot(data_sel$Income, xlab="Income")

table(data_sel$Area)
table(data_sel$Initial_admin)


ggplot(data_sel, aes(x=Area)) + 
  geom_bar()

ggplot(data_sel, aes(x=Initial_admin)) + 
  geom_bar()

ggplot(data_sel, aes(fill=ReAdmis, x=Initial_days)) + 
  geom_histogram(bins=35)

ggplot(data_sel, aes(fill=ReAdmis, x=Population)) + 
  geom_histogram(position='fill', bins=12)

ggplot(data_sel, aes(fill=ReAdmis, x=Population)) + 
  geom_histogram(bins=12)

ggplot(data_sel, aes(fill=ReAdmis, x=State)) + 
  geom_bar(position='fill')

ggplot(data_sel, aes(fill=ReAdmis, x=State)) + 
  geom_bar()

ggplot(data_sel, aes(fill=ReAdmis, x=Services)) + 
  geom_bar(position='fill')

ggplot(data_sel, aes(fill=ReAdmis, x=Services)) + 
  geom_bar()
