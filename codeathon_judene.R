rm(list=ls()) #Cleans the environment 

#Installed packages
install.packages("here")
install.packages("writexl")

#Loading Libraries 
library(purrr)
library(tidyverse)
library(stats)
library(haven)
library(here)
library(dplyr)
library(writexl)

##Setting the Working Directory
setwd("C:/Users/juden/OneDrive/Documents/EPI_codeathon")
list.files()

##Importing Data 
pulse2020_1 <- read_excel("pulse2020_puf_01.xlsx")
pulse2020_2 <- read_excel("pulse2020_puf_02.xlsx")

##Appending Dataframes Together 
pulse2020 <- pulse2020_1 %>%
  bind_rows(pulse2020_2)


#Change the variables in excel to lowercase 
names(pulse2020) <- tolower(names(pulse2020))

#Create a new df for clean data
pulse2020_clean <- data.frame(pulse2020)
  
  
#Keeping the variables below
data_clean <- select(pulse2020_clean, eeduc, thhld_numkid, 
       anxious, worry, interest, down, hlthins4, curfoodsuf,foodsufrsn1,tenure,mortconf, rrace, rhispanic,freefood)

#Renaming variables
data_clean <- data_clean %>%
  rename(educ = eeduc,num_child = thhld_numkid, medicaid=hlthins4)

#Clearing up -88 and -99 variables 
data_clean[data_clean== -88] <-NA
data_clean[data_clean < 0] <-0

#New variables and revising existing ones
data_clean <- data_clean %>%
  mutate(new_educ= case_when(educ == 1 ~ 1,
                              educ == 2 ~ 1,
                              educ == 3 ~ 2,
                              educ == 4 ~ 3,
                              educ == 5 ~ 3,
                              educ == 6 ~ 4,
                              educ == 7 ~ 4),
         new_educ = labelled(new_educ, c("Less than a high school diploma" = 1, "High school diploma or GED" = 2, 
                                         "Some college/Associate's degree" = 3, "Bachelor's degree or higher" = 4)),
         food_insec = case_when(curfoodsuf == 1 ~ 0,
                                curfoodsuf > 1 & foodsufrsn1 == 1 ~ 1,
                                freefood == 1 ~ 1,
                                TRUE ~ 2),
         food_insec = labelled(food_insec, c("Food insecure" = 1, "Food secure" = 0)),
         housing_conf = case_when(tenure == 1 ~ 1,
                                  tenure == 4 ~ 1,
                                  mortconf == 1 | mortconf == 2 ~ 0,
                                  mortconf >= 3 ~ 1,
                                  TRUE ~ 2),
         housing_conf = labelled(housing_conf, c("Confident in payment" = 1, "Not confident in payment" = 0)),
         # 1) not at all; 2) several days ; 3) more than half the days; 4) nearly everyday
         depr_anxi = case_when(anxious > 1 ~ 1,
                               worry > 1 ~ 1,
                               interest > 1 ~ 1,
                               down > 1 ~ 1, 
                               TRUE ~ 0),
         fam_child = if_else(num_child > 0, 1, 0))
        

pulse2020_educ <- data_clean %>% 
  group_by(educ, fam_child, new_race) %>% 
  summarize(depr_anxi = mean(depr_anxi, na.rm = TRUE))

pulse2020_housing <- data_clean %>% 
  group_by(housing_conf, fam_child, new_race) %>% 
  summarize(depr_anxi = mean(depr_anxi, na.rm = TRUE))

pulse2020_food <- data_clean %>% 
  group_by(food_insec, fam_child, new_race) %>% 
  summarize(depr_anxi = mean(depr_anxi, na.rm = TRUE))

           
           
