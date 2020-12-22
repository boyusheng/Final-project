setwd("/Users/sunyiyun/Desktop/sta304/final project")

library(cesR)
library(haven)
library(tidyverse)
library(knitr)
library(labelled)
library(lme4)

ces2019_web <- read_csv("/Users/sunyiyun/Desktop/sta304/final project/ces2019_web.csv")
ces2019_web <- to_factor(ces2019_web) 
reduced_data <- ces2019_web %>%
  select(cps19_votechoice, cps19_gender, cps19_province, cps19_education, cps19_income_number) %>%
  mutate(cps19_LP = ifelse(cps19_votechoice == "Liberal Party",1,0)) %>%
  mutate(cps19_CP = ifelse(cps19_votechoice == "Conservative Party",1,0)) %>%
  drop_na()


reduced_data$cps19_gender<-ifelse(reduced_data$cps19_gender=="A woman","Female","Male")
reduced_data <- reduced_data %>%
  rename(gender = cps19_gender)

reduced_data <- reduced_data %>%
  filter(cps19_province != "Northwest Territories") %>%
  filter(cps19_province != "Nunavut") %>%
  filter(cps19_province != "Yukon")
reduced_data <- reduced_data %>%
  rename(province = cps19_province)

reduced_data <- reduced_data %>%
  filter(cps19_education != "Don't know/ Prefer not to answer")

edu.higher<-c("Master's degree","Professional degree or doctorate")
edu.lesshigh <- c("Completed elementary school", "Some elementary school", "No schooling", "Some secondary/ high school")
edu.lessuni <- c("Some technical, community college, CEGEP, College Classique", "Some university"   )


reduced_data<-reduced_data %>% 
  mutate(education = case_when(cps19_education =="Bachelor's degree" ~ "Bachelor's degree",
                               cps19_education=="Completed secondary/ high school"~"High school diploma or a high school equivalency certificate",
                               cps19_education =="Completed technical, community college, CEGEP, College Classique" ~ "Technical, community college, CEGEP, College Classique",
                               cps19_education %in% edu.higher~"Above bachelor",
                               cps19_education %in% edu.lesshigh ~"Below high school",
                               cps19_education %in% edu.lessuni~"Below bachelor"
  )) 

reduced_data<-reduced_data %>%
  mutate(income_family = case_when(cps19_income_number < 25000 ~ "Less than $25,000",
                                   cps19_income_number >= 25000  & cps19_income_number <= 49999 ~ "$25,000 to $49,999",
                                   cps19_income_number >= 50000  & cps19_income_number <= 74999 ~ "$50,000 to $74,999",
                                   cps19_income_number >= 75000  & cps19_income_number <= 99999 ~ "$75,000 to $99,999",
                                   cps19_income_number >= 100000  & cps19_income_number <= 1249999 ~ "$100,000 to $ 124,999 ",
                                   cps19_income_number >= 125000 ~ "$125,000 and more"))
                                
                               

                                  

reduced_data <- na.omit(reduced_data)  
reduced_data <- to_factor(reduced_data)


write_csv(reduced_data, "survey_data.csv")
