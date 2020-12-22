setwd("/Users/sunyiyun/Desktop/sta304/final project")
library(haven)
library(tidyverse)
library(knitr)
library(labelled)
data <- read_csv("/Users/sunyiyun/Desktop/sta304/ps2/gss.csv")
data <- to_factor(data) 
reduced_census <- data %>%
  select(sex, province, education, income_family) %>%
  drop_na()

reduced_census <- reduced_census %>%
  rename(gender = sex) %>%
  rename(educd = education) 

edu = c("Trade certificate or diploma", "College, CEGEP or other non-university certificate or di..." )

reduced_census <-reduced_census  %>% 
  mutate(education = case_when(educd =="Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ "Bachelor's degree",
                               educd =="High school diploma or a high school equivalency certificate" ~ "High school diploma or a high school equivalency certificate",
                               educd == "University certificate, diploma or degree above the bach..."  ~ "Above bachelor",
                               educd == "Less than high school diploma or its equivalent" ~ "Below high school",
                               educd == "University certificate or diploma below the bachelor's level" ~ "Below bachelor",
                               educd %in% edu ~ "Technical, community college, CEGEP, College Classique"
  ))  

reduced_census <- 
  reduced_census%>%
  count(gender,province,education, income_family)  %>%
  drop_na()


write_csv(reduced_census, "census_data.csv")



