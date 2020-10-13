## R Script for Stats 506, F20
## Week 6 activities
## 
## This script is used to complete week 6 activities part1
##
## Author(s): Yan Chen, yanchann@umich.edu
## Updated: Oct 13, 2020 

# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# Part 1

# data: -----------------------------------------------------------------------
demo = read.csv(url(paste0("https://raw.githubusercontent.com/jbhender/",
                           "Stats506_F20/master/problem_sets/data/",
                           "nhanes_demo.csv")))
ohxden = read.csv(url(paste0("https://raw.githubusercontent.com/jbhender/",
                             "Stats506_F20/master/problem_sets/data/",
                             "nhanes_ohxden.csv")))

## 2 Merge OHDDESTS into the demograhics data

merged = ohxden %>% select(SEQN, OHDDESTS) %>% left_join(demo, by = "SEQN")

## 3 
cleaned_df = merged %>% 
  transmute(
    id = SEQN, gender = factor(RIAGENDR), age = RIDAGEYR,
    under_20 = as.integer(RIDAGEYR<20),
    college = ifelse(
      under_20 == 1, "No college",
      ifelse(DMDEDUC2>=4, "College", "No college")),
      ohx_status = OHDDESTS, exam_status = RIDSTATR) 
                             

summary(cleaned_df)

## 4
cleaned_df = cleaned_df %>% 
  mutate(
    ohx = ifelse(!is.na(ohx_status),
                 ifelse(exam_status == 2 & ohx_status ==1,
                 "complete", "missing"), "missing")
  )

## 5
cleaned_df = cleaned_df %>% filter(exam_status == 2)


  
#Part 2

#table1 = 
cleaned_df %>% pivot_wider(
  id_cols = id, names_from = ohx_status,
  values_from = c(gender)) %>% 
  summarise(n = n())
  
  
  
  group_by(ohx, under_20) %>%
  summarise(n = n())


  
  
  
  
  
  
  
format(n, trim = TRUE, big.mark = ",")



# 79: -------------------------------------------------------------------------