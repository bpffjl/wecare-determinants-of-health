library(tidyverse)
library(readtext)
library(DBI)
library(lubridate)
library(readxl)
library(arsenal)
library(janitor)
library(skimr)
library(ggthemes)
library(naniar)
library(writexl)
library(magrittr)
library(flextable)
library(openxlsx)
source(here::here("scripts","report_functions.R"))
source(here::here("scripts","Summarize_WEN_Functions.R"))
## Load and Prepare data 

## load data



d=here::here("data","WECARE-Assessments-Month-08-01-2022.xlsx")%>%
  clean_WECARE()

d %>% 
  glimpse()

## tidy strings


d <- 
  d %>% 
  mutate_if(is.character,~str_trim(.)) 

d %>% 
  glimpse()


## remove test people

d <- d %>% 
  filter(!str_detect(str_replace_all(person_name, regex("\\W+")," "),regex("test person",ignore_case = TRUE))
  )

d %>%
  select(person_name) %>% 
  distinct()



## tall questions


tall_q <- 
  d %>% 
  select(intake_id,contains("_question_"),contains("assist_")) %>% 
  pivot_longer(-intake_id)

tall_q

tall_q %>% 
select(name) %>% 
distinct() %>% 
  write_xlsx(
    here::here("output",
               "excel",
               "question list.xlsx"
    )
  )
