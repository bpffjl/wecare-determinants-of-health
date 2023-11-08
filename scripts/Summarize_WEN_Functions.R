library(dplyr)
library(stringr)
library(tidyverse)
library(janitor)

clean_WECARE = function(in_file)
{
  fake = read_excel(in_file)
  
  fake =  fake %>%
    select_if(is.character) %>%
    mutate_all(~na_if(.,"N/A")) %>%
    mutate_all(~replace_na(.,"Unknown")) %>%
    bind_cols(
      fake %>%
        select_if(Negate(is.character)))
  
  fake = fake %>% 
    select(
      -starts_with('Number'),
      -starts_with('Children'),
      -Program,
      -starts_with('Assist'),
      -starts_with('Mental'),
      -starts_with('Emotional'),
      -starts_with('Physical')) %>%
    bind_cols(
      fake %>%
        select(
          Program,
          starts_with('Assist'),
          starts_with('Mental'),
          starts_with('Emotional'),
          starts_with('Physical')) %>%
        mutate_all(as.factor),
      fake %>%
        select(starts_with('Number'),
               starts_with('Children')) %>%
        mutate_all(as.numeric)
    )
  
  fake = fake %>% clean_names()
  
  return(fake)
}

summarize.WEN = function(in_data=NULL, in_column_1=NULL, in_column_2=NULL)
{
  # in_data = fake
  # in_column_1 = 'Assist_Enroll'
  # in_column_2 = 'Assist_Medical'
  # # in_column_2=NULL
  # # missing(in_column_2)
  if (missing(in_column_2)) {
    total = as.numeric(in_data %>% summarise(n()))
    sum.data = in_data %>% group_by_(in_column_1) %>%
      summarise(Count = n(),
                Proportion = n() / total)
  }
  if (!missing(in_column_2)) {
    total = as.numeric(in_data %>% summarise(n()))
    sum.data = in_data %>% group_by_(in_column_1,in_column_2) %>%
      summarise(Count = n(),
                Proportion = n() / total)
  }
  
  # cols = names(sum.data %>% select(Count,Proportion))
  # to_pre = in_column
  # 
  # sum.data = sum.data %>% 
  #   rename_at(cols, funs( paste(to_pre,.,sep = '_' ) ) ) 
  # 
  sum.data = sum.data %>%
    rename_(Response = in_column_1)
  return(sum.data)
}

summarize.WEN.all = function(in_data, in_column_2)
{
  # in_data = fake
  # in_column_1 = 'Assist_Enroll'
  # in_column_2 = 'Assist_Medical'
  if (missing(in_column_2)) {
  sum.data = in_data %>%
    summarize.WEN(names(
      in_data %>% select(
        starts_with('Assist'),
        starts_with('Mental'),
        starts_with('Emotional'),
        starts_with('Physical')
      )
    )[1]) %>%
    mutate(Question = names(
      in_data %>% select(
        starts_with('Assist'),
        starts_with('Mental'),
        starts_with('Emotional'),
        starts_with('Physical')
      )
    )[1])
  
  
  for (n in names(in_data %>% select(starts_with('Assist'),
                                  starts_with('Mental'),
                                  starts_with('Emotional'),
                                  starts_with('Physical')))[2:ncol(in_data %>% 
                                                                   select(starts_with('Assist'),
                                                                          starts_with('Mental'),
                                                                          starts_with('Emotional'),
                                                                          starts_with('Physical')))])
  {
    temp = in_data %>% summarize.WEN(n) %>%
      mutate(Question = n)
    sum.data = bind_rows(sum.data,temp)
  }
  }
  if (!missing(in_column_2)) {
    sum.data = in_data %>%
      summarize.WEN(names(
        in_data %>% select(
          starts_with('Assist'),
          starts_with('Mental'),
          starts_with('Emotional'),
          starts_with('Physical')
        )
      )[1],in_column_2) %>%
      mutate(Question = names(
        in_data %>% select(
          starts_with('Assist'),
          starts_with('Mental'),
          starts_with('Emotional'),
          starts_with('Physical')
        )
      )[1])
    
    
    for (n in names(in_data %>% select(starts_with('Assist'),
                                       starts_with('Mental'),
                                       starts_with('Emotional'),
                                       starts_with('Physical')))[2:ncol(in_data %>% 
                                                                        select(starts_with('Assist'),
                                                                               starts_with('Mental'),
                                                                               starts_with('Emotional'),
                                                                               starts_with('Physical')))])
    {
      temp = in_data %>% summarize.WEN(n,in_column_2) %>%
        mutate(Question = n)
      sum.data = bind_rows(sum.data,temp)
    }
  }
  return(sum.data)
}

total_reponses = function(in_data) 
{
  mental = in_data %>%
    select(intake_id,starts_with("mental")) %>%
    pivot_longer(cols = starts_with("mental")) %>%
    mutate(area = "Mental")
  emotional = in_data %>%
    select(intake_id,starts_with("emotional")) %>%
    pivot_longer(cols = starts_with("emotional")) %>%
    mutate(area = "Emotional")
  physical = in_data %>%
    select(intake_id,starts_with("physical")) %>%
    pivot_longer(cols = starts_with("physical")) %>%
    mutate(area = "Physical")
  mental %>%
    bind_rows(emotional) %>%
    bind_rows(physical) %>%
    group_by(value,area) %>%
    count() %>%
    ungroup()
}