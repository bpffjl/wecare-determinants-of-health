word_number_to_number_number = function(data)
{
  data %>%
    separate(name, c("number", "question"), " ", extra = "merge") %>%
    mutate(question = str_to_sentence(question),
           question = str_replace(question,"mfip","MFIP"),
           question = str_replace(question,"id$","ID"),
           question = str_replace(question,"ged$","GED"),
           number = str_replace(number,"fourteen","14"),
           number = str_replace(number,"^one","1"),
           number = str_replace(number,"two","2"),
           number = str_replace(number,"three","3"),
           number = str_replace(number,"four","4"),
           number = str_replace(number,"five","5"),
           number = str_replace(number,"six","6"),
           number = str_replace(number,"seven","7"),
           number = str_replace(number,"eight","8"),
           number = str_replace(number,"nine","9"),
           number = str_replace(number,"ten","10"),
           number = str_replace(number,"eleven","11"),
           number = str_replace(number,"twelve","12"),
           number = str_replace(number,"thirteen","13"),
           number = as.numeric(number)) %>%
    unite(name, number:question, sep  = ".  ",remove = FALSE)
}

question_tidy = function(data,question_name)
{
  data %>%
    select(intake_id,starts_with(question_name)) %>%
    pivot_longer(cols = starts_with(question_name)) %>%
    group_by(name, value) %>%
    summarise(n = n_distinct(intake_id)) %>% 
    ungroup() %>%
    mutate(Answered = if_else(value == "Unknown","Unknown", "Answered"),
           value = factor(value, levels = c("Unknown","Urgent Need","No","Yes")),
           value = fct_relevel(value,"Unknown","Urgent Need","No","Yes")) %>%
    mutate(name = str_replace_all(name,paste(question_name,"_question_","",sep=""),""),
           name = str_replace_all(name,"_"," "))
}

question_chart = function(data,question_name)
{
  p = d %>%
    question_tidy(question_name = question_name) %>%
    word_number_to_number_number() %>%
    mutate(name = str_wrap(name,width = 40,exdent = 5)) %>%
    mutate(value = factor(value, 
                          levels = c("Urgent Need","Yes","No","Unknown")),
           name = fct_reorder(name,-number))   %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(x = name, y = n, fill = value)) +
    geom_bar(stat = "identity",
             position = "dodge"
             ) +
    facet_grid(~value) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_colorblind() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3, min.n = 3)) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          text = element_text(size = 20),
          axis.text.y = element_text(hjust = 0)) 
  return(p)
}

correct_sentiment_emotional_domestic_relationship = function(data)
{
  data %>%
    mutate(n = case_when(
      
      str_detect(name,"emotional_domestic_relationship_question_five_someone_is_physically_mentally_verbally_or_sexually_hurting_my_chidren") &
        value=="No"~n,
      str_detect(name,"emotional_domestic_relationship_question_five_someone_is_physically_mentally_verbally_or_sexually_hurting_my_chidren")&
        value=="Yes"~-n,
      str_detect(name,"emotional_domestic_relationship_question_one_i_am_having_regular_conflicts_with_partner") &
        value=="No"~n,
      str_detect(name,"emotional_domestic_relationship_question_one_i_am_having_regular_conflicts_with_partner")&
        value=="Yes"~-n,
      str_detect(name,"emotional_domestic_relationship_question_six_someone_touches_me_or_my_children_in_a_way_that_makes_me_feel_uncomfortable") &
        value=="No"~n,
      str_detect(name,"emotional_domestic_relationship_question_six_someone_touches_me_or_my_children_in_a_way_that_makes_me_feel_uncomfortable")&
        value=="Yes"~-n,
      str_detect(name,"emotional_domestic_relationship_question_four_someone_is_physically_mentally_verbally_or_sexually_hurting_me") &
        value=="No"~n,
      str_detect(name,"emotional_domestic_relationship_question_six_someone_touches_me_or_my_children_in_a_way_that_makes_me_feel_uncomfortable")&
        value=="Yes"~-n,
      value=="No"~-n,
      value=="Urgent Need"~-n,
      TRUE ~n),
      name = str_replace_all(name,paste(question_name,"_question_","",sep=""),""),
      name = str_replace_all(name,"_"," "))
}

correct_sentiment_emotional_substance_use = function(data)
{
  data %>%
    mutate(n = case_when(
      
      str_detect(name,"emotional_substance_use_question_five_i_would_like_more_information_about_support_services") &
        value=="No"~n,
      str_detect(name,"emotional_substance_use_question_five_i_would_like_more_information_about_support_services")&
        value=="Yes"~-n,
      str_detect(name,"emotional_substance_use_question_four_i_would_like_help_to_quit_using_tobacco") &
        value=="No"~n,
      str_detect(name,"emotional_substance_use_question_four_i_would_like_help_to_quit_using_tobacco")&
        value=="Yes"~-n,
      str_detect(name,"emotional_substance_use_question_one_substance_use_is_effecting_my_family_and_i_need_help") &
        value=="No"~n,
      str_detect(name,"emotional_substance_use_question_one_substance_use_is_effecting_my_family_and_i_need_help")&
        value=="Yes"~-n,
      str_detect(name,"emotional_substance_use_question_three_i_would_like_more_information_about_addiction") &
        value=="No"~n,
      str_detect(name,"emotional_substance_use_question_three_i_would_like_more_information_about_addiction")&
        value=="Yes"~-n,
      str_detect(name,"emotional_substance_use_question_three_i_would_like_more_information_about_addiction") &
        value=="No"~n,
      str_detect(name,"emotional_substance_use_question_three_i_would_like_more_information_about_addiction")&
        value=="Yes"~-n,
      value=="No"~-n,
      value=="Urgent Need"~-n,
      TRUE ~n),
      name = str_replace_all(name,paste(question_name,"_question_","",sep=""),""),
      name = str_replace_all(name,"_"," "))
}

correct_sentiment_emotional_children = function(data)
{
  data %>%
    mutate(n = case_when(
      
      str_detect(name,"emotional_children_question_five_i_would_like_to_enroll_my_children_in_pre_school_head_start_or_child_care") &
        value=="No"~n,
      str_detect(name,"emotional_children_question_five_i_would_like_to_enroll_my_children_in_pre_school_head_start_or_child_care")&
        value=="Yes"~-n,
      str_detect(name,"emotional_children_question_four_my_children_would_benefit_from_additional_educational_and_or_recreational_activities") &
        value=="No"~n,
      str_detect(name,"emotional_children_question_four_my_children_would_benefit_from_additional_educational_and_or_recreational_activities")&
        value=="Yes"~-n,
      str_detect(name,"emotional_children_question_one_i_have_concerns_about_my_children_having_behavioral_broblems_at_school_home_or_in_the_community") &
        value=="No"~n,
      str_detect(name,"emotional_children_question_one_i_have_concerns_about_my_children_having_behavioral_broblems_at_school_home_or_in_the_community")&
        value=="Yes"~-n,
      value=="No"~-n,
      value=="Urgent Need"~-n,
      TRUE ~n),
      name = str_replace_all(name,paste(question_name,"_question_","",sep=""),""),
      name = str_replace_all(name,"_"," "))
}

correct_sentiment_emotional_client = function(data)
{
  data %>%
    mutate(n = case_when(
      
      str_detect(name,"emotional_client_question_five_i_would_like_to_learn_more_about_my_cultural_heritage") &
        value=="No"~n,
      str_detect(name,"emotional_client_question_five_i_would_like_to_learn_more_about_my_cultural_heritage")&
        value=="Yes"~-n,
      str_detect(name,"emotional_client_question_four_do_you_or_someone_in_your_family_have_thoughts_of_suicide") &
        value=="No"~n,
      str_detect(name,"emotional_client_question_four_do_you_or_someone_in_your_family_have_thoughts_of_suicide")&
        value=="Yes"~-n,
      str_detect(name,"emotional_client_question_one_i_have_experience_a_traumatic_event_in_my_life") &
        value=="No"~n,
      str_detect(name,"emotional_client_question_one_i_have_experience_a_traumatic_event_in_my_life")&
        value=="Yes"~-n,
      str_detect(name,"emotional_client_question_three_i_believe_me_or_my_children_would_benefit_from_mental_health_services") &
        value=="No"~n,
      str_detect(name,"emotional_client_question_three_i_believe_me_or_my_children_would_benefit_from_mental_health_services")&
        value=="Yes"~-n,
      str_detect(name,"emotional_client_question_two_do_you_feel_sad_or_unhappy_frequently") &
        value=="No"~n,
      str_detect(name,"emotional_client_question_two_do_you_feel_sad_or_unhappy_frequently")&
        value=="Yes"~-n,
      value=="No"~-n,
      value=="Urgent Need"~-n,
      TRUE ~n),
      name = str_replace_all(name,paste(question_name,"_question_","",sep=""),""),
      name = str_replace_all(name,"_"," "))
}

correct_sentiment = function(data,question_name)
{
  if(question_name =="emotional_domestic_relationship")
  {
    out = data %>%
      correct_sentiment_emotional_domestic_relationship()
  }
  
  if(question_name=="emotional_substance_use")
  {
    out = data %>%
      correct_sentiment_emotional_substance_use()
  }
  
  if(question_name=="emotional_children")
  {
    out = data %>%
      correct_sentiment_emotional_children()
  }
  
  if(question_name=="emotional_client")
  {
    out = data %>%
      correct_sentiment_emotional_client()
  }
  
  return(out)
}

date_to_mmddyyyy = function(in_date)
{
  paste(month(in_date), day(in_date), year(in_date),sep = "/")
}
