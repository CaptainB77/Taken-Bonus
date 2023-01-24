usethis::edit_git_config()
usethis::use_git()
use_github()
usethis::use_github()


library('tidyverse') # essential R packages for data science
library('data.table')

library('ggplot2') # visualization
library("ggtext")
library("highcharter")
library('gganimate')

library('dplyr') # data manipulation
library('readr') # input/output
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('janitor') # data cleaning
library('scales')

tibble(Assessment_taken_bonuses_export_Sample)

# renaming location to name for mapping
colnames(Assessment_taken_bonuses_export_Sample)[2]<-"name"

Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="FI"]<- "Finland"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="JP"]<- "Japan"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="NZ"]<- "New Zealand"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="AT"]<- "Austria"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="BE"]<- "Belgium"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="CA"]<- "Canada"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="CH"]<- "Switzerland"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="CK"]<- "Cook Islands"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="CY"]<- "Cyprus"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="CZ"]<- "Czech Republic"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="DE"]<- "Germany"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="IE"]<- "Republic of Ireland"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="LU"]<- "Luxembourg"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="NO"]<- "Norway"
Assessment_taken_bonuses_export_Sample$name[Assessment_taken_bonuses_export_Sample$name=="UA"]<- "Ukraine"


total_cases<-Assessment_taken_bonuses_export_Sample %>%
  select(name, deposit_amount) 

total_cases_world <- total_cases %>% 
  group_by(name) %>% 
  summarize(sum_total = sum(deposit_amount)) %>% 
  arrange(desc(sum_total)) 

#Mapping of total deposit

highchart() %>%
  hc_add_series_map(worldgeojson, df = total_cases_world, value = "sum_total", joinBy = "name") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_bloom()) %>% 
  hc_colorAxis(minColor = "lightblue", maxColor="mediumblue") %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "World Total Deposit", style = list(fontSize = "25px")) %>%
  hc_subtitle(text="Total Deposit Amount: 27,828", style=list(fontsize = "15px")) %>% 
  hc_add_theme(hc_theme_economist())

#Graphic of the cases by country

total_cases_world %>% 
  arrange(desc(sum_total)) %>% 
  slice(1:15)

highchart() %>% 
  hc_chart(type="column", options3d=list(enabled=TRUE, alpha=15, beta=15)) %>% 
  hc_xAxis(categories=total_cases_world $name) %>% 
  hc_add_series(data=total_cases_world$sum_total, name="Country") %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text="Total deposit by country")
