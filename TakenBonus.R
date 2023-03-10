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
library(DT)

tibble(Assessment_taken_bonuses_export_Sample)

#Let's clean our data first

Assessment_taken_bonuses_export_Sample %>%
  clean_names()


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


#Create the total amount of deposit
Assessment_taken_bonuses_export_Sample$total_deposit <- Assessment_taken_bonuses_export_Sample$number_of_deposits * Assessment_taken_bonuses_export_Sample$deposit_amount


total_cases<-Assessment_taken_bonuses_export_Sample %>%
  select(name, total_deposit) 

total_cases_world <- total_cases %>% 
  group_by(name) %>% 
  summarize(sum_total = sum(total_deposit)) %>% 
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

#Now let's analyze the bonus data
bonus_data = Assessment_taken_bonuses_export_Sample %>% 
  select(name, bonus_type, bonus_name, provider_name, freespins_campaign_id, slot_name, total_deposit)

by_bonus = bonus_data %>% 
  select(bonus_name, total_deposit,name) %>% 
  group_by(bonus_name)

data = Assessment_taken_bonuses_export_Sample %>% 
  select(name, bonus_type, bonus_name, provider_name, freespins_campaign_id, slot_name, total_deposit)

data %>% 
  select(bonus_name,total_deposit,name,bonus_type) %>% 
  datatable(., options = list(pageLength = 10))

BonusTop20 <- data %>% 
  arrange(desc(total_deposit)) %>% 
  head(n = 20) %>%
  as.data.frame()

BonusTop20 %>% 
  select(bonus_name,total_deposit,name,bonus_type) %>% 
  datatable(., options = list(pageLength = 10))


#Let's see the best bonus by deposit
highchart() %>% 
  hc_chart(type="column", options3d=list(enabled=TRUE, alpha=15, beta=15)) %>% 
  hc_xAxis(categories=BonusTop20$bonus_name) %>% 
  hc_add_series(data=BonusTop20$total_deposit, name="Bonus name") %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text="Top 20 Bonus name") 

#Let's take a look to the top 3 country: Japan, Finland and Germany

JapanBonus = subset(by_bonus, name %in% c("Japan"))
FinlandBonus = subset(by_bonus, name %in% c("Finland"))
GermanyBonus = subset(by_bonus, name %in% c("Germany"))

#Japan
highchart() %>% 
  hc_chart(type="column", options3d=list(enabled=TRUE, alpha=15, beta=15)) %>% 
  hc_xAxis(categories=JapanBonus$bonus_name) %>% 
  hc_add_series(data=JapanBonus$total_deposit, name="Bonus name") %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text="Japan Bonus name") 

#Finland
highchart() %>% 
  hc_chart(type="column", options3d=list(enabled=TRUE, alpha=15, beta=15)) %>% 
  hc_xAxis(categories=FinlandBonus$bonus_name) %>% 
  hc_add_series(data=FinlandBonus$total_deposit, name="Bonus name") %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text="Finland Bonus name") 

#Germany
highchart() %>% 
  hc_chart(type="column", options3d=list(enabled=TRUE, alpha=15, beta=15)) %>% 
  hc_xAxis(categories=GermanyBonus$bonus_name) %>% 
  hc_add_series(data=GermanyBonus$total_deposit, name="Bonus name") %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text="Germany Bonus name") 


#Let's analyze the bonus type
by_bonustype = Assessment_taken_bonuses_export_Sample %>% 
  select(bonus_type, total_deposit,name) 


by_bonustypet = by_bonustype %>% 
  group_by(name, bonus_type) %>% 
  summarize(totald = sum(total_deposit))

#Let's graphic by type of bonus and each country

by_bonustypet %>% 
  hchart(., type = "column", 
         hcaes(x = name, 
               y = totald, 
               group = bonus_type)) %>% 
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%"))


#Now let's take a look to the provider
by_provider = Assessment_taken_bonuses_export_Sample %>% 
  select(provider_name,total_deposit,name)


by_providert = by_provider %>% 
  group_by(name, provider_name) %>% 
  summarize(totald = sum(total_deposit))

by_providert %>% 
  hchart(., type = "column", 
         hcaes(x = name, 
               y = totald, 
               group = provider_name)) %>% 
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%"))


#By descriptor 
by_descrp = Assessment_taken_bonuses_export_Sample %>% 
  select(descriptor,total_deposit,name)

by_descriptor = by_descrp %>% 
  group_by(name, descriptor) %>% 
  summarize(totald = sum(total_deposit))

by_descriptor %>% 
  hchart(., type = "column", 
         hcaes(x = name, 
               y = totald, 
               group = descriptor)) %>% 
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%"))


#Campaings by country
by_camp = Assessment_taken_bonuses_export_Sample %>% 
  select(`campaigns ID`,total_deposit,name)

campaing = by_camp %>% 
  group_by(name, `campaigns ID`) %>% 
  summarize(totald = sum(total_deposit))


campg = campaing %>% 
 count(name)

campg2 = campaing %>% 
  select(name, totald) %>% 
  summarize(totald2 = sum(totald))

colnames(campg)[2] = "number_campaings"

campaingt = merge(x = campg, y = campg2, by = c("name"))

campaingt %>% 
  arrange(desc(number_campaings)) %>% 
  hchart(., type = "column", 
         hcaes(x = name, 
               y = number_campaings, 
               group = totald2)) %>% 
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%"))
