# Toboggan Fence



library(tidyverse)
library(readxl)
library(lubridate)

fish <- read_excel("TobogganFence_MASTERcope29-dec-22.xlsx", 
                   sheet = "IndividualFish") %>% 
  mutate(julian = yday(date), 
         fake.date = as_date(julian, origin = ymd("2022-01-01")),
         start.date = yday(first(date)))
  
str(fish)
effort <- read_excel("TobogganFence_MASTERcope29-dec-22.xlsx", 
                     sheet="Effort") %>% 
  mutate(effort_id = 1:length(date))
str(effort)

catch.effort <- effort %>% 
  full_join(fish, by = c("year", "date", "time_of_day")) %>% 
  filter(year %in% c(2015:2022))
str(catch.effort)

catch.effort %>% 
  group_by(year) %>% 
  summarize(start.date = as_date(first(date)), 
            finish.date = as_date(last(date)),
            days.total = length(unique(date)),
            checks.total = length(unique(effort_id))) %>% 
  mutate(time.est1.5 = checks.total*1.5, 
         time.est2.0 = checks.total*2.0)

fish %>% 
  filter(species %in% "co") %>% 
  group_by(year) %>% 
  summarize(total.co = length(species))

ggplot(fish[fish$species %in% "co",])+
  geom_histogram(aes(x=fake.date))+
  #geom_vline(aes(xintercept = as_date(start.date, 
  #                                    origin = ymd("2022-01-01"))))+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks", date_labels= "%b-%d")+
  theme(axis.text.x = element_text(hjust= 1, angle=45))


