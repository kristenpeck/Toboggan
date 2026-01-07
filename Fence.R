
# Toboggan Fence

library(tidyverse)
library(readxl)
library(lubridate)

fish <- read_excel("TobogganFenceData_MASTER-7-Apr-2025.xlsx", 
                   sheet = "IndividualFish",
                   col_types = c("guess","guess","guess",
                                 "guess","guess","guess",
                                 "guess","guess","numeric",
                                 "guess","guess","guess",
                                 "guess","guess","guess",
                                 "guess","guess","guess",
                                 "guess","guess","guess",
                                 "guess","guess","guess",
                                 "guess","guess","guess",
                                 "guess","guess","guess")) %>% 
  mutate(julian = yday(date), 
         fake.date = as_date(julian-1, origin = ymd("2024-01-01")),
         start.date = yday(first(date)),
         gender = recode(mark_gender, "af"="F", "wf"="F","am"="M",
                         "wm"="M","wj"="M","aj"="M"),
         mark = recode(mark_gender, "af"="a", "wf"="w","am"="a",
                       "wm"="w","wj"="w","aj"="a"))
  
str(fish)
effort <- read_excel("TobogganFenceData_MASTER-7-Apr-2025.xlsx", 
                     sheet="Effort", 
                     col_types = c("guess","guess","guess",
                                   "guess","date","date",
                                   "guess","guess","guess",
                                   "guess","guess","guess",
                                   "guess","guess","guess",
                                   "guess","guess","guess",
                                   "guess","guess","guess",
                                   "guess","guess","guess",
                                   "guess","guess","guess",
                                   "guess","guess","guess")) %>% 
  mutate(effort_id = 1:length(date)) %>% 
  mutate(duration = difftime(departure_time,arrival_time,units = "hours"))
str(effort)

# how many hours worked?

effort %>% 
  group_by(year) %>% 
  summarize(num.hrs = sum(duration, na.rm=T))



#quick summaries:
unique(fish$mark_gender)
unique(fish$species)
fish[which(is.na(fish$mark)),"mark_gender"]

(tmp <- fish %>% 
  select(year, date, total, species, mark_gender, mark, gender) %>% 
  filter(species %in% "co") %>% 
  group_by(year) %>% 
  summarize(total = sum(total, na.rm=T)))

(tmp <- fish %>% 
  select(year, date, total, species, mark_gender, mark, gender) %>% 
  filter(species %in% "co") %>% 
  group_by(year, gender) %>% 
  summarize(total = sum(total, na.rm=T)))

# daily catch of coho over the season

#remove the fish that came through the fence twice:
no.repeats.fish <- fish %>% 
  mutate(remove = ifelse(recap_tag_colour %in% "orange" & year %in% c(2022,2024),T,
                         ifelse(recap_tag_colour %in% "pink" & year %in% 2020,T,NA))) %>% 
  filter(is.na(remove))


daily.co <- no.repeats.fish %>% 
  filter(year %in% 2024) %>%
  full_join(effort[,c("year", "date", "time_of_day")],by = c("year", "date", "time_of_day")) %>% 
  filter(year %in% 2024) %>% 
  mutate(co.total = ifelse(species %in% "co", total, 0), 
         co.w.total = ifelse(mark %in% "w", total, 0), 
         co.a.total = ifelse(mark %in% "a", total, 0), 
         co.w.broodstock = ifelse(broodstock %in% TRUE, total, 0)) %>% 
  group_by(year, date) %>% 
  summarize(total = sum(co.total, na.rm=T), co.w = sum(co.w.total), co.w.brood = sum(co.w.broodstock), 
            co.a = sum(co.a.total)) %>% 
  mutate(yday = yday(date), fake.date = as_date(yday-1, origin = "2024-01-01"))  
  
write_csv(daily.co, "daily_co2024.csv")





plot.daily.co2324 <- ggplot(daily.co)+
  geom_bar(aes(x=fake.date, y=total), stat = "identity")+
  facet_wrap(~year, nrow=2)+
  labs(x="date",y="total daily coho")+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1, angle=45))
plot.daily.co2324

# ggsave(plot = plot.daily.co2324, filename = "plot.daily.co2324.png", device="png",
#        width=6, height=4)

#interpolating missing fish in lay down year
library(tidyhydat)

hy_stations(station_number = "08EE012")
simp.flows.real.raw <- realtime_dd(station_number = "08EE012")

simp.flows.real.raw <- read_csv("08EE012_QR_20241129T0104.csv",skip = 10,
                               col_types = c("c","?","?","?","?"),
                               col_names = c("Date","Parameter","Value","Approval","Qualifier")) %>% 
  mutate(Date = ymd(substr(Date,1,10)), Parameter = "Flow") %>% 
  group_by(Date, Parameter) %>% 
  summarize(ave.flow = mean(Value, na.rm=T)) %>% #ave daily flow 
  select(Date,Parameter, Value=ave.flow) %>% 
  mutate(Year = year(Date), fyear = as.factor(Year),
         julian = yday(Date),fake.date = as_date(julian-1,origin="2024-01-01"),
         STATION_NUMBER = "08EE012")


plot.daily.co.adult <- ggplot(daily.co[daily.co$year %in% 2024,])+
  geom_bar(aes(x=fake.date, y=total), stat = "identity")+
  geom_line(data = simp.flows.real.raw, aes(x=fake.date, y= Value*100), col="blue")+
  facet_wrap(~year)+
  labs(x="date",y="total daily coho")+
  scale_x_date(limits = c(ymd("2024-08-12"),ymd("2024-11-01")), 
               date_breaks = "1 week", date_labels = "%b-%d")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1, angle=45))
plot.daily.co.adult

# ggsave(plot = plot.daily.co.adult, filename = "plot.daily.co.adult.png", device="png",
#        width=6, height=4)

#calc effort 

catch.effort <- effort %>% 
  full_join(fish, by = c("year", "date", "time_of_day")) %>% 
  filter(year %in% c(2015:2024))
str(catch.effort)

catch.effort %>% 
  group_by(year) %>% 
  summarize(start.date = as_date(first(date)), 
            finish.date = as_date(last(date)),
            days.total = length(unique(date)),
            checks.total = length(unique(effort_id))) %>% 
  mutate(time.est1.5 = checks.total*1.5, 
         time.est2.0 = checks.total*2.0)

#how many days with over 100 CO?
catch.effort.by.date <- catch.effort %>% 
  filter(species %in% "co") %>% 
  group_by(year, date) %>% 
  summarize(total.co = length(species)) 
catch.effort.by.date %>% 
  filter(total.co >=100) %>% 
  group_by(year) %>% 
  summarize(days.over.100  = length(total.co))
catch.effort.by.date %>% 
  filter(total.co >=200) %>% 
  group_by(year) %>% 
  summarize(days.over.200  = length(total.co))


fish %>% 
  filter(species %in% "co") %>% 
  group_by(year) %>% 
  summarize(total.co = length(species))

ggplot(fish[(fish$species %in% "co"),])+
  geom_histogram(aes(x=fake.date))+
  #geom_vline(aes(xintercept = as_date(start.date, 
  #                                    origin = ymd("2022-01-01"))))+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks", date_labels= "%b-%d")+
  theme(axis.text.x = element_text(hjust= 1, angle=45))

ggplot(fish[fish$species %in% "co",])+
  geom_histogram(aes(x=fake.date), binwidth = 1)+
  #geom_vline(aes(xintercept = as_date(start.date, 
  #                                    origin = ymd("2022-01-01"))))+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "1 week", date_labels= "%b-%d")+
  theme(axis.text.x = element_text(hjust= 1, angle=45))

# if only pink tagged the first 20 fish of each day, what percentage of fish would be tagged?
# by year, which days had over 20 fish? Truncate to 20 fish and compare with total

catch.effort.by.date %>% 
  mutate(total.co.under20 = ifelse(total.co <= 20, total.co, 20)) %>% 
  group_by(year) %>% 
  summarize(total.co = sum(total.co), total.co.under20 = sum(total.co.under20)) %>% 
  mutate(percent.tagged = total.co.under20/total.co*100)

#if only the tags before Oct 15th ish are counted:
yday(ymd("2023-Oct-15"))

catch.effort.by.date %>% 
  mutate(total.co.under20 = ifelse(total.co <= 20, total.co, 20)) %>% 
  mutate(julian = yday(date),
           prepost = ifelse(julian <= 288, "pre","post")) %>% 
  filter(prepost %in% "pre") %>% 
  group_by(year) %>% 
  summarize(total.co = sum(total.co), total.co.under20 = sum(total.co.under20)) %>% 
  mutate(percent.tagged = total.co.under20/total.co*100)  
  
#only tagging the first 10 of each day:
catch.effort.by.date %>% 
  mutate(total.co.under10 = ifelse(total.co <= 10, total.co, 10)) %>% 
  mutate(julian = yday(date),
         prepost = ifelse(julian <= 288, "pre","post")) %>% 
  filter(prepost %in% "pre") %>% 
  group_by(year) %>% 
  summarize(total.co = sum(total.co), total.co.under10 = sum(total.co.under10)) %>% 
  mutate(percent.tagged = total.co.under10/total.co*100)


# 2023 Scalebook subsampling ####

library(FSA)

fish23 <- read_excel("Toboggan_Adult_Fence_2023_copy20-Dec-2023.xlsx", 
           sheet="Individual_fish")
head(fish23)
unique(fish23$broodstock)

meh <- data.frame(scale_book = c(1002525,1022865,1002523,1022855,1022857),
                  no_good = "x")


scales <- fish23 %>% 
  filter(!is.na(scale_book)) %>% 
  left_join(meh, by="scale_book") %>% 
  mutate(catFL = lencat(fork_length_mm,
                        w = 50)) 


ggplot(scales)+
  geom_histogram(aes(x=fork_length_mm, fill=broodstock))

ggplot(scales)+
  geom_histogram(aes(x=fork_length_mm, fill=no_good))+
  facet_wrap(~scale_book)
  
  









  
  
  
