
# This script covers the analysis of Toboggan Creek smolt data 

#Created: 7-Feb-2023
# By : Kristen Peck

# Libraries

library(tidyverse)
library(lubridate)
library(readxl)
library(recapr)

options(scipen=999)

# Load the data 
year.summaries <- read_excel("Toboggan-YearSummaries_COPY7-Feb-2023.xlsx", sheet="TobogganFence")

cwt <- year.summaries %>% 
  select(year=Year,CWTreleased) 

effort <- read_excel("Toboggan_smolt_dataentry_COPY7-Feb-2023.xlsx", sheet="effort") %>% 
  mutate(year = year(date))
fish <- read_excel("Toboggan_smolt_dataentry_COPY7-Feb-2023.xlsx", sheet="individualfish") %>% 
  mutate(year=year(date),arrive.hr = hour(arrival_time), arrive.min = minute(arrival_time),
         arrive.datetime = ymd_hm(paste(date,arrive.hr,arrive.min))) %>% 
  mutate(julian = yday(date), fake.date = as_date(julian, origin = "2022-01-01"))

str(effort)
str(fish)

# summarize effort:

effort %>% 
  group_by(year) %>% 
  summarize(start.date = as_date(first(date)), 
            finish.date = as_date(last(date)),
            days.total = length(unique(date)))

#timing of adipose and wild co captures through season:
unique(fish$species)

(fish.sum <- fish %>% 
  group_by(year) %>% 
  summarize(total.co.w = length(which(species %in% "co-w")),
            total.co.a = length(which(species %in% "co-a")),
            total.co.f = length(which(species %in% "co-f"))))

co.fish <- fish %>% 
  filter(species %in% c("co-f","co-w","co-a")) %>% 
  group_by(date,fake.date, year) %>% 
  summarize(daily.co.w = length(which(species %in% "co-w")),
            daily.co.a = length(which(species %in% "co-a")),
            daily.co.f = length(which(species %in% "co-f"))) %>% 
  left_join(fish.sum, by= "year") %>% 
  mutate(prop.daily.co.w = daily.co.w/total.co.w,
         prop.daily.co.a = daily.co.a/total.co.a,
         prop.daily.co.f = daily.co.f/total.co.f)




plot.co.timing <- ggplot(co.fish)+
  geom_point(aes(x=fake.date, y=prop.daily.co.w), col="black")+
  geom_point(aes(x=fake.date, y=prop.daily.co.a), col="blue")+
  geom_line(aes(x=fake.date, y=prop.daily.co.w), col="black")+
  geom_line(aes(x=fake.date, y=prop.daily.co.a), col="blue")+
  facet_wrap(~year, nrow=3)+
  scale_x_date(date_breaks = "1 week",date_labels = "%b-%d", 
               minor_breaks = "1 week")+
  labs(fill="", x="date", y="proportion of coho per day (black=wild, blue=hatchery)")

plot.co.timing
ggsave(plot.co.timing, filename = "plot.co.timing.png",width = 6, height = 4)


# estimate LP from marked (adipose-clipped) and wild coho

pooled.pop.estimate <- co.fish %>% 
  group_by(year) %>% 
  summarize(adipose.caught = length(which(species %in% "co-a")),
            total.caught = length(which(species %in% c("co-a","co-w"))),
            wild.caught = length(which(species %in% "co-w"))) %>% 
  left_join(cwt) %>% 
  mutate(LP = CWTreleased*total.caught/adipose.caught) %>% 
  mutate(Chap = NChapman(CWTreleased,total.caught, adipose.caught)) %>% 
  mutate(vChap = vChapman(CWTreleased,total.caught, adipose.caught)) %>% 
  mutate(sdChap = sqrt(vChap)) %>% 
  mutate(CV = sdChap/Chap*100) %>% 
  mutate(seChap = seChapman(CWTreleased,total.caught, adipose.caught)) %>% 
  mutate(CI95Chap = seChap*1.96)

pooled.pop.estimate
#write_csv(pooled.pop.estimate, "pooled.pop.estimateALL.csv")


ggplot(pooled.pop.estimate)+
  geom_point(aes(x=year,y=Chap))+
  geom_errorbar(aes(x=year, ymax = Chap+CI95Chap, ymin = Chap-CI95Chap), width=0.1)+
  scale_y_continuous(breaks=seq(0,max(pooled.pop.estimate$Chap+pooled.pop.estimate$CI95Chap),10000))+
  scale_x_continuous(breaks = seq(min(pooled.pop.estimate$year), max(pooled.pop.estimate$year),1))
  





#### Subset ####
#If trap was only operated for four days of each week (currently can only do for 2022)

effort2022 <- read_excel("toboggan2022_smolt_dataentry_COPY7-Feb-2023.xlsx", sheet="effort")
fish2022 <- read_excel("toboggan2022_smolt_dataentry_COPY7-Feb-2023.xlsx", sheet="individualfish")

#create subset of days (4 days/wk) :

yrs <- data.frame(date = seq(ymd("2020-01-01"), ymd("2022-12-31"),1)) %>% 
  mutate(week = week(date), year = year(date), wday =wday(date))

 
#  mutate(day.in.week = ifelse(year %in% c(2021,2022),c(rep(1:7,52),1),c(rep(1:7,52),2)))

effort %>% 
  group_by(year(date)) %>% 
  summarize(first.date = first(date), last.date = last(date))

duration <- yrs %>% 
  mutate(running = ifelse(date %in% c(ymd("2020-05-04"):ymd("2020-07-15")),T, F)) %>% 
  mutate(running = ifelse(date %in% c(ymd("2021-05-03"):ymd("2021-06-25")),T, running)) %>% 
  mutate(running = ifelse(date %in% c(ymd("2022-05-02"):ymd("2022-07-06")),T, running)) %>% 
  filter(wday %in% c(4:7), running %in% T) %>% 
  select(date,wday)
  
subset <- duration %>% 
  left_join(fish, by="date") %>% 
  mutate(species = ifelse(is.na(species), "NFC", species), year = year(date)) %>% 
  filter(species %in% c("co-f","co-w","co-a"))

#calc pop estimate with subset
pooled.pop.estimate.sub <- subset %>% 
  group_by(year) %>% 
  summarize(adipose.caught = length(which(species %in% "co-a")),
            total.caught = length(which(species %in% c("co-a","co-w"))),
            wild.caught = length(which(species %in% "co-w"))) %>% 
  left_join(cwt) %>% 
  mutate(LP = CWTreleased*total.caught/adipose.caught) %>% 
  mutate(Chap = NChapman(CWTreleased,total.caught, adipose.caught)) %>% 
  mutate(vChap = vChapman(CWTreleased,total.caught, adipose.caught)) %>% 
  mutate(sdChap = sqrt(vChap)) %>% 
  mutate(CV = sdChap/Chap*100) %>% 
  mutate(seChap = seChapman(CWTreleased,total.caught, adipose.caught)) %>% 
  mutate(CI95Chap = seChap*1.96)

pooled.pop.estimate.sub
#write_csv(pooled.pop.estimate.sub, "pooled.pop.estimate.sub.csv")

ggplot(subset)+
  geom_histogram(aes(x=fake.date, fill=species), binwidth = 5,col="black", 
                 position="dodge")+
  facet_wrap(~year, nrow=3)+
  scale_x_date(date_breaks = "2 weeks",date_labels = "%b-%d", minor_breaks = "1 week")+
  labs(fill="")

plot.pooled.LP.subset <- ggplot()+
  geom_point(data = pooled.pop.estimate.sub, aes(x=year+0.1,y=Chap), col="gray50")+
  geom_errorbar(data = pooled.pop.estimate.sub, 
                aes(x=year+0.1, ymax = Chap+CI95Chap, ymin = Chap-CI95Chap), col="gray50", width=0.1)+
  geom_point(data = pooled.pop.estimate, aes(x=year,y=Chap))+
  geom_errorbar(data = pooled.pop.estimate, 
                aes(x=year, ymax = Chap+CI95Chap, ymin = Chap-CI95Chap),width=0.1)+
  scale_y_continuous(breaks=seq(0,max(pooled.pop.estimate$Chap+pooled.pop.estimate$CI95Chap),10000))+
  scale_x_continuous(breaks = seq(min(pooled.pop.estimate$year), max(pooled.pop.estimate$year),1))+
  labs(x="Year", y="Pooled Chapman +/- 95 CI", title = "Toboggan Smolt Project - grey is 4 days/week, black is all 7 days")

plot.pooled.LP.subset

# ggsave(plot = plot.pooled.LP.subset, filename="plot.pooled.LP.subset4-7.png", width = 6.5,
#        height=4)


# check out size at age

yr.select = "2003"

ages <- read_excel("Ages2_NUSEDS_Toboggan_30-Jan-2023.xlsx", sheet = "Data") %>% 
  select(gr.age = `GR Age`, eu.age = `EU Age`, age.code = `Part Age Code`,
         address=`Container Address`,
         sp = Species,source = `Sample Source`,life.stage = `Life History Stage`,
         start.date = `Sample Start Date`,end.date=`Sample End Date`,
         label = `Container Label`) %>% 
  mutate(year = year(start.date)) %>% 
  filter(year %in% yr.select, life.stage %in% "Juvenile") %>% 
  mutate(#scale_book = as.numeric(substr(label, 5, 9)),
         scale_number = as.numeric(address))

str(ages)

#2020 to 2022 ages
aged.fish <- fish %>% 
  filter(!is.na(scale_number), year %in% yr.select) %>% 
  inner_join(ages) %>% 
  filter(is.na(age.code))

tob03 <- read_excel("TobogganSmolts2003to2010.xls", sheet="2003 Coho Smolts") %>% 
  select(year = `YEAR (SAMPLING EVENT)`,fork_length_mm = `NOSE FORK LENGTH (mm)`,
         wt = `BODY WEIGHT (gm)`,scale_book = `SCALE BOOK NUMBER`, 
         scale_number = `SCALE NUMBER`) %>% 
  left_join(ages) %>% 
  filter(is.na(age.code))
  
str(tob03)





ggplot(tob03)+
  geom_boxplot(aes(x=eu.age, y= fork_length_mm))+
  geom_jitter(aes(x=eu.age, y= fork_length_mm),width = .1) +
  scale_y_continuous(breaks = seq(min(tob03$fork_length_mm)-1,
                                max(tob03$fork_length_mm)+1,10))



