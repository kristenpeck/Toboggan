#### 2025 ####

#split off from main on 22-Jan-2026 because formatting is different

# note that a bunch of changes were made for 2025 after 2024 - co-a was used to 
# identify any adipose clipped fish, and the caudal clip and location was used 
# to ID them between hatchery and wild recaps. Also there was the Friday Fish 
# event, where the CWT taggers were down for a few days and fish from the barn were
# released with only a top caudal clip. Any recaps were subsequently tagged on 
# RECAPTURE at the fence. The barn trap also shut down earlier than the Fence trap
# because the landowner requested that happen. To compensate, unmarked fish caught at the 
# Fence were caudal clipped and transported upstream to be released at the barn site, 
# therefore becoming "barn" fish. So those fish (after June 16th) were both in the C of the fence 
# and also the M of the barn. 
# Fish size class was tidied up and reclassified to more obviously named categories based on 
# head mold bracket, and retention checks were done for each size class

# Libraries

library(tidyverse)
library(lubridate)
library(readxl)
library(recapr)

options(scipen=999)

##### CWT retention ####

CWT25 <- read_excel("toboggan_smolt_dataentry_2025_copy12-Jan-2026.xlsx",
                    sheet="cwt_log") %>%
  mutate(isoweek = isoweek(tag_date),
         mort_n = ifelse(is.na(mort_n),0,mort_n),
         sacrifice_n = ifelse(is.na(sacrifice_n),0,sacrifice_n))
str(CWT25)

CWTretention25 <- CWT25 %>%
  filter(!is.na(r_check_date)) %>%
  select(-c(inj_n,mort_n,sacrifice_n)) %>%
  mutate(retention.rt = r_retention_n/r_sample_size) %>%
  group_by(isoweek, head_mold_size, tag_code) %>%
  summarize(weekly.retention = ifelse(mean(retention.rt, na.rm=T) %in% NaN, 1,
                                        mean(retention.rt, na.rm=T)),
            n.retagged = sum(r_re_tagged_n))
CWTretention25

n.retagged25 <- CWTretention25 %>%
  group_by(tag_code) %>%
  summarize(n.retagged = sum(n.retagged))

#size and headmold key

size.headmold.key25 <- data.frame(head_mold_size = c(NA,unique(CWT25$head_mold_size)),
                                  category = c("XS","S","M","L"),
                                  length_range_mm = c("<70","70-85","86-115","116-190"))

# Indiv fish - remake into first tagging and recap in same columns with sacrifice or mort noted ####
fish25 <- read_excel("toboggan_smolt_dataentry_2025_copy12-Jan-2026.xlsx",
                     sheet="individualfish") %>%
  filter(species %in% c("co-w","co-a"), size_cwt %in% c(NA, "S","M","L")) %>%
  mutate(fate = ifelse(!is.na(sacrifice),"sacrifice",
                       ifelse(!is.na(mort),"mort","live"))) %>%
  mutate(tag.status = ifelse(!is.na(a_adclip) & fate %in% "live","A",
                             ifelse(!is.na(r_adclip),"R",NA))) %>%
  mutate(isoweek = isoweek(date), yday = yday(date),week.yday = as.numeric(paste0(isoweek,".",yday)),
         head_mold_size = `head mold (lbs)`) %>%  #this week starts on a monday, which matches our mark switch day
  select(-`head mold (lbs)`)

# CWT tag summary - individuals ####

CWTlog.indiv25 <- fish25 %>%
  filter(!is.na(tag_code), fate %in% "live") %>%
  group_by(isoweek,date, site, tag_code, head_mold_size) %>%
  summarize(inj_n = sum(ifelse(!is.na(tag_code),count,0), na.rm = TRUE)) %>%    #note that in this version, sacrifices and morts are not included in injected
  left_join(CWTretention25, by=c("isoweek","tag_code","head_mold_size"))
CWTlog.indiv25

CWTlog.indiv25summary <- CWTlog.indiv25 %>% #use mean retention when did not have week/code-specific
  mutate(weekly.retention = ifelse(is.na(weekly.retention),mean(CWTlog.indiv25$weekly.retention, na.rm=T),
                                   weekly.retention),
  retained.est = inj_n*weekly.retention) %>%
  group_by(tag_code) %>%
  summarize(injected = sum(inj_n),est.retained = round(sum(retained.est),0)) %>%
  mutate(estd.lost = injected-est.retained) %>%
  left_join(n.retagged25, by="tag_code") %>%
  mutate(est.oceanreleases = est.retained+n.retagged)
#write_csv(CWTlog.indiv24summary,"CWT24logindiv.csv")
CWTlog.indiv25summary

ave.CWTmeristics25 <- fish25 %>%
  group_by(tag_code) %>%
  summarize(n.measured = length(!is.na(fork_length_mm)), ave.FL = round(mean(fork_length_mm, na.rm=T),1),
            sd.FL = round(sd(fork_length_mm, na.rm=T),1), ave.wt = round(mean(weight_g, na.rm=T),1),
            sd.wt = round(sd(weight_g, na.rm=T),1))
ave.CWTmeristics25

CWT.tag.summary2025 <- CWTlog.indiv25summary %>%
  left_join(ave.CWTmeristics25, by="tag_code") %>%
  arrange(ave.FL)
CWT.tag.summary2025 # copy over to report
#write_csv(CWT.tag.summary2025,"CWT.tag.summary2025.csv")





##### Effort ####

effort25 <- read_excel("toboggan_smolt_dataentry_2025_copy12-Jan-2026.xlsx",
                     sheet="effort",
                     col_types = c("guess","date","text","text","text",
                                   "date","date","guess","guess",
                                   "guess","guess","guess","guess"
                                   )) %>%
  mutate(year = year(date),
         arrival.hr = substr(arrival_time_24h,12,13),
         arrival.min = substr(arrival_time_24h,15,16),
         depart.hr = substr(departure_time_24h,12,13),
         depart.min = substr(departure_time_24h,15,16),
         arrival.datetime = ymd_hm(paste(date,arrival.hr,arrival.min,sep="-")),
         depart.datetime = ymd_hm(paste(date,depart.hr,depart.min,sep="-")),
         duration = depart.datetime-arrival.datetime,
         isoweek = isoweek(date))
         #crew.duration = duration*crew.members)

# summarize effort:

effort25 %>%
  group_by(year) %>%
  summarize(start.date = as_date(first(date)),
            finish.date = as_date(last(date)),
            days.total = length(unique(date)),
            duration.total = sum(duration, na.rm=T)/60)

ggplot(effort25)+
  geom_bar(aes(x=date,y=duration, fill=type), stat="identity")


#### summarize fish marking/recapture ####

#QA checks - are there any fish with A and R? Fix
#Recode size into categories

fish25 %>%
  filter(!is.na(a_adclip)&!is.na(r_adclip))
#should be 0

fish.co.w <- fish %>%
  filter(species %in% "co-w") %>%
  mutate(mark.status = ifelse(!is.na(a_adclip),"A",
                               ifelse(!is.na(r_adclip),"R",NA)),
         size_cwt.calc = ifelse(fork_length_mm < 70, "little",
                                ifelse(fork_length_mm>=70&fork_length_mm<=85, "small",
                                       ifelse(fork_length_mm>=86&fork_length_mm<=115, "big",
                                              ifelse(fork_length_mm>=116, "bigger", NA)))),
         size_cwt = ifelse(is.na(size_cwt),size_cwt.calc))
tmp <- fish.co.w %>%
  select(date, site, fork_length_mm, size_cwt, size_cwt.calc)

tmp[which(tmp$size_cwt!=tmp$size_cwt.calc),]



#Recode weekly mark








# (fish.sum <- fish %>%
#   group_by(year) %>%
#   summarize(total.co.w = length(which(species %in% "co-w")),
#             total.co.a = length(which(species %in% "co-a")),
#             total.co.f = length(which(species %in% "co-f"))))


co.new <- fish %>%
  filter(species %in% c("co-w") & a_adclip %in% c("yes")) %>%
  group_by(fake.date,site) %>%
  summarize(total.marked = sum(count))

co.recaps <- fish %>%
  filter(r_adclip %in% c("yes")) %>%
  group_by(fake.date, site) %>%
  summarize(total.recaps = sum(count)) %>%
  full_join(co.new)

co.fish.stack <- co.recaps %>%
  ungroup() %>%
  select(fake.date, site, total.marked, total.recaps) %>%
  pivot_longer(!c(fake.date, site), names_to="status",values_to = "count")



# co.fish <- fish %>%
#   ungroup() %>%
#   filter(species %in% c("co-w","co-a")) %>%
#   group_by(date,fake.date, site) %>%
#   summarize(daily.co.w = ifelse(species %in% "co-w", sum(count),
#                               NA)) %>%
#   left_join(fish.sum, by= "year") %>%
#   mutate(prop.daily.co.w = daily.co.w/total.co.w,
#          prop.daily.co.a = daily.co.a/total.co.a,
#          prop.daily.co.f = daily.co.f/total.co.f)


plot.co.daily <- ggplot(co.fish.stack)+
  geom_bar(stat="identity",aes(x=fake.date, y=count, fill=status))+
  facet_wrap(~site, nrow=2)+
  labs(x="date",y="daily count", fill="")+
  theme_bw()+
  scale_x_date(date_breaks="1 week", date_labels = "%d-%b")+
  theme(axis.text.x = element_text(hjust=1, angle=45))+
  theme(legend.position = "bottom")
plot.co.daily

ggsave(plot=plot.co.daily, filename = "plot.co.daily.png",
       width=6, height=4)


# plot.co.timing <- ggplot(co.fish)+
#   geom_point(aes(x=fake.date, y=prop.daily.co.w), col="black")+
#   geom_point(aes(x=fake.date, y=prop.daily.co.a), col="blue")+
#   geom_line(aes(x=fake.date, y=prop.daily.co.w), col="black")+
#   geom_line(aes(x=fake.date, y=prop.daily.co.a), col="blue")+
#   facet_wrap(~year, nrow=3)+
#   scale_x_date(date_breaks = "1 week",date_labels = "%b-%d",
#                minor_breaks = "1 week")+
#   labs(fill="", x="date", y="proportion of coho per day (black=wild, blue=hatchery)")
#
# plot.co.timing
# ggsave(plot.co.timing, filename = "plot.co.timing.png",width = 6, height = 4)


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

# #2020 to 2022 ages
# aged.fish <- fish %>%
#   filter(!is.na(scale_number), year %in% yr.select) %>%
#   inner_join(ages) %>%
#   filter(is.na(age.code))
#
# tob03 <- read_excel("TobogganSmolts2003to2010.xls", sheet="2003 Coho Smolts") %>%
#   select(year = `YEAR (SAMPLING EVENT)`,fork_length_mm = `NOSE FORK LENGTH (mm)`,
#          wt = `BODY WEIGHT (gm)`,scale_book = `SCALE BOOK NUMBER`,
#          scale_number = `SCALE NUMBER`) %>%
#   left_join(ages) %>%
#   filter(is.na(age.code))
#
# str(tob03)
#
#
#
#
#
# ggplot(tob03)+
#   geom_boxplot(aes(x=eu.age, y= fork_length_mm))+
#   geom_jitter(aes(x=eu.age, y= fork_length_mm),width = .1) +
#   scale_y_continuous(breaks = seq(min(tob03$fork_length_mm)-1,
#                                 max(tob03$fork_length_mm)+1,10))



