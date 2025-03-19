library(tidyverse)
library(sf)
library(leaflet)
library(arrow)
library(plotly)
library(dataRetrieval)
library(patchwork)


# want to update to do this by code, but for 
# now the first step is to query detections
# within the South Fork Clearwater during
# the spawn year of interest and preceding year,
# and save those results in the detections folder
# of this project. This code will filter
# to get only the most recent spawn year available
# and will drop any detections of fish tagged
# as juveniles that are detected in the 
# same year as tagging

# once that part is done, read in the output here

dat <- read_csv("data/SF Clearwater STHD.csv") %>% 
  mutate(observation_sitecode=word(`Site Name`,1,sep=" "),
         release_sitecode=word(`Release Site Name`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time Value`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         observation_month=month(observation_datetime),
         observation_year=year(observation_datetime),
         spawn_year=ifelse(observation_month>6,(observation_year+1),
                           observation_year),
         release_datetime=mdy(`Release Date MMDDYYYY`),
         release_year=year(release_datetime),
         yrs_at_large=observation_year-release_year) %>% 
  select(pit_id=`Tag Code`,rear_type=`Rear Type Code`,
         release_sitecode,release_lifestage=`Mark Life Stage Value`,
         release_datetime,release_year,
         observation_sitecode,
         observation_datetime,observation_month,
         observation_year,
         yrs_at_large,
         spawn_year,
         length_mm=`Mark Length mm`) %>% 
  mutate(most_recent=max(spawn_year,na.rm=T)) %>% 
  filter(spawn_year==most_recent) %>% 
  filter(yrs_at_large!=0 | release_lifestage != "Juvenile")

# get a table of unique pit
# ids to be run through the PTAGIS complete
# tag history 

int.dat <- dat %>% 
  ungroup() %>% 
  select(pit_id) %>% 
  distinct()

# write to the int_files folder

write.table(int.dat,"data/inseason_int.txt",
            quote=FALSE,row.names=FALSE,col.names=FALSE)

# another part to automate, run
# that output text file through the 
# complete tag history in PTAGIS
# and export the results to the
# taghistory folder

# make a key for life stage at tagging
# based on pit id

lifestage.key <- dat %>% 
  group_by(pit_id) %>% 
  summarize(life_stage=first(release_lifestage))

# now read in the interrogation results for the adult-marked
# and compress to unique day/event site combos

inseason.int <- read_csv("data/Complete Tag History.csv") %>% 
  mutate(observation_datetime=as.POSIXct(`Event Date Time Value`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         observation_date=as_date(observation_datetime)) %>% 
  select(pit_id=`Tag Code`,obs_type=`Event Type Name`,
         observation_sitecode=`Event Site Code Value`,
         observation_datetime,observation_date,
         length_mm=`Event Length mm`) %>% 
  group_by(pit_id,observation_date,observation_sitecode) %>% 
  slice(which.min(observation_datetime)) %>% 
  left_join(lifestage.key,by="pit_id")


# will need to join those detections to PTAGIS info, 
# so we can get at rkm to see if those tagged as
# juveniles were ever detected
# downstream, indicating they actually went to the ocean

ptagis.dat <- read_feather("data/ptagis_sites") %>% 
  group_by(site_code) %>% 
  slice(which.max(total_rkm))

# also list out site code ids for interrogation
# sites that indicate the South Fork overall,
# and those that are at the mouth so detections
# are interpreted as entry into South Fork

sf_logical <- c("SC1","SC2","SC3","SC4")

sfentry_logical <- c("SC1","SC2")

# So now split out and deal with juveniles

juv.dat1 <- inseason.int %>% 
  filter(life_stage=="Juvenile",
         obs_type=="Observation") %>% 
  left_join(ptagis.dat,by=c("observation_sitecode"="site_code")) %>% 
  group_by(pit_id,observation_datetime) %>%
  arrange(pit_id,observation_datetime) %>% 
  group_by(pit_id) %>% 
  mutate(last_site=last(observation_sitecode),
         lowest_rkm=min(total_rkm))

# So now filter for those records that are fish that were detected
# at least as far down as lower granite and their last detection was in 
# the south fork

juv.filter <- juv.dat1  %>% 
  filter(lowest_rkm<=695,
         last_site %in% sf_logical)  %>% 
  group_by(pit_id,observation_date,observation_sitecode) %>% 
  slice(which.min(observation_datetime)) %>% 
  select(pit_id,obs_type,observation_sitecode,
         observation_datetime,observation_date) %>% 
  mutate(length_mm=as.numeric(NA),
         mark_stage="Juvenile")  


# pull out the adults from the initial int data
# distinguish their mark stage then bind
# to the filtered juvenile data

int.complete <- inseason.int %>% 
  filter(life_stage=="Adult") %>% 
  rename(mark_stage=life_stage) %>% 
  bind_rows(juv.filter)

# now summarize relevant values

sf_individuals.summary <- int.complete %>% 
  mutate(sf=ifelse(observation_sitecode %in% sf_logical,TRUE,
                   FALSE),
         sf_entry=ifelse(observation_sitecode %in% sfentry_logical,
                         TRUE,FALSE)) %>% 
  group_by(pit_id) %>% 
  summarize(lgr_final=last(observation_datetime[observation_sitecode=="GRA"]),
            sf_first=first(observation_datetime[sf==TRUE]),
            sf_entry_final=last(observation_datetime[sf_entry==TRUE]),
            sf_diff=as.numeric(sf_entry_final-sf_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            mark_stage=first(mark_stage))

sf_entry.summary <- sf_individuals.summary %>% 
  mutate(sf_final_date=as_date(sf_entry_final)) %>% 
  group_by(sf_final_date) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(sy_total=sum(n),
         cumulative_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         spawn_year=2025)


# now also grab sf flow data from USGS gaging station

stites.site <- "13338500"

# temp is coded as 00010, discharge as 00060,
# we'll go for both of those

parm.cd <- c("00010","00060")

# put today's date into text format
# to feed into the query of daily
# water data

today.text <- as.character(today(tz="America/Los_Angeles"))

stites.daily <- readNWISdv(siteNumber=stites.site,
                           parameterCd = parm.cd,
                           startDate="1990-01-01",
                           endDate=today.text) %>% 
  select(date=Date,
         mean_temp=X_00010_00003,
         mean_discharge=X_00060_00003) %>% 
  mutate(date=as_date(date))


stites.dat <- stites.daily %>% 
  filter(date>=min(sf_entry.summary$sf_final_date,na.rm=T),
         date<=today()) %>% 
  mutate(group=1)

write_feather(stites.dat,"data/stites_flow")

write_feather(sf_individuals.summary,
              "data/individuals")

write_feather(sf_entry.summary,
              "data/daily")


# bring in completed year; this was originally constructed 
# for everything available through 2024; I found
# there weren't a lot of these fish tagged until
# spawn year 2018 so this filters for completed
# years from 2018 on

complete_ind <- read_feather("data/completedyrs_individual") %>% 
  filter(spawn_year>=2018,spawn_year<first(sf_entry.summary$spawn_year)) %>% 
  mutate()

complete_daily <- read_feather("data/completedyrs_daily") %>% 
  filter(spawn_year>=2018,spawn_year<first(sf_entry.summary$spawn_year)) %>% 
  ungroup() %>% 
  complete(sf_final_date=seq(as_date("2017-07-01"),as_date("2024-06-30"),
                    by="day")) %>% 
  mutate(obs_year=year(sf_final_date),
         obs_month=month(sf_final_date),
         spawn_year=ifelse(obs_month>6,(obs_year+1),
                           obs_year))%>% 
  group_by(spawn_year) %>% 
  fill(sy_total,.direction = "updown") %>% 
  mutate(n=ifelse(is.na(n),0,n),
         daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         daily_percent=daily_cumulative*100) %>% 
  mutate(day_of_year=yday(sf_final_date),
         dummy_sfentry_date=if_else(day_of_year<182,
                                    as.Date(day_of_year,origin="1977-12-31"),
                                    as.Date(day_of_year,origin="1976-12-31"))) %>% 
  filter(!day_of_year==182) %>% 
  mutate(plot_category="Completed Spawn Years")


# get estimates of how much of the run has been
# completed on a given day of the year for use
# in estimating what the total will be based
# on year-to-date numbers in given spawn year

complete_reference <- complete_daily %>% 
  group_by(dummy_sfentry_date) %>% 
  summarize(median_cum=median(daily_cumulative),
            min_cum=min(daily_cumulative),
            max_cum=max(daily_cumulative),
            min_dailyprop=min(daily_prop),
            median_dailyprop=median(daily_prop),
            max_daily_prop=max(daily_prop))


complete_current <- sf_entry.summary %>% 
  complete(sf_final_date=seq(as_date("2024-07-01"),today(),
                            by="day")) %>% 
  mutate(obs_year=year(sf_final_date),
         obs_month=month(sf_final_date),
         spawn_year=ifelse(obs_month>6,(obs_year+1),
                           obs_year))%>% 
  group_by(spawn_year) %>% 
  mutate(n=ifelse(is.na(n),0,n),
         daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         daily_percent=daily_cumulative*100) %>% 
  mutate(day_of_year=yday(sf_final_date),
         dummy_sfentry_date=if_else(day_of_year<182,
                                    as.Date(day_of_year,origin="1977-12-31"),
                                    as.Date(day_of_year,origin="1976-12-31"))) %>% 
  filter(!day_of_year==182) %>% 
  select(spawn_year,sf_final_date,n,dummy_sfentry_date) %>% 
  mutate(daily_cumulative_n=cumsum(n))


projected_totals <- complete_current %>% 
  slice(which.max(sf_final_date)) %>% 
  left_join(complete_reference,by="dummy_sfentry_date") %>% 
  mutate(max_sy_total=daily_cumulative_n/min_cum,
         median_sy_total=daily_cumulative_n/median_cum,
         min_sy_total=daily_cumulative_n/max_cum) %>% 
  select(spawn_year,min_sy_total,median_sy_total,
         max_sy_total) %>% 
  pivot_longer(min_sy_total:max_sy_total,
               values_to = "sy_total") %>% 
  mutate(projection_category=str_to_title(word(name,1,sep="_")))

# make the projections points that can go on the plot

projected_pts <- projected_totals %>% 
  mutate(sf_final=as_date("2025-05-01"),
         dummy_sfentry_date=as_date("1978-05-01"))

# now what about putting this on a plot comparing to previous years

alldaily <- complete_daily %>% 
  select(spawn_year,sf_final_date,n,dummy_sfentry_date,
         daily_cumulative_n=daily_running_total) %>% 
  bind_rows(complete_current)

# dump out additional parts needed for shiny

write_feather(alldaily,"data/alldaily")
write_feather(projected_pts,"data/projections")


# move this to Shiny app.

## get tooltip for points also

cumplot.all <- alldaily %>% 
  ggplot(aes(x=dummy_sfentry_date,y=daily_cumulative_n,
             group=spawn_year,color=as.factor(spawn_year)))+
  geom_line(aes(text=str_c(" Date:",format(dummy_sfentry_date, "%b %d"),
                           "<br>",
                           "Spawn Year:",spawn_year,
                           "<br>",
                           "Number In:",round(daily_cumulative_n),sep=" ")))+
  geom_point(data=projected_pts,
             aes(x=dummy_sfentry_date,y=sy_total))+
  theme_bw()+
  scale_x_date(date_breaks = "1 month", date_labels="%b")+
  labs(x="Date of entry to South Fork Clearwater",
       y="Percent of Run Completed",
       color="")
cumplot.all
