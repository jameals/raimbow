library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(raster)
library(fasterize)
select <- dplyr::select
library(rnaturalearth)
library(viridis)
library(magrittr)
library(gridExtra)
library(nngeo)
library(scales)


# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.text.x.bottom = element_text(angle=45),
        legend.position = c(0.8,0.3),
        title=element_text(size=12),
        legend.title = element_text(size=10),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)
options(dplyr.summarise.inform = FALSE)

#########################################


#Self reported logbook data not always very accurate
#There are stringlines that have a length of 0m (start and end loc are exactly the same)
#as well as stringlines that are several kilometers long


traps_g <- read_rds(here::here('wdfw', 'data','traps_g_license_all_logs_2013_2019_LineLengthIncluded.rds'))

# remove geometry, create columns for season, month etc 
traps_g %<>%
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name),
    month_interval = paste0(month_name, 
                            "_", 
                            ifelse(day(SetDate)<=15,1,2)
    ),
    season_month_interval = paste0(season, 
                                   "_", 
                                   month_interval)
  )

# Read in and join license & pot limit info
WA_pot_limit_info <- read_csv(here::here('wdfw', 'data','WA_pot_limit_info_May2021.csv'))

WA_pot_limit_info %<>%
  rename(License = License_ID)

# join Pot_Limit to traps_g 
traps_g %<>%
  left_join(WA_pot_limit_info,by=c("License")) %>% 
  drop_na(Pot_Limit) #2 NAs for cases with no license info unless correct it with drop_na(Pot_Limit)


#---------------------------------------------------
#Investigate the relationship between stringline length and the reported no. of pots

# In the df each row is an individual simulated pot - remove duplicated rows based on SetID
traps_g_v2 <-  traps_g %>% distinct(SetID, .keep_all = TRUE)

p1 <- ggplot(traps_g_v2, aes(x=line_length_m, y=PotsFished))+ 
  geom_point() + 
  facet_wrap(~ season) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p1 


#some of the really high line length values are messing things up, so remove those
traps_g_v3 <-  traps_g_v2 %>% 
  filter(line_length_m < 1e+05)

p2 <- ggplot(traps_g_v3, aes(x=line_length_m, y=PotsFished))+ 
  geom_point() + 
  facet_wrap(~ season) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p2 

p3 <- ggplot(traps_g_v2, aes(x=line_length_m/1000))+ 
  geom_histogram(binwidth=1) + 
  #scale_x_continuous(breaks=seq(0, 100, 10),limits=c(0,100))+
  #facet_wrap(~ season) +
  labs(x="Stringline length (km)",y="No. of Stringlines") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p3 

p3b <- ggplot(traps_g_v3, aes(x=line_length_m/1000))+ 
  geom_histogram(binwidth=1) + 
  facet_wrap(~ season) +
  labs(x="Stringline length (km)",y="No. of Stringlines") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p3b

p3c <- traps_g_v3 %>% 
  ggplot() + #aes(color=wintersummer, fill=wintersummer)
  geom_bar(aes(x=line_length_m/1000, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 100, 5)) + #you can specify x-axis break here, e.g.: breaks=seq(0, 125, 5)
  scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Stringline length (km)",y="Proportion") +
  ggtitle('Proportion of string lengths')
p3c

p3d <- traps_g_v3 %>% 
  mutate(Pot_Limit = factor(Pot_Limit, levels = c('300','500'))) %>% 
  ggplot() + #aes(color=wintersummer, fill=wintersummer)
  geom_bar(aes(x=line_length_m/1000, y=stat(prop)), position = "dodge") +
  facet_wrap(~ Pot_Limit) +
  scale_x_binned(breaks=seq(0, 100, 5)) + #you can specify x-axis break here, e.g.: breaks=seq(0, 125, 5)
  scale_y_continuous(breaks=seq(0, 0.7, 0.05),limits=c(0,0.7))+
  labs(x="Stringline length (km)",y="Proportion") +
  ggtitle('Proportion of string lengths')
p3d
#-------------------------------------------------
#proportion of stringlines that are 0m per season?
traps_g_v4 <-  traps_g_v2 %>% 
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  #group_by(season, month_name) %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_0m_length = length(line_length_m[line_length_m<0.1])) %>% 
  mutate(percent_0m_length = (n_0m_length/n_records)*100)
#Across all seasons, the proportion of stringlines that are 0m in length (i.e. begin and end locs are exactly the same) is 1% or less
#2017-2018 season had the smallest proportion of stringlines that were 0m (better logbook reporting)
#2018-2019 season had the highest proportion of stringlines that were 0m (poorer logbook reporting)

p4 <- traps_g_v4 %>% 
  ggplot(aes(x=month_name,y=prop_0m_length, colour = season, group=season))+
  geom_line(size=1)+
  scale_colour_brewer(palette = "PRGn") +
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  scale_y_continuous(breaks=seq(0, 0.04, 0.01),limits=c(0,0.05))+
  labs(x="Month",y="Proportion of strignlines that are 0m") +
  ggtitle("Proportion of 0m stringlines,\nall years by season and month") + 
  theme(legend.position = ("top"),legend.title=element_blank())
p4


#proportion/percent of stringlines that are 0m per season and by pot tier?
traps_g_v5 <-  traps_g_v2 %>% 
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  mutate(Pot_Limit = factor(Pot_Limit, levels = c('300','500'))) %>% 
  #group_by(season, Pot_Limit, month_name) %>% 
  group_by(season, Pot_Limit) %>% 
  summarise(n_records = n(),
            n_0m_length = length(line_length_m[line_length_m<0.1])) %>% 
  mutate(percent_0m_length = (n_0m_length/n_records)*100)
#Overall 500 pot tier vessels had a smaller proportion of 0m stringlines than 300 pot tier 
#vessels, i.e., 500 pot tier vessels had better/more accurate logbook reporting 
p5 <- traps_g_v5 %>% 
  ggplot(aes(x=month_name,y=prop_0m_length, colour = Pot_Limit, group=Pot_Limit))+
  geom_line(size=1)+
  facet_wrap(~ season) +
  #scale_colour_brewer(palette = "PRGn") +
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  #scale_y_continuous(breaks=seq(0, 0.04, 0.01),limits=c(0,0.05))+
  labs(x="Month",y="Proportion of strignlines that are 0m") +
  ggtitle("Proportion of 0m stringlines,\nall years by season") + 
  theme(legend.position = ("top"),legend.title=element_blank())
p5



# Cumulative distribution of stringline length - by season 
length_by_season <- traps_g_v3 %>%
  mutate(line_length_m = (round(line_length_m, digits = 1))) %>% 
  count(season, line_length_m) %>% 
  ungroup() %>% 
  # do cumulative counts
  group_by(season) %>%
  arrange(line_length_m) %>% 
  mutate(cumulative_lengths=cumsum(n),perc_lengths=cumulative_lengths/last(cumulative_lengths)*100)
glimpse(length_by_season)

line_length_dist_by_season <- length_by_season %>% 
  ggplot(aes(x=line_length_m/1000,y=perc_lengths, colour = season, group=season))+
  geom_line(size=1)+
  scale_colour_brewer(palette = "PRGn") +
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  scale_x_continuous(breaks=seq(0, 100, 10),limits=c(0,100))+
  labs(x="Stringline length (km)",y="Cumulative % Stringlines") +
  ggtitle("Distribution of DCRB strinlines by length,\nall years by season") + 
  theme(legend.position = ("top"),legend.title=element_blank())
line_length_dist_by_season



# Cumulative distribution of stringline length - by season and pot tier
length_by_season_and_pot_tier <- traps_g_v3 %>%
  mutate(Pot_Limit = factor(Pot_Limit, levels = c('300','500'))) %>% 
  mutate(line_length_m = (round(line_length_m, digits = 1))) %>% 
  count(season, Pot_Limit, line_length_m) %>% 
  ungroup() %>% 
  # do cumulative counts
  group_by(season, Pot_Limit) %>%
  arrange(line_length_m) %>% 
  mutate(cumulative_lengths=cumsum(n),perc_lengths=cumulative_lengths/last(cumulative_lengths)*100)
glimpse(length_by_season_and_pot_tier)

line_length_dist_by_season_and_pot_tier <- length_by_season_and_pot_tier %>% 
  ggplot(aes(x=line_length_m/1000,y=perc_lengths, colour = Pot_Limit, group=Pot_Limit))+
  geom_line(size=1)+
  facet_wrap(~ season) +
  #scale_colour_brewer(palette = "PRGn") +
  #geom_hline(aes(yintercept = 90), colour="blue", linetype=2)+
  scale_x_continuous(breaks=seq(0, 100, 10),limits=c(0,100))+
  labs(x="Stringline length (km)",y="Cumulative % Stringlines") +
  ggtitle("Distribution of DCRB strinlines by length,\nall years by season and by pot tier") + 
  theme(legend.position = ("top"),legend.title=element_blank())
line_length_dist_by_season_and_pot_tier



#--------------------------
#focus on lines that are 0m - there are both 0 and 0.000 cases. After that the next value is 12m
# how many pots fished on those stringlines that were 0m?
traps_g_v6 <-  traps_g_v3 %>% 
  filter(line_length_m < 0.1)
p6 <- ggplot(traps_g_v6, aes(x=PotsFished))+ 
  geom_histogram(binwidth=5) + 
  #facet_wrap(~ season) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p6 

traps_g_v6_summary <- traps_g_v6 %>% 
  group_by(season, Pot_Limit) %>% 
  summarise(n_records = n(),
            n_0m_length = length(line_length_m[line_length_m<0.1]),
            nvessels=n_distinct(Vessel,na.rm=T)) 


# as a comparison, how many pots fished on those stringlines that had a length <0m?
traps_g_v6b <-  traps_g_v3 %>% 
  filter(line_length_m > 0.1) %>% 
  filter(PotsFished < 250)
p6b <- ggplot(traps_g_v6b, aes(x=PotsFished))+ 
  geom_histogram(binwidth=5) + 
  #facet_wrap(~ season) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p6b 


#------------------------------------------------------
# the 'too long' stringlines
# what would be the cutoff if exclude top 5%/2.5% - for each pot tier

#first remove 0m lines

#traps_g_v2 = where didn't drop the very long ones that were messing up plots
traps_300_tier <- traps_g_v2 %>% 
  filter(Pot_Limit == 300) %>% 
  filter(line_length_m > 0)
  

p7 <- ggplot(traps_300_tier, aes(x=line_length_m/1000))+ 
  geom_histogram(binwidth=1, aes(fill=season)) + 
  #facet_wrap(~ season) +
  labs(x="Stringline length (km)",y="No. of Stringlines") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p7

traps_500_tier <- traps_g_v2 %>% 
  filter(Pot_Limit == 500)%>% 
  filter(line_length_m > 0)

p8 <- ggplot(traps_500_tier, aes(x=line_length_m/1000))+ 
  geom_histogram(binwidth=1, aes(fill=season)) + 
  #facet_wrap(~ season) +
  labs(x="Stringline length (km)",y="No. of Stringlines") +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
p8




#Calculate percentiles # 5% cut-off = 11448.16, 2.5% cut-off = 13212.62
traps_300_tier_quants <-  quantile(traps_300_tier$line_length_m, probs = c(0.975)) 
#Subset according to percentiles
traps_300_tier_exc_5percent <- traps_300_tier %>% 
  filter(line_length_m < traps_300_tier_quants)
  
#Calculate percentiles # 5% cut-off = 18885.67, 2.5% cut-off = 22529.54
traps_500_tier_quants <-  quantile(traps_500_tier$line_length_m, probs = c(0.95)) 
#Subset according to the two percentiles
traps_500_tier_exc_5percent <- traps_500_tier %>% 
  filter(line_length_m < traps_500_tier_quants)



p9 <- rbind(traps_300_tier_exc_5percent,traps_500_tier_exc_5percent) %>% 
  mutate(Pot_Limit = factor(Pot_Limit, levels = c('300','500'))) %>% 
  ggplot() + 
  geom_bar(aes(x=line_length_m/1000, y=stat(prop)), position = "dodge") +
  facet_wrap(~ Pot_Limit) +
  scale_x_binned(breaks=seq(0, 25, 1)) + #you can specify x-axis break here, e.g.: breaks=seq(0, 125, 5)
  scale_y_continuous(breaks=seq(0, 0.2, 0.05),limits=c(0,0.2))+
  labs(x="Stringline length (km)",y="Proportion") +
  ggtitle('Proportion of string lengths (2.5% cut-off)')
p9





traps_300_tier_quants_season <- traps_300_tier %>% 
  #mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>% 
  group_by(season) %>% #, month_name
  summarise(quants_5percent = quantile(line_length_m, probs = c(0.95)),
            quants_2.5percent = quantile(line_length_m, probs = c(0.975))
            ) 

traps_500_tier_quants_season <- traps_500_tier %>% 
  group_by(season) %>% 
  summarise(quants_5percent = quantile(line_length_m, probs = c(0.95)),
            quants_2.5percent = quantile(line_length_m, probs = c(0.975))
  ) 

#----------------------------------
#% lines lost with different cut-off values

percent_lost_300_tier <-  traps_300_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > traps_300_tier_quants])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)

percent_lost_500_tier <-  traps_500_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > traps_500_tier_quants])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)


#set 20km cut off
percent_lost_300_tier_set_cutoff <-  traps_300_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > 20000])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)

percent_lost_500_tier_set_cutoff <-  traps_500_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > 25000])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)



#what happens if you consider the % of traps excluded with the 2.5% and 5% cutoffs 
#for the % of sets, or the 20km or 25km cutoff?

#so work on pots, not stringlines
# i.e., traps_g instead of traps_g_v2

traps_300_tier <- traps_g %>% 
  filter(Pot_Limit == 300) %>% 
  filter(line_length_m > 0)

#Calculate percentiles for 300 tier # 5% cut-off = 11448.16, 2.5% cut-off = 13212.62
percent_lost_300_tier <-  traps_300_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > 13212.62])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)


traps_500_tier <- traps_g %>% 
  filter(Pot_Limit == 500) %>% 
  filter(line_length_m > 0)

#Calculate percentiles for 500 tier # 5% cut-off = 18885.67, 2.5% cut-off = 22529.54
percent_lost_500_tier <-  traps_500_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > 22529.54])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)


#set 20km/25km cut off
percent_lost_300_tier_set_cutoff <-  traps_300_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > 20000])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)

percent_lost_500_tier_set_cutoff <-  traps_500_tier %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long = length(line_length_m[line_length_m > 30000])) %>% 
  mutate(percent_too_long = (n_too_long/n_records)*100)


#quantiles for individual season, but use the df where removed duplicates
traps_300_tier_quants_season <-  traps_g_v2 %>% 
  filter(Pot_Limit == 300)%>% 
  filter(line_length_m > 0) %>% 
  group_by(season) %>% 
  summarise(quant_05percent = quantile(line_length_m, probs = c(0.95)), 
            quant_025percent = quantile(line_length_m, probs = c(0.975))
              )
  
traps_300_tier_quant_joined <- traps_300_tier %>% 
  left_join(traps_300_tier_quants_season, by=("season"))

percent_lost_300_tier_quant_joined <-  traps_300_tier_quant_joined %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long_05percent = length(line_length_m[line_length_m > quant_05percent]),
            n_too_long_025percent = length(line_length_m[line_length_m > quant_025percent])
            ) %>% 
  mutate(percent_too_long_05percent = (n_too_long_05percent/n_records)*100,
         percent_too_long_025percent = (n_too_long_025percent/n_records)*100
         )


#quantiles for individual season, but use the df where removed duplicates
traps_500_tier_quants_season <-  traps_g_v2 %>% 
  filter(Pot_Limit == 500)%>% 
  filter(line_length_m > 0) %>% 
  group_by(season) %>% 
  summarise(quant_05percent = quantile(line_length_m, probs = c(0.95)), 
            quant_025percent = quantile(line_length_m, probs = c(0.975))
  )

traps_500_tier_quant_joined <- traps_500_tier %>% 
  left_join(traps_500_tier_quants_season, by=("season"))

percent_lost_500_tier_quant_joined <-  traps_500_tier_quant_joined %>% 
  group_by(season) %>% 
  summarise(n_records = n(),
            n_too_long_05percent = length(line_length_m[line_length_m > quant_05percent]),
            n_too_long_025percent = length(line_length_m[line_length_m > quant_025percent])
  ) %>% 
  mutate(percent_too_long_05percent = (n_too_long_05percent/n_records)*100,
         percent_too_long_025percent = (n_too_long_025percent/n_records)*100
  )

#-----------------------------------------------
#list of SetIDs that have string length 0m
unique_SetIDs_0m <- traps_g_v2 %>% 
  filter(line_length_m < 0.1) %>% 
  distinct(SetID)

logs_stackcoords_2009_2019 <- read_csv(here('wdfw', 'data','WDFW-Dcrab-logbooks-compiled_stackcoords_2009-2019.csv'),col_types = 'ccdcdccTcccccdTddddddddddddddddiddccddddcddc')

logs_stackcoords_2009_2019_0m <- filter(logs_stackcoords_2009_2019, SetID %in% unique_SetIDs_0m)
