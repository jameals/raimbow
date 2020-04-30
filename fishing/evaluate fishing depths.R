library(lubridate)
library(tidyverse)
library(data.table)

# load RDS with fish ticket informed VMS data, matched to vessel lengths
dcrb_landings_length_all_years <- read_rds("~/Documents/RAIMBOW/Processed Data/VMS/vms_all_interpolated_w_grd_vlengths.RDS")

dcrb_vms_tix_analysis <- dcrb_landings_length_all_years

#####################################################
# filter the data using the control variables below
# years <- seq(2009,2019,1)

state_agency_code <- "C"
#state <- "CA"
target_rev <- "DCRB"
target_lbs <- "DCRB"
region_north <- c(1040, 1041, 1042)
vessel_size_category_break <- 40
winter_months <- c("November", "December", "January", "February", "March")
crab_months <- c(1:7,11:12)
removal_types <- c("COMMERCIAL (NON-EFP)", "COMMERCIAL(DIRECT SALES)", "UNKNOWN")
max_speed <- 4.11556
min_speed <- 0

#ports <- c("CCA", "ERA", "BGA","BDA", "SFA", "MRA", "MNA")
#region_south <- c(1035, 1036, 1037, 1038)
#####################################################

#####################################################

# subset the data based on above queries. CA only. keep all depths <1200m
dcrb_ca_vms_tix_analysis <- dcrb_vms_tix_analysis %>%
  filter(agency_code == state_agency_code) %>%
  #filter(agency_code == state_agency_code & STATE == state) %>%
  filter(TARGET_rev == target_rev | TARGET_lbs == target_lbs) %>%
  filter(removal_type_name %in% removal_types) %>%
  filter(CA_OFFSHOR != -999) %>%
  #filter(DEPTH_CATM == "0-100m" | DEPTH_CATM == "100-150m") %>%
  filter(DEPTH_CATM != ">1200m") %>%
  filter(avg_speed_recalc <= max_speed & avg_speed_recalc >= min_speed) %>%
  #filter(is.na(in_port) == TRUE) %>% # only removes ~4000 records
  #filter(port_group_code %in% ports) %>%
  mutate(#GRID5KM_ID = as.character(GRID5KM_ID),
    Region = ifelse(CA_OFFSHOR %in% region_north,
                    "NorCA","CenCA"),
    BIA_mn_noNAs = ifelse(is.na(BIA_mn)==TRUE,0,BIA_mn),
    BIA_bm_noNAs = ifelse(is.na(BIA_bm)==TRUE,0,BIA_bm),
    #year = lubridate::year(westcoastdate_notime), # removed because it is included with the vessel_lengths step
    year_month = paste0(lubridate::year(westcoastdate_notime),"_", substr(lubridate::ymd(westcoastdate_notime),6,7)), # substr() ensures month is a 2 digit value
    month = lubridate::month(westcoastdate_notime, label=TRUE, abbr = FALSE),
    month_as_numeric = month(westcoastdate_notime),
    week_of_year = week(westcoastdate_notime),
    day_of_year = yday(westcoastdate_notime),
    season = as.character(ifelse(month %in% winter_months, "Winter", "Spring-Summer")),
    crab_year = ifelse(
      month_as_numeric >= 11, paste0(year,"_",1+year), paste0(year - 1,"_",year)
    ),
    Vessel_Size = as.character(ifelse(FINAL_LENGTH >= vessel_size_category_break, paste0(">=",vessel_size_category_break, " ft"),paste0("<",vessel_size_category_break, " ft"))),
    BIA_bm_or_mn = ifelse(BIA_bm_noNAs !=0 | BIA_mn_noNAs != 0, "Inside BIA","Outside BIA")#,
  ) %>%
  filter(westcoastdate_notime >= as.Date("2009-11-01") & westcoastdate_notime <= as.Date("2019-08-01")) %>%
  filter(month_as_numeric %in% crab_months) %>%
  dplyr::select(
    Rec_ID, VMS_RECNO, drvid, Vessel_Size,
    westcoastdate_notime, year, crab_year, year_month, month, month_as_numeric, week_of_year, day_of_year, season,
    GRID5KM_ID, BAND_25KM, BAND_50KM, CA_OFFSHOR, Region, BIA_mn_noNAs, BIA_bm_noNAs, BIA_bm_or_mn, pacfin_port_code, port_group_code, DEPTH_CATM, NGDC_M,
    TARGET_lbs, TARGET_rev, DCRB_lbs, DCRB_revenue
  )

glimpse(dcrb_ca_vms_tix_analysis)
unique(dcrb_ca_vms_tix_analysis$DEPTH_CATM)
range(dcrb_ca_vms_tix_analysis$NGDC_M/100)
View(with(dcrb_ca_vms_tix_analysis, table(NGDC_M, DEPTH_CATM)))
length(which(dcrb_ca_vms_tix_analysis$NGDC_M >0))
length(which(dcrb_ca_vms_tix_analysis$NGDC_M < -12000))

#####################################################

# make some histograms by depth

tmp <- dcrb_ca_vms_tix_analysis %>% 
  filter(NGDC_M <= 0 & NGDC_M >= -12000) 
dim(tmp)
dim(dcrb_ca_vms_tix_analysis)
length(which(tmp$NGDC_M >0))
length(which(tmp$NGDC_M <= -12000))
range(tmp$NGDC_M)
unique(tmp$DEPTH_CATM)

tmp %>% 
  ggplot(aes(NGDC_M/10)) + geom_boxplot()

tmp %>% 
  ggplot(aes(x=1,y=NGDC_M/10)) + geom_violin()

probs <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
quantiles <- quantile(tmp$NGDC_M/10, prob=probs)

tmp %>% 
  ggplot(aes(NGDC_M/10, stat(density)))+
  geom_histogram(fill='#33638DFF', alpha=0.8, binwidth=5)+
  geom_vline(aes(xintercept=quantiles[1]),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantiles[3]),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantiles[4]),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantiles[5]),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantiles[7]),
             color="red", linetype="dashed", size=1) +
  labs(x='Depth (m)')

probs <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
quantiles <- quantile(tmp$NGDC_M/10, prob=probs)

dt <- data.table(x=c(1:200),y=rnorm(200))
dens <- density(dt$y)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
quantiles <- quantile(dt$y, prob=probs)
tmp$quant <- factor(findInterval(tmp$NGDC_M,quantiles))
ggplot(tmp, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")
