# Considering DCRB vessel response to delayed fishery openings in CA in 2015-16, compared to 2014-15 season

# package website: googlesheets4.tidyverse.org

library(tidyverse)
library(googlesheets4)
library(googledrive)
#devtools::install_github("tidyverse/googlesheets4")
library(janitor)

#sheets_auth()

(DCRB.response <- drive_get("https://docs.google.com/spreadsheets/d/1hEmTRNjeDbJsj8cNE4e-LkExk6EsRTO_skIzI5L28T4/edit#gid=0")) # this is the URL for "2015-16 Closures Vessel Information"
DCRB.response.df <- read_sheet(DCRB.response) %>% clean_names()
head(DCRB.response.df)

# note total fishing vessels refers to All commercial fishing vessels in the 7 d.crab port groups
# note that The drop-out / movement counts are based on vessels classified as active d.crab in 2014-15.

# change to long format

DCRB.response.df.long <- DCRB.response.df %>%
  select(-metric_long) %>%
  pivot_longer(-c(metric, dcrb_year), names_to = "port", values_to = "num_vessels")
glimpse(DCRB.response.df.long)
head(DCRB.response.df.long)
View(DCRB.response.df.long)

# change to wide format

DCRB.response.df.wide <- DCRB.response.df.long %>%
  pivot_wider(names_from = "metric", values_from = "num_vessels") %>% 
  clean_names()
glimpse(DCRB.response.df.wide)
head(DCRB.response.df.wide)
View(DCRB.response.df.wide)

# make some summaries
names(DCRB.response.df.wide)
DCRB.response.df.summary <- DCRB.response.df.wide %>%
  group_by(port) %>%
  #group_by(dcrb_year, port) %>%
  mutate(
    percent.lg.dcrb.vessels = lg_dcrab_vessels/total_dcrab_vessels,
    percent.sm.dcrb.vessels = sm_dcrab_vessels/total_dcrab_vessels,
    
    num.dcrb.vessels.dropout.during.all = lg_dcrab_vessels_dropped_out_during_closures + sm_dcrab_vessels_dropped_out_during_closures,
    num.dcrb.vessels.dropout.after.all = lg_dcrab_vessels_dropped_out_after_closures + sm_dcrab_vessels_dropped_out_after_closures,
    num.dcrb.vessels.moved.during.all = lg_dcrab_vessels_moved_during_closures + sm_dcrab_vessels_moved_during_closures,
    
    percent.dcrb.vessels.droput.during.all = 100* ( num.dcrb.vessels.dropout.during.all/total_dcrab_vessels[dcrb_year=="2014-2015"] ),
    percent.Lg.dcrb.vessels.droput.during.all = 100* (lg_dcrab_vessels_dropped_out_during_closures/lg_dcrab_vessels[dcrb_year=="2014-2015"] ),
    percent.Sm.dcrb.vessels.droput.during.all = 100* ( sm_dcrab_vessels_dropped_out_during_closures/sm_dcrab_vessels[dcrb_year=="2014-2015"] ),
    
    percent.dcrb.vessels.droput.after.all = 100* ( num.dcrb.vessels.dropout.after.all/total_dcrab_vessels[dcrb_year=="2014-2015"] ),
    percent.Lg.dcrb.vessels.droput.after.all = 100* ( lg_dcrab_vessels_dropped_out_after_closures/lg_dcrab_vessels[dcrb_year=="2014-2015"] ),
    percent.Sm.dcrb.vessels.droput.after.all = 100* ( sm_dcrab_vessels_dropped_out_after_closures/sm_dcrab_vessels[dcrb_year=="2014-2015"] ),
    
    percent.dcrb.vessels.moved.during.all = 100* ( num.dcrb.vessels.moved.during.all/total_dcrab_vessels[dcrb_year=="2014-2015"] ),
    percent.Lg.dcrb.vessels.moved.during.all = 100* ( lg_dcrab_vessels_moved_during_closures/lg_dcrab_vessels[dcrb_year=="2014-2015"] ),
    percent.Sm.dcrb.vessels.moved.during.all = 100* ( sm_dcrab_vessels_moved_during_closures/sm_dcrab_vessels[dcrb_year=="2014-2015"] )
    
  )

# determine range of data to check that % columns are [0,1]
apply(DCRB.response.df.summary, MARGIN = 2, function(x) range(x, na.rm=TRUE))

glimpse(DCRB.response.df.summary)
View(DCRB.response.df.summary)

# make plots showing % of vessels active in  2014-15 that drop out during closure in 2015-16 coastwide: all, lg, sm

# first, make a df
df.during.allports <- data.frame(
  c("All","Large", "Small"),
  c(subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.dcrb.vessels.droput.during.all,
    subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.Lg.dcrb.vessels.droput.during.all,
    subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.Sm.dcrb.vessels.droput.during.all)
)
colnames(df.during.allports) <- c("vessel_size","percent.droput.during")

ggplot(data = df.during.allports) +
  geom_bar(stat="identity",aes(x= vessel_size,
                               y= percent.droput.during)
           ) +
  theme_classic() 
ggsave("~/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/Figures/Delayed opening impacts 2015-16/Percent DCRB vessel dropouts - during - coastwide.pdf")

# make plots showing % of vessels active in  2014-15 that drop out after closure in 2015-16 coastwide: all, lg, sm

# first, make a df
df.after.allports <- data.frame(
  c("All","Large", "Small"),
  c(subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.dcrb.vessels.droput.after.all,
    subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.Lg.dcrb.vessels.droput.after.all,
    subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.Sm.dcrb.vessels.droput.after.all)
)
colnames(df.after.allports) <- c("vessel_size","percent.droput.after")

ggplot(data = df.after.allports) +
  geom_bar(stat="identity",aes(x= vessel_size,
                               y= percent.droput.after)
  ) +
  theme_classic() 
ggsave("~/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/Figures/Delayed opening impacts 2015-16/Percent DCRB vessel dropouts - after - coastwide.pdf")

# make plots showing % of vessels active in  2014-15 that stopped fishing in a port they fished in 2014-15 but fished at another port during the closure in 2015-16 coastwide: all, lg, sm

# first, make a df
df.moved.during.allports <- data.frame(
  c("All","Large", "Small"),
  c(subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.dcrb.vessels.moved.during.all,
    subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.Lg.dcrb.vessels.moved.during.all,
    subset(DCRB.response.df.summary,c(port=="all_ports" & dcrb_year == "2015-2016"))$percent.Sm.dcrb.vessels.moved.during.all)
)
colnames(df.moved.during.allports) <- c("vessel_size","percent.moved.during")

ggplot(data = df.moved.during.allports) +
  geom_bar(stat="identity",aes(x= vessel_size,
                               y= percent.moved.during)
  ) +
  theme_classic() 
ggsave("~/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/Figures/Delayed opening impacts 2015-16/Percent DCRB vessel dropouts - moved during - coastwide.pdf")



### did not do this yet ###


# make plots showing % of vessels that drop out port by port: all, lg, sm

# first, make a df
df.during.eachport <- data.frame(
  rep(c("All","Large", "Small"),length(unique(DCRB.response.df.summary$port))),
  c(subset(DCRB.response.df.summary,c(dcrb_year == "2015-2016"))$percent.dcrb.vessels.droput.during.all,
    subset(DCRB.response.df.summary,c(dcrb_year == "2015-2016"))$percent.Lg.dcrb.vessels.droput.during.all,
    subset(DCRB.response.df.summary,c(dcrb_year == "2015-2016"))$percent.Sm.dcrb.vessels.droput.during.all)
)
colnames(df.during.eachport) <- c("vessel_size","percent.droput.during")

ggplot(data = df.during.eachport) +
  geom_bar(stat="identity",aes(x= vessel_size,
                               y= percent.droput.during)
  ) +
  theme_classic() 
ggsave("~/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/Figures/Delayed opening impacts 2015-16/Percent DCRB vessel dropouts - during - coastwide.pdf")




#old school read in
#DCRB.response.df <- read.csv("/Users/jameal.samhouri/Dropbox/Projects/In progress/RAIMBOW/Samhouri et al. whales risk/Output_Data/2015-16 Closures Vessel Information.csv")

