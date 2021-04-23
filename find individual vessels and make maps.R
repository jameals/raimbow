# 022820

#library(foreign)
library(lubridate)
library(tidyverse)
# library(reshape2)
# library(scales)
# library(zoo)
# library(ggrepel)
# library(sf)
library(data.table)
# library(wesanderson)
library(viridis)
# library(here)
#library(ggerr)

# load RDS
dcrb_vms_tix_analysis <- readRDS("~/Documents/RAIMBOW/Processed Data/VMS/vms_all_interpolated_w_grd.RDS")
glimpse(dcrb_vms_tix_analysis)

# looking for brett's boat
which(dcrb_vms_tix_analysis$drvid == "298475")
glimpse(dcrb_vms_tix_analysis[which(dcrb_vms_tix_analysis$drvid == "298475"),])
View(dcrb_vms_tix_analysis[which(dcrb_vms_tix_analysis$drvid == "298475"),])
dcrb_vms_tix_analysis[which(dcrb_vms_tix_analysis$drvid == "298475"),'VESSEL_NAM']




unique(dcrb_vms_tix_analysis[which(dcrb_vms_tix_analysis$drvid == "298475"),'port_group_code'])