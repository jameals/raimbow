#check shapefiles
library(sf)
library(tidyverse)

# I downloaded and unzipped the files into a folder called "shapefiles_ca_ramp"
fls <- list.files("C:/Users/Owen.Liu/Downloads/shapefiles_ca_ramp/",full.names = T)
shp_names <- fls[grepl('.shp',fls)]
shp_names <- shp_names[!grepl('.xml',shp_names)]

shp_names

# looks like there's districts and ramp zones

districts <- read_sf(shp_names[1])
zones <- read_sf(shp_names[2])

glimpse(districts)

# Zones is a bunch of lines and looks like it delineates management boundaries
zones
zones %>% ggplot(aes(OBJECTID))+geom_sf()+labs(x="",title="Zones")

# districts contains a lot of extra information I think
# try subsetting to just marine
marine_districts <- districts %>% filter(IsMarine==1)
marine_districts
marine_districts %>% ggplot(aes(col=name,fill=name))+geom_sf()+labs(x="",title="Districts",fill="",col="")
