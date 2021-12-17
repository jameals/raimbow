#WA plot revenue and landings from PacFin

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
library(cowplot)
library(ggpubr)
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


#------------------------------------------------------------------------------

pacfin_data <- read_csv(here::here('wdfw', 'data','WA_DCRAB_landing_and_revenue.csv'))


pacfin_data_MaySep <-  pacfin_data %>%  
  mutate(is_May_Sep = 
         ifelse(month %in% c('May', 'June', 'July', 'August', 'September')
                ,'Y', 'N')) %>% 
  filter(is_May_Sep == "Y") %>% 
  mutate(month = factor(month, levels = c('May','June','July','August','September','October','November'))) %>% 
  mutate(season = factor(season, levels = c('2013-2014', '2014-2015', '2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020')))  
  
  

MaySep_rev_ts <- ggplot(pacfin_data_MaySep, aes(x=month, y=REVENUE, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Revenue") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
MaySep_rev_ts


MaySep_landings_ts <- ggplot(pacfin_data_MaySep, aes(x=month, y=WEIGHT_MTONS, colour=season, group=season))+
  geom_line(size=1.5, lineend = "round") + 
  scale_colour_brewer(palette = "PRGn") +
  ylab("Ladings weight (MTONS)") +
  xlab("Month") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
MaySep_landings_ts




summary_pacfin_data_MaySep <- pacfin_data_MaySep %>% 
  group_by(season) %>% 
  summarise(sum_revenue = sum(REVENUE),
            sum_weight_mtons = sum(WEIGHT_MTONS)
            )


sum_MaySep_rev_ts <- ggplot(summary_pacfin_data_MaySep, aes(x=season, y=sum_revenue, group=1))+
  geom_line(size=1, lineend = "round") + 
  geom_point(size=2.5) + 
  ylab("Revenue (sum May-Sep)") +
  xlab("Season") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
sum_MaySep_rev_ts


sum_MaySep_landings_ts <- ggplot(summary_pacfin_data_MaySep, aes(x=season, y=sum_weight_mtons, group=1))+
  geom_line(size=1, lineend = "round") + 
  geom_point(size=2.5) + 
  ylab("Landings weight (sum May-Sep) [mtons]") +
  xlab("Season") + 
  guides(color = guide_legend(override.aes = list(size = 2))) + #this will make legend for the years look better
  theme(legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position="bottom"
  )
sum_MaySep_landings_ts
