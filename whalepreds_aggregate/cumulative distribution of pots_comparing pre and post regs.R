#cumulative distribution of simulated pots - comparing pre-reg seasons vs 2018-19 and 2019-20

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
library(ggpubr)


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


#---------------------------------------------------------------------------------------------------------

#bring in logbook data in point form (not yet summarised to grid level)
#point data includes all effort in WA waters
traps_g_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds'))
glimpse(traps_g_all_logs)

logs_all <- traps_g_all_logs %>% 
  #st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m)) %>% 
# dataset has highly negative values (~ -30000) to denote port and bay areas - remove those. 
# Also note that place_traps function (script 1) already removes depths deeper than 200m as crab fishing at deeper depths is not likely
  filter(depth > -1000) %>% 
  mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))
glimpse(logs_all)


#add label for pooled pre-reg seasons, 2018-19 and 2019-20 dfs
logs_all_pre_post_regs <- logs_all %>%
  mutate(pre_post_regs = case_when(
    season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')  ~ 'pre_regulations',
    season == '2018-2019' ~ '2018-2019',
    season == '2019-2020' ~ '2019-2020'
    ))



# Break things down by winter vs spring/summer
spsum <- c("May","June","July","August","September")

pots_by_depth_spsumvswinter_pre_post_regs <- logs_all_pre_post_regs %>%
  mutate(
    win_or_spsum = case_when(
      m %in% spsum ~ "May-Sep",
      TRUE ~ "Dec-Apr"
    )
  ) %>%
  count(pre_post_regs, win_or_spsum,depth) %>% 
  ungroup() %>% 
  rename(pots=n) %>% 
  # do cumulative counts
  mutate(depth=-depth) %>% 
  group_by(pre_post_regs, win_or_spsum) %>% 
  arrange(depth) %>% 
  mutate(cumulative_pots=cumsum(pots),perc_pots=cumulative_pots/last(cumulative_pots)*100) %>% 
  mutate(pre_post_regs = factor(pre_post_regs, levels = c("pre_regulations","2018-2019","2019-2020")))
glimpse(pots_by_depth_spsumvswinter_pre_post_regs)


p1 <- ggplot() +
  geom_line(data=pots_by_depth_spsumvswinter_pre_post_regs, 
            aes(x=depth, y=perc_pots, color=win_or_spsum), 
            size=1)+
  scale_color_manual(values = c("deepskyblue3", "indianred1"))+
  scale_x_continuous(breaks=seq(0, 200, 20),limits=c(0,200))+
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,100))+
  labs(x="Depth (m)",y="Cumulative % of pots") +
  facet_wrap(~ pre_post_regs)+ 
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.90,0.2)) 
p1



#ggsave(here('wdfw','plots',paste0('Cumulative distribution of pots by depth_pre reg vs post reg seasons_win v sprsum_WA waters','.png')),plot_out,w=14,h=10)

path_figures <- "C:/Users/Leena.Riekkola/Projects/NOAA data/maps_ts_whales/figures"
png(paste0(path_figures, "/Cumulative distribution of pots by depth_pre reg vs post reg seasons_win v sprsum_WA waters.png"), width = 14, height = 7, units = "in", res = 300)
ggarrange(p1,
          ncol=1,
          nrow=1,
          #legend="top",
          #labels="auto",
          vjust=8,
          hjust=0
)
invisible(dev.off())



#---------------------------------------------------------
#bar chart of proportions -- need data in point for (simulated pots)
traps_g_all_logs <- read_rds(here::here('wdfw', 'data','traps_g_all_logs_2014_2020_clipped_to_WA_waters_20220126.rds'))
glimpse(traps_g_all_logs)

logs_all <- traps_g_all_logs %>% 
  #st_set_geometry(NULL) %>% 
  mutate(m=month(SetDate),d=day(SetDate),period=ifelse(d<=15,1,2)) %>% 
  mutate(m = month.name[m], period = ifelse(period==1,"first half","second half")) %>% 
  mutate(season = str_sub(SetID,1,9)) %>% 
  mutate(season_month = paste0(season,"_",m)) %>% 
  # dataset has highly negative values (~ -30000) to denote port and bay areas - remove those. 
  # Also note that place_traps function (script 1) already removes depths deeper than 200m as crab fishing at deeper depths is not likely
  filter(depth > -1000) %>% 
  mutate(m = factor(m, levels = c('December','January','February','March','April','May','June','July','August','September','October','November')))
glimpse(logs_all)

#add label for pooled pre-reg seasons, 2018-19 and 2019-20 dfs, and winter vs summer
spsum <- c("May","June","July","August","September")

logs_all_pre_post_regs <- logs_all %>%
  mutate(pre_post_regs = case_when(
    season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')  ~ 'pre_regulations',
    season == '2018-2019' ~ '2018-2019',
    season == '2019-2020' ~ '2019-2020'
  )) %>%
  mutate(pre_post_regs = factor(pre_post_regs, levels = c("pre_regulations","2018-2019","2019-2020"))) %>% 
  mutate(
    win_or_spsum = case_when(
      m %in% spsum ~ "May-Sep",
      TRUE ~ "Dec-Apr"
    ))


pot_depth_dist_bar <- logs_all_pre_post_regs %>% 
  mutate(depth=-depth) %>% 
  mutate(pre_post_regs = factor(pre_post_regs, levels = c("pre_regulations","2018-2019","2019-2020"))) %>% 
  ggplot() + 
  geom_bar(aes(x=depth, y=stat(prop), fill=win_or_spsum), position = "dodge") +
  scale_fill_manual(values = c("deepskyblue3", "indianred1"))+
  scale_x_binned(breaks=seq(0, 200, 25),limits=c(0,200)) + 
  facet_wrap(~ pre_post_regs) +
  #scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Depth (m)",y="Proportion") +
  #ggtitle('WA - spacing between pots (m)') +
  theme_bw()
pot_depth_dist_bar





##K-S test on pot distribution (raw, not cumulative pots)
pre_reg_seasons <- logs_all_pre_post_regs %>%  
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  filter(win_or_spsum == 'May-Sep')
post_reg_2018_2019 <- logs_all_pre_post_regs %>%  filter(season == '2018-2019') %>% 
  filter(win_or_spsum == 'May-Sep')
post_reg_2019_2020 <- logs_all_pre_post_regs %>%  filter(season == '2019-2020') %>% 
  filter(win_or_spsum == 'May-Sep')

post_reg_2018_2019_2020 <- logs_all_pre_post_regs %>%  filter(season %in% c('2018-2019', '2019-2020')) %>% 
  filter(win_or_spsum == 'May-Sep')



kstest1 <- ks.test(pre_reg_seasons$depth, post_reg_2018_2019$depth) #p<0.005
kstest2 <- ks.test(pre_reg_seasons$depth, post_reg_2019_2020$depth) #p<0.005

kstest3 <- ks.test(pre_reg_seasons$depth, post_reg_2018_2019_2020$depth) #p<0.005





library(ggridges)
logs_all_pre_post_regs_MaySep <- logs_all_pre_post_regs %>% 
  filter(win_or_spsum == 'May-Sep') %>% 
  mutate(depth=-depth)
logs_all_pre_post_regs_MaySep$season <- factor(logs_all_pre_post_regs_MaySep$season, levels = c('2019-2020', '2018-2019', '2017-2018', '2016-2017', '2015-2016', '2014-2015', '2013-2014'))

pot_depth_ridges_quantiles <- ggplot(logs_all_pre_post_regs_MaySep, aes(x = depth, y = season, fill = stat(quantile))) +
  stat_density_ridges(quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.25, 0.5, 0.75),
                      rel_min_height = 0.005,
                      scale = 1.5) +
  scale_fill_manual(name = "Quantile", values = c("#E8DED2", "#A3D2CA", "#5EAAA8", "#056676"),
                    labels = c("0-25%", "25-50%","50-75%", "75-100%")) + 
  #xlim(0,72)+
  scale_x_continuous(limits = c(0, 200), expand = c(0, 0))+
  xlab("Pot depth [m] (May-Sep)") +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE)
pot_depth_ridges_quantiles







pre_reg_seasons <- logs_all_pre_post_regs %>%  
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018')) %>% 
  filter(win_or_spsum == 'Dec-Apr')
post_reg_2018_2019 <- logs_all_pre_post_regs %>%  filter(season == '2018-2019') %>% 
  filter(win_or_spsum == 'Dec-Apr')
post_reg_2019_2020 <- logs_all_pre_post_regs %>%  filter(season == '2019-2020') %>% 
  filter(win_or_spsum == 'Dec-Apr')

post_reg_2018_2019_2020 <- logs_all_pre_post_regs %>%  filter(season %in% c('2018-2019', '2019-2020')) %>% 
  filter(win_or_spsum == 'Dec-Apr')

kstest1 <- ks.test(pre_reg_seasons$depth, post_reg_2018_2019$depth) #p<0.005
kstest2 <- ks.test(pre_reg_seasons$depth, post_reg_2019_2020$depth) #p<0.005




library(WRS2)
logs_all_pre_post_regs_groups <- logs_all_pre_post_regs %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "post-reg")
  ) %>% 
  filter(win_or_spsum == 'May-Sep')
#taking too long to run - not working
testx2 <- qcomhd(depth ~ seasons_with_regs, data = logs_all_pre_post_regs_groups, q = c(0.25, 0.5, 0.75, 1), nboot = 200)
testx2



logs_all_pre_post_regs_groups_MaySep <- logs_all_pre_post_regs %>%  
  mutate(seasons_with_regs = 
         ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                "pre-reg",
                "post-reg")
) %>% 
  filter(win_or_spsum == 'May-Sep')

res <- wilcox.test(depth ~ seasons_with_regs, data = logs_all_pre_post_regs_groups_MaySep,
                   exact = FALSE)
res #p< 0.005


post_reg_2018_2019_2020 <- logs_all_pre_post_regs %>%  filter(season %in% c('2018-2019','2019-2020'))  %>% 
  filter(win_or_spsum == 'May-Sep')
res <- wilcox.test(depth ~ season, data = post_reg_2018_2019_2020,
                   exact = FALSE)
res #p< 0.005



pre_regs_vs_2018_2019 <- logs_all_pre_post_regs %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "post-reg")
  ) %>% 
  filter(is_May_Sep == 'Y')

res <- wilcox.test(depth ~ seasons_with_regs, data = pre_regs_vs_2018_2019,
                   exact = FALSE)
res #p< 0.005


