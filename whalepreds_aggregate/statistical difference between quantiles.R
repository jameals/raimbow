#-----------------------------------------------------------------------------------
#statistically comparing quantiles/distribution 


library(tidyverse)
library(here)
library(lubridate)
library(rnaturalearth) #devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(rgeos)
library(viridis)
library(ggpubr)
library(scales)
library(ggridges)

library(WRS2)
library(quantreg)
#-----------------------------------------------------------------------------------

# bring in gridded WA logbook data (clipped to WA waters)
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_2wk_step.rds"
#path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/adj_summtraps_2014_2020_all_logs_WA_waters_1mon_step.rds"

x.fish_WA <- readRDS(path.fish_WA) %>% 
  mutate(is_May_Sep = 
           ifelse(month_name %in% c('May', 'June', 'July', 'August', 'September')
                  ,'Y', 'N')) %>%
  #Grid ID 122919 end up having very high trap densities in few months 
  #(e.g., 244pots/km2 in May 2013-2014 season, also high in July 2013-2014
  #this is because the grid is split across land, and few points happen to fall in a very tiny area
  #remove it
  filter(GRID5KM_ID != 122919)

#the above data is filtered to be only effort that is in WA waters
#but may have been landed in either WA or OR

x.fish_WA_MaySep <-  x.fish_WA %>% 
  filter(is_May_Sep == "Y") 
x.fish_WA_MaySep$season <- factor(x.fish_WA_MaySep$season, levels = c('2013-2014', '2014-2015', '2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020'))




#Jul-Sep pre-reg vs 2018-2019
x.fish_WA_JulSep <- x.fish_WA %>% 
  mutate(month_name = as.character(month_name)) %>% 
  filter(month_name %in% c('July', 'August', 'September')) %>% 
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019')) %>% 
  mutate(pre_post_reg = 
           ifelse(season == '2018-2019', "2018-2019", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))
x.fish_WA_JulSep_pre_reg <- x.fish_WA_JulSep %>% 
  filter(pre_post_reg == "pre-reg")
x.fish_WA_JulSep_2018_2019 <- x.fish_WA_JulSep %>% 
  filter(pre_post_reg == "2018-2019")

#May-Sep pre-reg vs 2019-2020
x.fish_WA_MaySep <- x.fish_WA %>% 
  mutate(month_name = as.character(month_name)) %>% 
  filter(is_May_Sep == "Y") %>%  
  #take out 2018-2019 season
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2019-2020')) %>%  
  mutate(pre_post_reg = 
           ifelse(season == '2019-2020', "2019-2020", "pre-reg")) %>% 
  mutate(pre_post_reg = as.factor(pre_post_reg))
x.fish_WA_MaySep_pre_reg <- x.fish_WA_MaySep %>% 
  filter(pre_post_reg == "pre-reg")
x.fish_WA_MaySep_2019_2020 <- x.fish_WA_MaySep %>% 
  filter(pre_post_reg == "2019-2020")


# kstest_JulSep <- ks.test(x.fish_WA_JulSep_pre_reg$M2_trapdens, x.fish_WA_JulSep_2018_2019$M2_trapdens)
# #p-value = 5.22e-08
# 
# kstest_MaySep <- ks.test(x.fish_WA_MaySep_pre_reg$M2_trapdens, x.fish_WA_MaySep_2019_2020$M2_trapdens)
# #p-value < 2.2e-16
# 
# wilcox_test_JulSep <- wilcox.test(M2_trapdens ~ pre_post_reg, data = x.fish_WA_JulSep, exact = FALSE)
# #p-value = 1.799e-08
# 
# wilcox_test_MaySep <- wilcox.test(M2_trapdens ~ pre_post_reg, data = x.fish_WA_MaySep, exact = FALSE)
# #p-value < 2.2e-16


qcomhd_JulSep <- qcomhd(M2_trapdens ~ pre_post_reg, data = x.fish_WA_JulSep, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
qcomhd_JulSep


qcomhd_MaySep <- qcomhd(M2_trapdens ~ pre_post_reg, data = x.fish_WA_MaySep, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
qcomhd_MaySep








####### quantile regression #######

#can specify a tau option which tells rq which conditional quantile we want. 
#The default value for tau is 0.5 which corresponds to median regression. 
fit1 <- rq(M2_trapdens ~ season, tau = .5, data = x.fish_WA_MaySep)
fit1
summary(fit1)

fit2 <- rq(M2_trapdens ~ season, tau = .75, data = x.fish_WA_MaySep)
fit2
summary(fit2)

#the tau option tells rq which conditional quantile we want. 
#we can set tau to be a vector and rq will give us the fits for all those quantiles:
multi_rqfit <- rq(M2_trapdens ~ season, data = x.fish_WA_MaySep, tau = seq(0, 1, by = 0.25))
multi_rqfit
#but the summary command doesn't work when multiple taus are provided ??
summary(multi_rqfit) #doesn't work



fit1 <- rq(M2_trapdens ~ season, tau = 0.25, data = x.fish_WA_MaySep)
fit2 <- rq(M2_trapdens ~ season, tau = 0.5, data = x.fish_WA_MaySep)
fit3 <- rq(M2_trapdens ~ season, tau = 0.75, data = x.fish_WA_MaySep)
anova(fit1, fit2, fit3)



##### WRS2 package #####

fitquart <- Qanova(M2_trapdens ~ season, x.fish_WA_MaySep, q = c(0.25, 0.5, 0.75), nboot = 200)
fitquart


# I think qcomhd() can only compare 2 distributions at a time, so this is not working correctly
#this just compares first two seasons (2013-14 and 2014-15)
testx <- qcomhd(M2_trapdens ~ season, data = x.fish_WA_MaySep, q = c(0.25, 0.5, 0.75), nboot = 500)
testx


# to compare just two distributions, create column to pool pre and post-reg seasons
x.fish_WA_MaySep_groups <- x.fish_WA_MaySep %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "post-reg")
                  )

testx2 <- qcomhd(M2_trapdens ~ seasons_with_regs, data = x.fish_WA_MaySep_groups, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
testx2


#comparing the two post-reg seasons against each other
x.fish_WA_MaySep_xx <- x.fish_WA_MaySep %>% 
  filter(season %in% c('2018-2019','2019-2020')) %>% 
  mutate(season = factor(season, levels = c('2018-2019','2019-2020')))

testx3 <- qcomhd(M2_trapdens ~ season, data = x.fish_WA_MaySep_xx, q = c(0.25, 0.5, 0.75, 1), nboot = 500)
testx3



#quantile regression formula on pooled pre and post reg seasons
m_quantreg <- rq(M2_trapdens ~ seasons_with_regs, data = x.fish_WA_MaySep_groups, tau = seq(0, 1, by = 0.25))
m_quantreg
#summary command doesn't work...??
summary(m_quantreg)







####   K-S   #######
pre_reg_seasons <- x.fish_WA_MaySep %>%  
  filter(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'))
post_reg_2018_2019 <- x.fish_WA_MaySep %>%  filter(season == '2018-2019')
post_reg_2019_2020 <- x.fish_WA_MaySep %>%  filter(season == '2019-2020')
post_reg_2018_2019_2020 <- x.fish_WA_MaySep %>%  filter(season %in% c('2018-2019','2019-2020'))

pre_reg_2017_2018 <- x.fish_WA_MaySep %>%  filter(season == '2017-2018')


kstest1 <- ks.test(pre_reg_seasons$M2_trapdens, post_reg_2018_2019$M2_trapdens)
kstest2 <- ks.test(pre_reg_seasons$M2_trapdens, post_reg_2019_2020$M2_trapdens)
kstest3 <- ks.test(post_reg_2018_2019$M2_trapdens, post_reg_2019_2020$M2_trapdens)

kstest4 <- ks.test(pre_reg_seasons$M2_trapdens, post_reg_2018_2019_2020$M2_trapdens)






#### box-plot #####

p2 <- ggplot(x.fish_WA_MaySep, aes(x=season, y = M2_trapdens))  + 
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  theme_bw()
p2




res <- wilcox.test(M2_trapdens ~ seasons_with_regs, data = x.fish_WA_MaySep_groups,
                   exact = FALSE)
res #p< 0.005


res <- wilcox.test(M2_trapdens ~ season, data = post_reg_2018_2019_2020,
                   exact = FALSE)
res #p< 0.005


pre_reg_2015_2016_2017 <- x.fish_WA_MaySep %>%  filter(season %in% c('2015-2016','2016-2017'))
res <- wilcox.test(M2_trapdens ~ season, data = pre_reg_2015_2016_2017,
                   exact = FALSE)
res #p=0.8115

pre_reg_2015_2016 <- x.fish_WA_MaySep %>%  filter(season == '2015-2016')
pre_reg_2016_2017 <- x.fish_WA_MaySep %>%  filter(season == '2016-2017')
kstest4 <- ks.test(pre_reg_2015_2016$M2_trapdens, pre_reg_2016_2017$M2_trapdens) #p= 0.7721




pre_reg_vs_2018_2019 <- rbind(pre_reg_seasons, post_reg_2018_2019) %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "2018-2019")
  )
res <- wilcox.test(M2_trapdens ~ seasons_with_regs, data = pre_reg_vs_2018_2019,
                   exact = FALSE)
res #p< 0.005


pre_reg_vs_2019_2020 <- rbind(pre_reg_seasons, post_reg_2019_2020) %>% 
  mutate(seasons_with_regs = 
           ifelse(season %in% c('2013-2014','2014-2015','2015-2016','2016-2017','2017-2018'),
                  "pre-reg",
                  "2019-2020")
  )
res <- wilcox.test(M2_trapdens ~ seasons_with_regs, data = pre_reg_vs_2019_2020,
                   exact = FALSE)
res #p< 0.005