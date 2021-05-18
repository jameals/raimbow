library(tidyverse)

load("../raimbow-local/RDATA_files/ngb_Whale_risk_timeseries.Rdata")
all.df.ngb <- all.df
all.df.summ.ngb <- all.df.summ
rm(all.df, all.df.summ)

load("../raimbow-local/RDATA_files/Whale_risk_timeseries.Rdata")


#####
diff.df <- all.df.summ %>% 
  select(risk_sum_total) %>% 
  mutate(risk_sum_total_ngb = all.df.summ.ngb$risk_sum_total, 
         risk_total_diff = risk_sum_total - risk_sum_total_ngb, 
         risk_total_diff_abs = abs(risk_total_diff), 
         risk_total_perc = risk_total_diff / risk_sum_total, 
         risk_total_perc_abs = risk_total_diff_abs / risk_sum_total)

ggplot(diff.df, aes(risk_sum_total, risk_sum_total_ngb)) + 
  geom_point() + 
  geom_abline(slope = 1, col = "blue")


#####
diff.df.mn <- all.df.summ %>% 
  select(mn_sum_abund) %>% 
  mutate(mn_sum_abund_ngb = all.df.summ.ngb$mn_sum_abund, 
         mn_abund_diff = mn_sum_abund - mn_sum_abund_ngb, 
         mn_abund_diff_abs = abs(mn_abund_diff), 
         mn_abund_perc = mn_abund_diff / mn_sum_abund, 
         mn_abund_perc_abs = mn_abund_diff_abs / mn_sum_abund)

ggplot(diff.df.mn, aes(mn_sum_abund, mn_sum_abund_ngb)) + 
  geom_point() + 
  geom_abline(slope = 1, col = "blue")


#####
x <- inner_join(
  select(all.df, GRID5KM_ID, ym, risk_total), 
  select(all.df.ngb, GRID5KM_ID, ym, risk_total_ngb = risk_total)
)
x.nona <- x %>% 
  filter(!is.na(risk_total) & !is.na(risk_total_ngb))
cor(x.nona$risk_total, x.nona$risk_total_ngb)

ggplot(x, aes(risk_total, risk_total_ngb)) + 
  geom_point() + 
  geom_abline(slope = 1, col = "blue")


#####
x.mn <- inner_join(
  select(all.df, GRID5KM_ID, ym, mn_abund), 
  select(all.df.ngb, GRID5KM_ID, ym, mn_abund_ngb = mn_abund)
)
cor(x.mn$mn_abund, x.mn$mn_abund_ngb)

ggplot(x.mn, aes(mn_abund, mn_abund_ngb)) + 
  geom_point() + 
  geom_abline(slope = 1, col = "blue")

