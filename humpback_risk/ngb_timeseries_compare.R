library(tidyverse)

load("../raimbow-local/RDATA_files/ngb_Whale_risk_timeseries.Rdata")
all.df.ngb <- all.df
all.df.summ.ngb <- all.df.summ
rm(all.df, all.df.summ)

load("../raimbow-local/RDATA_files/Whale_risk_timeseries.Rdata")



diff.df <- all.df.summ %>% 
  select(risk_sum_total) %>% 
  mutate(risk_total_diff = all.df.summ$risk_sum_total - all.df.summ.ngb$risk_sum_total, 
         risk_total_diff_abs = abs(risk_total_diff), 
         risk_total_perc = risk_total_diff / risk_sum_total, 
         risk_total_perc_abs = risk_total_diff_abs / risk_sum_total)
sum(abs(risk.total.diff))


diff.df.mn <- all.df.summ %>% 
  select(mn_sum_abund) %>% 
  mutate(mn_abund_diff = all.df.summ$mn_sum_abund - all.df.summ.ngb$mn_sum_abund, 
         mn_abund_diff_abs = abs(mn_abund_diff), 
         mn_abund_perc = mn_abund_diff / mn_sum_abund, 
         mn_abund_perc_abs = mn_abund_diff_abs / mn_sum_abund)
