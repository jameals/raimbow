#####################################################

# create normalized pings, with reference to all years within the study period. that way we can compare normalized pings

#dcrb_ca_vms_tix_analysis_TripInfo %>%


range(risk_5km_yr_mth_sq_all$Num_DCRB_VMS_pings)  

# when scaled pings = 0, reset pings to be = 0.5 * the minimum non zero rescaled value (2 pings on the original scale)
tmp <- as.vector(scale(risk_5km_yr_mth_sq_all$Num_DCRB_VMS_pings,center=min(risk_5km_yr_mth_sq_all$Num_DCRB_VMS_pings),scale=diff(range(risk_5km_yr_mth_sq_all$Num_DCRB_VMS_pings))))
range(tmp)
one_ping_scaled_value = 0.5 * min( tmp[tmp!=min(tmp)] )
ggplot(data.frame(tmp)) + geom_histogram(aes(x=tmp), bins = 100)

tmp2 <- risk_5km_yr_mth_sq_all$Num_DCRB_VMS_pings/max(risk_5km_yr_mth_sq_all$Num_DCRB_VMS_pings)
range(tmp2)
min( tmp2[tmp2!=min(tmp2)] )
ggplot(data.frame(tmp2)) + geom_histogram(aes(x=tmp2), bins = 100)

#####################################################