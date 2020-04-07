
#Tradeoff plots
# developed from "Simple early closure analysis.Rmd"

#Plotting function
p_function1 <- function(
  df, fishing_col, whaleRisk_col, label_col, region_var, time_var, xaxis_lab, yaxis_lab
) #
{
  p_tmp <- ggplot(
    df, #df
    aes_string(
      x=fishing_col,
      y=whaleRisk_col,
      label=label_col
    )
  ) + # group=1 tells ggplot that there is only 1 group
    geom_point(size=2) + # aes_string(x=time_col,y=response_var,colour=grouping_var,shape=shape_var),
    geom_text_repel() + 
    facet_wrap(as.formula(paste(time_var, "~", region_var)),nrow=2) +
    scale_x_continuous(trans = "log10") +  
    ylab(yaxis_lab) +
    xlab(paste0("log10"," ", xaxis_lab)) +
    theme_classic() +
    theme(
      title = element_text(size = 26),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 20),
      strip.text = element_text(size=18))
  p_tmp
}

# code for debugging function commented out below
# p_function1 <- 
#   ggplot(risk.df.annually.byCAregion.bySeason,
#          aes(
#            x = mean_dollars_DCRB,
#            y = mean_Hump_risk_pings,
#            label = crab.year
#          )
#   ) +
#   geom_point() +
#   geom_text_repel() +
#   facet_wrap(B_or_A_April1~Region,nrow=2) +
#   scale_x_continuous(trans = "log10") +
#   theme_classic() +
#   theme(#legend.title = element_blank(),
#         title = element_text(size = 26),
#         #legend.text = "none",
#         #legend.position = "none",
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         strip.text = element_text(size=18))
# p_function1

```
<br>
  
  Plots of whales and DCRB fishing over last 10 years
```{r}
plot.filepath2 <- paste0(root.dir,"Figures/Whales and fishing plots/")
# df's of interest: risk.df.annually.byCAregion, risk.df.annually.byCAregion.bySeason, risk.df.annually.byAllBIAs, risk.df.annually.byAllBIAs.bySeason

# Humpbacks
# status quo, consider relationship between whales and DCRB pings in winter and spring by region
png(paste0(plot.filepath2, "sum_H_Avg_Abund_sum_Num_DCRB_VMS_pings_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_function1(df = risk.df.annually.byCAregion.bySeason, 
            fishing_col = "sum_Num_DCRB_VMS_pings", 
            whaleRisk_col = "sum_H_Avg_Abund", 
            label_col = "crab.year", 
            region_var = "Region", 
            time_var = "B_or_A_April1", 
            xaxis_lab = "Dungeness crab VMS pings", 
            yaxis_lab = "Predicted abundance of humpback whales") #\n (monthly mean number per 5km grid cell)
dev.off()

# status quo, consider relationship between whale risk and DCRB $ in winter and spring by region
png(paste0(plot.filepath2, "sum_H_Avg_Abund_sum_dollars_DCRB_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_function1(df = risk.df.annually.byCAregion.bySeason, 
            fishing_col = "sum_dollars_DCRB", 
            whaleRisk_col = "sum_H_Avg_Abund", 
            label_col = "crab.year", 
            region_var = "Region", 
            time_var = "B_or_A_April1", 
            xaxis_lab = "Dungeness crab revenue ($)", #\n (monthly mean per 5km grid cell)
            yaxis_lab = "Predicted abundance of humpback whales")
dev.off()

# Blues
# status quo, consider relationship between whales and DCRB pings in winter and spring by region
png(paste0(plot.filepath2, "mean_Blue_occurrence_sum_Num_DCRB_VMS_pings_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_function1(df = risk.df.annually.byCAregion.bySeason, 
            fishing_col = "sum_Num_DCRB_VMS_pings", 
            whaleRisk_col = "mean_Blue_occurrence", 
            label_col = "crab.year", 
            region_var = "Region", 
            time_var = "B_or_A_April1", 
            xaxis_lab = "Dungeness crab VMS pings", #\n (monthly mean per 5km grid cell)
            yaxis_lab = "Predicted occurrence of blue whales")#\n (monthly mean per 5km grid cell)
dev.off()

# status quo, consider relationship between whale risk and DCRB $ in winter and spring by region
png(paste0(plot.filepath2, "mean_Blue_occurrence_sum_dollars_DCRB_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_function1(df = risk.df.annually.byCAregion.bySeason, 
            fishing_col = "sum_dollars_DCRB", 
            whaleRisk_col = "mean_Blue_occurrence", 
            label_col = "crab.year", 
            region_var = "Region", 
            time_var = "B_or_A_April1", 
            xaxis_lab = "Dungeness crab revenue ($)", 
            yaxis_lab = "Predicted occurrence of blue whales")
dev.off()

# status quo, consider relationship between DCRB pings and DCRB $ in winter and spring by region
png(paste0(plot.filepath2, "mean_dollars_DCRB_mean_Num_DCRB_VMS_pings_B_or_A_April1_CA_cenCA_v_norCA_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_function1(df = risk.df.annually.byCAregion.bySeason, 
            fishing_col = "mean_dollars_DCRB", 
            whaleRisk_col = "mean_Num_DCRB_VMS_pings", 
            label_col = "crab.year", 
            region_var = "Region", 
            time_var = "B_or_A_April1", 
            xaxis_lab = "Dungeness crab revenue\n (monthly mean per 5km grid cell)", 
            yaxis_lab = "Dungeness crab pings\n (monthly mean number per 5km grid cell)")
dev.off()

```
<br>
  
  png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Hump_risk_pings_2009-18_annualmeans.png"), width = 10, height = 8, units = "in", res = 300)
p_0_h <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Hump_risk_pings,
    #label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to humpbacks") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  guides(color = guide_legend("Scenario"),  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18)
  )
p_0_h
dev.off()

png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Blue_risk_pings_2009-18_annualmeans.png"), width = 10, height = 8, units = "in", res = 300)
p_0_b <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Blue_risk_pings,
    #label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2, alpha=0.6) +
  stat_err(spread = "se", mult = 2, width=.1) + 
  stat_err(geom="point", size=7, alpha = 0.8) + 
  stat_err(spread = "se", mult = 2, geom = "errorbarh", height = .1) +
  #geom_text() + 
  #geom_point(data=df.tradeoff.annualmeans, size=10) +
  ylab("Relative reduction in risk to blues") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  guides(color = guide_legend("Scenario"),  shape = guide_legend("Scenario")) +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18)
  )
p_0_b
dev.off()

png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Hump_risk_pings_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_1 <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Hump_risk_pings,
    label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2) + # aes_string(x=time_col,y=response_var,colour=grouping_var,shape=shape_var),
  geom_text_repel() + 
  #facet_wrap(as.formula(paste(time_var, "~", region_var)),nrow=2) +
  #scale_x_continuous(trans = "log10") +  
  ylab("Relative reduction in risk to humpbacks") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18))
p_1
dev.off()

png(paste0(plot.filepath, "within_year_relative_dollars_DCRB_relative_Blue_risk_pings_2009-18.png"), width = 10, height = 8, units = "in", res = 300)
p_2 <- ggplot(
  df.tradeoff, #df
  aes(
    x=relative_dollars_DCRB,
    y=relative_Blue_risk_pings,
    label=crab.year,
    colour=Scenario_long
  )
) + # group=1 tells ggplot that there is only 1 group
  geom_point(size=2) + # aes_string(x=time_col,y=response_var,colour=grouping_var,shape=shape_var),
  geom_text_repel() + 
  #facet_wrap(as.formula(paste(time_var, "~", region_var)),nrow=2) +
  #scale_x_continuous(trans = "log10") +  
  ylab("Relative reduction in risk to blues") +
  xlab("Relative revenue to the Dungeness crab fishery") +
  theme_classic() +
  theme(
    title = element_text(size = 26),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20),
    strip.text = element_text(size=18))
p_2
dev.off()
