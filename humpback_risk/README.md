# Humpback risk analyses for Forney et al (in prep)

Note that these files are numbered according to files dependencies. For instance, "3_" files depend on output from "2_" files, etc.

'Mn' refers to <em>Megaptera novaeangliae</em>, the scientific name for humpback whales



## File descriptions

<!-- section break -->
#### Overlay and aggregate humpback predictions

* 1_Mn_predsaggr_overlay5km.R: For processing  (overlaying) 3km Mn data from KAF that has already been aggregated to monthly averages. Saves the data as a long rds file. Used for Forney et al. (in prep)

* 1a_Mn_preds_overlay5km.R: Overlay 3km Mn (bidaily) predictions from an Excel file onto the 5km grid from Blake

* 1b_Mn_preds_aggr.R: Aggregate bidaily output from '1a_Mn_preds_overlay5km.R' to monthly averages, and save the data as long data used in the rest of the workflow.

<!-- section break -->
#### Calculate risk data frames and assign to zones

* 2_Whale_risk.Rmd: Process and aggregate humpback and fishing data, and then calculate (humpback) whale risk of entanglement for each grid cell as: humpback density * VMS pings. This file then saves the humpback (abundance), fishing (total sum: VMS pings), and risk (total) values as an RDATA file (Whale_risk_long_nona.RDATA) for use in subsequent files.

* 2b_Grid_region.R: Get the region (both large and county) for each 5km grid cell ID. Superseded by 2b_Grid_zones.R

* 2b_Grid_region_zones.R: Get the region for each 5km grid cell ID, with region meaning zone 1-5 as defined by the California Department of Fish and Wildlife for their [Risk Assessment and Mitigation Program (RAMP)](https://wildlife.ca.gov/Conservation/Marine/Whale-Safe-Fisheries). This script saves two files to be used later: Grid_region_zones.Rdata, which contains a grid - zone-based region key and Grid_zones.Rdata, which contains a grid cell to zone key.

<!-- section break -->
#### Generate maps and timeseries plots

* 3_Whale_risk_maps.Rmd: Generates heat maps of humpback/fishing/risk data. Use 3_Whale_risk_maps_job.R to run this code as a job in RStudio.

* 3_Whale_risk_timeseries.Rmd: Summarize and plot timeseries of humpback whale risk of entanglement by region (zone). Generates figures for Forney et al

* 3_Whale_risk_timeseries_county: Same as 3_Whale_risk_timeseries.Rmd, except using county regions from Grid_region_county.Rdata

* 3_Whale_risk_timeseries_origregions.Rmd: Same as 3_Whale_risk_timeseries.Rmd, except using original regions from Grid_region.Rdata. This should be run with caution, as is mostly kept as a reference to an earlier phase.

* 3_Whale_risk_county_zones: Same as 3_Whale_risk_timeseries.Rmd, except using zone regions from Grid_zones.Rdata

* 3_Entanglement_report_mnpred.Rmd: Compare entanglement report locations with humpback predictions for Karin.





* 4_Entanglement_gridID.Rmd: Determine grid cell values, for report location and gear set county, for CA DC humpback entanglements with known gear set/time

* 4_Entanglements_risk.Rmd: Examine relationship between risk values and entanglement reports, including using lookback window

* 4_Whale_risk_management.Rmd: Examine the change in humpback entanglement risk under different management scenarios, e.g. early or late season closures, with no effort displacement. Used in Forney et al

* 4_Whale_risk_management_displacement.Rmd: Examine the change in humpback entanglement risk under different management scenarios, e.g. early or late season closures, while accounting for effort displacement

* 4_Whale_risk_timeseries_base.Rmd: A look at how risk would change if all humpback or fishing values were 'baseline' values, meaning the average of the values for the 2009-2010 to 2012-2013 fishing seasons

* 5_Whale_risk_timeseries_presentation.Rmd: Starting point of JVR presentation on Mn risk assessment for TriState call

* \_county_: Analyses (described above) but using CA counties instead of CA regions

* ngb_: Comparing overlaying Mn predictions onto 5km grid with resample onto 5km grid using nearest neighbors approach


<!-- section break -->
#### Other files

* Timeseries_forJVR.R: Output various humpback risk data to CSV files for JVR

* VMS_nonconfidential_duplicates.R: Identify duplicate rows in CA-only, non-confidential data

<!-- section break -->
#### Helper files - functions

* plot_raimbow.R: Functions for plotting objects (specifically maps) from humpback_risk raimbow analyses

* Funcs_whale_risk_mgmt.R: Functions for running and plotting output from management scenarios

* Funcs_whale_risk_timeseries.R: Functions for creating time series plots
