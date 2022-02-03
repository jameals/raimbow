# raimbow

<!-- badges: start -->
<!-- badges: end -->

This repository contains code for the RAIMBOW (Risk Assessment Integration for Mitigating Bycatch of Whales) project. Code for various pieces of the project are divided into folders:

* prep: Basic prep steps for analyses, such as creating grid cell - info keys and defining Regions. Also includes Grid5km_landerase.R, a  script to erase land from Blake's 5km equal area grid, and save the resulting object for future use in whale scripts.

* JS_OceanVisions: Code used to create plots of Jameal's April 2019 OceanVisions presentation

* blue_whale: Files and code for overlaying blue whale predictions (Abrahms et al 2019) onto the 5km grid using overlay method (areal interpolation)

* humpback_risk: Files and code used to determine the risk of entanglement for humpbacks in the Dungeness Crab fishery off the US west coast.

* tradeoffs: Tradeoff analyses for humpback and blue whale entanglement risk and Dungeness Crab fishery activity

* whalepreds_aggregate: Functions to summarize whale predictions by specified time interval. DO NOT EDIT; any edits should be done in [the whale-model-prep repository](https://github.com/smwoodman/whale-model-prep) and copied over

Other files include:

* User_script_local.R: Script for determining whom is running the code (user info used to set appropriate file paths); sourced in relevant files

Files in humpback_risk are for Forney et al. (in prep)
