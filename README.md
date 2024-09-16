# Freshwater fish climate responses

Freshwater fish abundance responses to climate change paper

Data and code for Brown et al. "Climate warming drives population trajectories of freshwater fish"


***

**assets**

***


RivFishTimeSurveyTable.csv Survey data obtained from RivFishTime (uploaded version requires unzipping prior to analysis)

RivFishTIME_TimeseriesTable.csv Time series data obtained from RivFishTime

RFT_COMB.csv Intermediate dataframe combining RivFishTime datasets (uploaded version requires unzipping prior to analysis)

GBIF data are available to download at  https://doi.org/10.15468/dl.c97hn3 

Tclim_input.csv Dataframe with the downloaded and unpacked climate data to match RivFishTime locations and times

df_models_avg_03May2024.csv Compiled data required for analyses, Tavg

df_models_max_03May2024.csv Compiled data required for analyses, Tmax

***

**scripts**

***

Section1_DataCompilation.R  Code to generate the dataframes for analysis from source data

Section2_Analysis.R  Code to conduct statistical analyses

Section3_PlottingMaps.R Code to plot figures
