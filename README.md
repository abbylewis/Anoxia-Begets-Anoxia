# Anoxia-Begets-Anoxia

## Summary

An analysis of oxygen, phosphorus, and chlorophyll-a dynamics across >650 widespread stratified lakes

![epi and hypo tp abstract](https://github.com/abbylewis/Anoxia-Begets-Anoxia/assets/51751937/11dce591-ca64-4486-86d0-3a74247bba1a)

## Keywords

Air temperature, anoxia, chlorophyll-a, dissolved oxygen, feedback, hypolimnion, oxygen demand, phosphorus, residence time, surface area

## Data availability

* In-lake data for this study published on the Environmental Data Initative data portal: https://doi.org/10.6073/pasta/2cd6628a942de2a8b12d2b19962712a0
* Additional data are presented in `./External data`:
    * Air temperature and climate data are drawn from the ERA5 climate reanalysis. ERA5 is a fifth-generation product from the European Centre for Medium-Range Weather Forecasts (ECMWF), and provides data from 1959–2022 on a 0.25 degree global grid (Hersbach et al., 2019)
    * Chlorophyll-a data from Filazzola et al. (2020) were added for n = 15 lakes without other data for chlorophyll-a
    *  We collated additional metadata for each lake using HydroLAKES, a global database of 1.4 million lakes (with surface area ≥10 ha; Messager et al., 2016)

## Repo content information

### ./Data analysis

All data analysis scripts:
* `01 - Load climate - ERA5.Rmd`
    * Load netcdf of global climate data and output a csv with air temperature records at each lake
* `02 - Temp and DO interpolation.Rmd`
    * Interpolate temperature and oxygen profiles to a 1 m depth resolution
* `03 - Stratified avgs.Rmd`
    * Calculate epilimnetic and hypolimnetic means during the stratified period of each year. Add in additional chlorophyll-a data using "chla_harmonizer.csv"
* `04 - Summer avgs.Rmd`
    * Calculate epilimnetic and hypolimnetic means during the late-summer period of each year
* `05 - hydrolakes.Rmd`
    * Load all hydrolakes data and output a csv with hydrolakes metadata for each lake
* `06 - VW DO Demand - based on strat dur.Rmd`
    * Calculate volume-weighted hypolimnetic oxygen demand for each lake-year
* `07 - Compile data.Rmd`
    * Combine late-summer and stratified means with oxygen demand, and climate data. Output a file for following analyses.
* `08 - Data characterization.Rmd`
    * Characterize the full, synthesized dataset. Output summary figures
* `09 - Anoxic factor.Rmd`
    * Characterize the full, synthesized dataset. Output summary figures
* `10 - Anoxic factor layers.Rmd`
    * Characterize the full, synthesized dataset. Output summary figures
* `11 - Air temp analysis.Rmd`
    * Analyze the effects of changing monthly air temperatures on hypolimnetic temperature and oxygen dynamics
* `12 - Lags.Rmd`
    * Analyze the relevant lags for each relationship in the anoxia-begets-anoxia (ABA) feedback
* `13 - Regressions.Rmd`
    * Perform regression analyses within and across lakes to characterize the ABA feedback. Functions for this analysis are in `lmer_functions.R`
* `14 - Depth cutoff of 3m.Rmd`
    * Re-run all analyses to generate Figure 3 using data from lakes ≥ 3 m, rather than ≥ 6.4 m

### ./External data

Downloaded data (unmodified from original sources)

### ./Compiled data

Compiled datasets, created by the scripts in `./Data analysis`

### ./Figures

Figures created by the scripts in `./Data analysis`

### ./Illustrator files

Adobe illustrator files used to create conceputal figure, graphical abstract, and annotated figures for manuscript

## References

Filazzola, A., Mahdiyan, O., Shuvo, A., Ewins, C., Moslenko, L., Sadid, T., Blagrave, K., Gray, D., Quinlan, R., O’Reilly, C., & Sharma, S. (2020). A global database of chlorophyll and water chemistry in freshwater lakes. https://doi.org/10.5063/F1JH3JKZ

Hersbach, H., Bell, B., Berrisford, P., Biavati, G., Horányi, A., Muñoz Sabater, J., Nicolas, J., Peubey, C., Radu, R., Rozum, I., Schepers, D., Simmons, A., Soci, C., Dee, D., & Thépaut, J.-N. (2019). ERA5 monthly averaged data on single levels from 1979 to present [Data set]. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). https://doi.org/10.24381/CDS.F17050D7

Messager, M. L., Lehner, B., Grill, G., Nedeva, I., & Schmitt, O. (2016). Estimating the volume and age of water stored in global lakes using a geo-statistical approach. Nature Communications, 7(1), Article 1. https://doi.org/10.1038/ncomms13603
