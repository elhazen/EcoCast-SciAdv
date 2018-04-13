# EcoCast-SciAdv
Code supporting the publication "A dynamic ocean management tool to reduce bycatch and support sustainable fisheries". Science Advances. 2018.

Code authors: Elliott Hazen (NOAA), Kylie Scales (University of the Sunshine Coast), Heather Welch (UCSC, NOAA), Sara Maxwell (ODU), and Dana Briscoe (UCSC)

Relevant manuscripts:

**Hazen et al.** “A dynamic ocean management tool to reduce bycatch and support sustainable fisheries.” Sciences Advances. 2018.  
**Welch et al.** "Practical considerations for operationalizing dynamic management tools." In prep.  
**Scales et al. 2017** "Fit to predict? Eco-informatics for predicting the catchability of a pelagic fish in near real time." Ecological Applications 27: 2313-2329.  
**Hazen et al. 2017** "WhaleWatch: a dynamic management tool for predicting blue whale density in the California Current." Journal of Applied Ecology 54: 1415-1428.  

## Description of scripts:

**1_load_packages.R** - Checks if all required packages are installed. If not, packages and all dependencies will be installed from the default CRAN mirror.  
**2_download_environmental_data.R** - Downloads relevant environmental data as netcdfs from CoastWatch-West Coast ERDDAP: https://coastwatch.pfeg.noaa.gov/erddap/index.html. Data included not available on ERDDAP such as SSH can be downloaded directly from AVISO / CMEMS, e.g. https://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/sla-h.html 

**3a_extract_functions.R** - Functions to support 3_extract_environmental_data_at_records.R.  
**3b_extract_environmental_data_at_records.R** - Extracts downloaded environmental data to species records.  
**4_fit_and_evaluate_BRTs.R** - Fits and evaluates Boosted Regression Tree models.  
**5_predict_BRTs.R** - Predicts confidence intervals, mean, and standard error surfaces for all species for target date.  
**6_plot_EcoCast.R** - Runs and plots EcoCast for all species.

EcoCast information page (tool and product) will be available at:

Operationalization built by Heather Welch, for the daily running of EcoCast, is available at: https://github.com/HeatherWelch/EcoCast_Operationalization
