# EcoCast-SciAdv

<img src="inst/imgs/logo.png?raw=True" width="400">

Code supporting the publication "A dynamic ocean management tool to reduce bycatch and support sustainable fisheries". Science Advances. 2018.

Code authors: Elliott Hazen (NOAA, UCSC), Kylie Scales (University of the Sunshine Coast), Heather Welch (UCSC, NOAA), Sara Maxwell (ODU), and Dana Briscoe (UCSC)

Relevant manuscripts:

**Hazen et al. 2018** “A dynamic ocean management tool to reduce bycatch and support sustainable fisheries.” Science Advances 4: eaar3001. 

**Maxwell et al. 2019** "Oceanographic drivers of spatial segregation in blue sharks by sex and size class." Diversity and Distributions 25: 1304-1317. doi.org/10.1111/ddi.12941

**Welch et al. 2018** "Practical considerations for operationalizing dynamic management tools." Journal of Applied Ecology.DOI: 10.1111/1365-2664.13281.

**Briscoe et al. 2018** "Characterizing habitat suitability of a central-place forager in a dynamic marine environment." Ecology and Evolution. DOI: 10.1002/ece3.3827.

**Scales et al. 2017** "Fit to predict? Eco-informatics for predicting the catchability of a pelagic fish in near real time." Ecological Applications 27: 2313-2329.  

**Hazen et al. 2017** "WhaleWatch: a dynamic management tool for predicting blue whale density in the California Current." Journal of Applied Ecology 54: 1415-1428.  


## Description of scripts:

**1_load_packages.R** - Checks if all required packages are installed. If not, packages and all dependencies will be installed from the default CRAN mirror.  

**2_download_environmental_data.R** - Downloads relevant environmental data as netcdfs from CoastWatch-West Coast ERDDAP: https://coastwatch.pfeg.noaa.gov/erddap/index.html. Data not available on ERDDAP can be downloaded directly from the data provider, such as SSHa via AVISO / CMEMS, e.g. https://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/sla-h.html 

**3a_extract_functions.R** - Functions to support 3_extract_environmental_data_at_records.R.  

**3b_extract_environmental_data_at_records.R** - Extracts downloaded environmental data to species records.  

**4_fit_and_evaluate_BRTs.R** - Fits and evaluates Boosted Regression Tree models for a single species.  

**5_predict_BRTs.R** - Predicts confidence intervals, mean, and standard error surfaces for all species for target date.  

**6_plot_EcoCast.R** - Runs and plots EcoCast for all species.

EcoCast information page (tool and product) will be available at: http://coastwatch.pfeg.noaa.gov/ecocast

Operationalization built by Heather Welch, for the daily running of EcoCast, is available at: https://github.com/HeatherWelch/EcoCast_Operationalization
