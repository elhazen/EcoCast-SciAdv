# EcoCast-SciAdv
Code supporting the publication "A dynamic ocean management tool to reduce bycatch and support sustainable fisheries". Science Advances. 2018.

Code authors: Elliott Hazen (NOAA), Kylie Scales (University of the Sunshine Coast), Heather Welch (UCSC, NOAA), Sara Maxwell (ODU), and Dana Briscoe (UCSC)

Relevant manuscripts:

**Hazen et al.** “A dynamic ocean management tool to reduce bycatch and support sustainable fisheries.” Sciences Advances. 2018.  
**Welch et al.** "Practical considerations for operationalizing dynamic management tools." In prep.  
**Scales et al.** "Fit to predict? Eco-informatics for predicting the catchability of a pelagic fish in near real time." Ecological Applications 27: 2313-2329.
**Hazen et al. 2017.** "WhaleWatch: a dynamic management tool for predicting blue whale density in the California Current." Journal of Applied Ecology 54: 1415-1428.  

## Description of scripts:

    Get_Env_Data_A.R (run once at beginning of the day): Get data sequence number one : Create final and temporary directories, acquire all static variables.
    Get_Env_Data_B.R (run multiple times during the day): Get data sequence number two : See which dynamic variables are missing. If none are missing, run EcoCast. If variables are missing, attempt to download missing variables. See which dynamic variables are still missing after download attempt. If none are missing, run EcoCast.
    Get_Env_Data_C.R (run once at end of the day): Get data sequence number three : Evaluates the most recent version of environmental data available, and then runs EcoCast. Script will not overwrite pre-existing final products (i.e. if final products were created by Get_Env_Data_B.R).
