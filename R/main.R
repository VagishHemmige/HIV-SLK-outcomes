#Main R script that runs other R scripts in order

#Initialization
source("R/setup.R")

#Files that define convenience functions for use in the analysis
source("R/functions.R")

#Primary data preparation
source("R/primary_data_preparation/import.R")
source("R/primary_data_preparation/excluding_prior_transplants.R")
source("R/primary_data_preparation/combining_liver_kidney_variables.R")
source("R/primary_data_preparation/create_hiv_data_set.R")
source("R/primary_data_preparation/process_slk_data.R")
source("R/primary_data_preparation/create_hiv_data_set.R")
source("R/primary_data_preparation/create_liver_only_dataset.R")



#Adding data from other data sets
source("R/add_data/add_donor_data.R")
source("R/add_data/add_candidate_data.R")



#Modeling


source("R/tables.R")
source("R/figures.R")
