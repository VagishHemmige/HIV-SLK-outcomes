#Main R script that runs other R scripts in order

source("R/setup.R")
source("R/functions.R")


source("R/import.R")
source("R/excluding_prior_transplants.R")
source("R/combining_liver_kidney_variables.R")
source("R/create_hiv_data_set.R")


source("R/tables.R")
source("R/figures.R")
