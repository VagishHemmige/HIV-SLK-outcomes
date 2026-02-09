#Setup

# ----This section imports libraries necessary for the analysis----

#File import libraries
library(haven)
library(readr)
library(readxl)

#File export libraries
library(writexl)

#Data management libraries
library(tidyverse)
library(labelled)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(lubridate)
library(conflicted)

#Core library for the SRTR
library(sRtr)

#Flowchart libraries
library(strobe)
library(flowchart)

#Statistical modeling libraries
library(MatchIt)
library(survival)
library(MASS)
library(sandwich)
library(lmtest)


#Table libraries
library(gtsummary)
library(gt)

#Plotting libraries
library(ggplot2)
library(survminer)

#Use the conflicted package to ensure that the select and filter functions are not superseded
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
