#Setup

#import libraries

library(tidyverse)
library(haven)
library(labelled)
library(dplyr)
library(sRtr)
library(ggplot2)
library(tidyr)
library(janitor)
library(readr)
library(purrr)
library(gtsummary)
library(gt)
library(writexl)
library(readxl)
library(MatchIt)
library(lubridate)
library(survival)
library(survminer)
library(MASS)
library(sandwich)
library(lmtest)
library(conflicted)
library(sRtr)

#Use the conflicted package to ensure that the select and filter functions are not superseded
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
