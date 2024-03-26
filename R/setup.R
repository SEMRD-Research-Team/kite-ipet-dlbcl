library(tidyverse)
library(magrittr)
library(coRanalysis)
library(dbplyr)
library(here)
library(progress)

source(here::here("R", "db.R"))

# DB Connections
db <- kiteDb$new()
db_con <- db$connection
schema <- db$schema
schema_public <- db$schema_public
Sys.setenv( aws_username = "ang@corevitas.com" )

# Project dates
study_start <- as.Date("2021-01-01")
study_end <- as.Date("2023-11-30")

eligible_start <- as.Date("2021-01-01")
eligible_end <- as.Date("2023-04-30")

lookback_start <- as.Date("2018-04-01")
lookback_end <- as.Date("2020-12-31")
