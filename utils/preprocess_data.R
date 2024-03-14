# ---------------------------------------------------------------------------- #
# Data Analyst: Gabriel Okasa
# Script: Loading and pre-processing raw data for analyses
# ---------------------------------------------------------------------------- #

# load libraries
library(here)
library(conflicted)
library(dplyr)

# resolve conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# ---------------------------------------------------------------------------- #
# load the raw data
data_raw <- read.csv(here("data", "data_raw.csv"))

# load data on federal contributions
data_federal <- read.csv(here("data", "data_federal.csv"))

# ---------------------------------------------------------------------------- #
# relabel international short research visits as science communication
data <- data_raw %>%
  # mutate the funding instrument from Careers to Science communication
  mutate(FundingInstrumentGaLevel1 = if_else(
    FundingInstrumentGaReporting == "International short research visits",
    "Science communication", FundingInstrumentGaLevel1))

# ---------------------------------------------------------------------------- #
# impute Eccelenza data for 22/23 as it has been integrated into starting grants
data_eccelenza_imputed <- data %>%
  # filter only Eccelenza
  filter(FundingInstrumentGaReporting == "Eccellenza") %>%
  # group by year
  group_by(CallDecisionYearReporting) %>%
  # summarize the quantities of interest needed for the descriptive analyses
  get_descriptive_stats() %>%
  # ungroup
  ungroup() %>%
  # and compute the averages of the quantities over the 4 years of Eccelenza
  colMeans(., na.rm = TRUE) %>%
  # format as dataframe
  as.data.frame() %>%
  # and transpose
  t() %>%
  # back to dataframe
  as.data.frame() %>%
  # and round the relevant columns to integers
  mutate(
    # integer for number of submissions
    TotalNumberSubmitted = round(TotalNumberSubmitted),
    # as well as number of approvals
    NumberApproved = round(NumberApproved),
    # force CallDecisionYearReporting to 2022 for later add_row()
    CallDecisionYearReporting = 2022,
    # and add column for FundingInstrumentGaReporting
    FundingInstrumentGaReporting = "Eccellenza")

# ---------------------------------------------------------------------------- #
# impute PRIMA data for 23 as it has been integrated into starting grants too
data_prima_imputed <- data %>%
  # filter only Eccelenza
  filter(FundingInstrumentGaReporting == "PRIMA") %>%
  # group by year
  group_by(CallDecisionYearReporting) %>%
  # summarize the quantities of interest needed for the descriptive analyses
  get_descriptive_stats() %>%
  # ungroup
  ungroup() %>%
  # and compute the averages of the quantities over the 5 years of PRIMA
  colMeans(., na.rm = TRUE) %>%
  # format as dataframe
  as.data.frame() %>%
  # and transpose
  t() %>%
  # back to dataframe
  as.data.frame() %>%
  # and round the relevant columns to integers
  mutate(
    # integer for number of submissions
    TotalNumberSubmitted = round(TotalNumberSubmitted),
    # as well as number of approvals
    NumberApproved = round(NumberApproved),
    # force CallDecisionYearReporting to 2023 for later add_row()
    CallDecisionYearReporting = 2023,
    # and add column for FundingInstrumentGaReporting
    FundingInstrumentGaReporting = "PRIMA")

# ---------------------------------------------------------------------------- #
# remove the raw data from the environment
rm(data_raw)

# ---------------------------------------------------------------------------- #
