
# title: "data_queries"
# author: "Jackson Bolos"
# date: "2024-01-23"

app_folder_path <- "../Econ Council Forecast Info App"


# NOTE: Some external sources (BEA, BLS, etc.) have more lag time in updating
#       certain indicators, so it is best to run this shortly before the council
#       meets to ensure as much data is included as possible.
#       A main purpose of this script is to automatically update data from all sources.


# UEC Forecasts SECTION STILL REQUIRES MANUAL ALTERATIONS (as well as within
# each indicator where UEC forecasts table is filtered to appropriate forecast)


# Gardner Institute Unofficial Population Estimate (%)
include_gardner <- TRUE
current_year_estimate <- 0.0165 # for current/unfinished year - number in decimal (0.01) format
next_year_estimate <- 0.0164 # for next year
two_year_estimate <- NA # forecast for in two years
date_received <- "Dec 2023" # string in "Mon YYYY" format

# Utah Population Committee Estimate (%)
include_committee <- TRUE
estimate <- 0.016 # for current/unfinished year - number in decimal (0.01) format


# BLUE CHIP US FORECASTS
include_blue_chip <- TRUE

# Blue Chip CPI
cpi_current_year_estimate <- 2.7 # for current/unfinished year - number in percent format
cpi_next_year_estimate <- 2.3 # for next year
cpi_two_year_estimate <- NA # forecast for in two years

# Blue Chip Unemployment Rate
ur_current_year_estimate <- 3.9 # for current/unfinished year - number in percent format
ur_next_year_estimate <- 4.1 # for next year
ur_two_year_estimate <- NA # forecast for in two years

# Blue Chip GDP
gdp_current_year_estimate <- 2.3 # for current/unfinished year - number in percent format
gdp_next_year_estimate <- 1.8 # for next year
gdp_two_year_estimate <- NA # forecast for in two years

# SETUP -------------------------------------------------------------------
library(tidyverse)
library(zoo)
# library(alfred)
library(fredr) # this one allows for more parameters
library(magrittr)
library(ggplot2)
library(gginnards)
library(plotly)
app_folder_path <- "C:/Users/ajbolos/RStudio/Economic Council Forecast Information/Econ Council Forecast Info App/"
# library(devtools)
# install_github("mikeasilva/blsAPI") # had to go through github because CRAN wasn't working
library(blsAPI)
library(readxl)
library(scales)
# library(stringi)
# library(stringr)
# library(reshape2)
# library(rjson)
# library(tempdisagg)

library(bea.R)

# for Moody's api
library(digest)
library(jsonlite)
library(httr)
# library(utils)
# library(xts)
# #library(xlsx)
# library(hrbrthemes)
# library(openxlsx)

# for Kem C. Gardner
library(rvest)


# api keys
bls_api = "136c5361cc6f4c738d8adb0081e9443e"
fred_api = "4e8a57a0570f125673e83ea04173ee40"
fredr_set_key(fred_api)
ACC_KEY <- "ECBD5151-359A-4F9B-BD37-1E1BE094A3A3" # get my own?
ENC_KEY <- "C4897F65-D794-4C59-ACA1-9D7093246007" # get my own?
bea_api <- "434CD0CA-10AA-406E-A251-E2ACAA53CF17"

# reference date info (all lowercase - give column names a Capital)
today <- as.Date(Sys.Date(), format = "%Y-%m-%d")
month <- as.yearmon(today)
quarter <- as.yearqtr(today)
year <- year(today)

# fiscal year needed?
FY_Month <- as.numeric(substr(today,start=6,stop=7))
Current_FY <- ifelse(FY_Month %in% c(1:6),year,as.numeric(Current_CY)+1)

# start date for recession plot
start_date <- as.Date("1970-01-01")


# THIS FUNCTION MAKES A FEW GENERALIZED ALTERATIONS TO ALL PLOT DATA
# Excludes forecasts of previous quarters within current year
# Drop quarters of historical data that aren't complete
# Fill plot gaps where forecasts don't start at most recently recorded historical date
forecast_backfill <- function(history_source, dataframe)
{
  # only year was originally used for filtering, which included past quarters of current year
  # filter forecasts to only include future and current/unfinished time periods
  if(class(dataframe$Date) == "Date")
  {
    dataframe %<>%
      filter(source == history_source | as.yearqtr(Date) >= quarter)
  }
  
  
  
  # get last date of historical data
  last_hist_date <- dataframe %>%
    filter(source == history_source) %>%
    arrange(Date) %>%
    slice(n()) %>%
    pull(Date)
  
  # determine if the last piece of historical data is a complete quarter/year
  # we can't know what a quarter's/year's indicator is until the timeframe is over
  # catch quarterly-dated data overlapping on quarter | catch yearly-dated data overlapping on year
  if(as.yearqtr(last_hist_date) == quarter | last_hist_date == year)
  {
    # logic is the same for both types of Date: "Date" & "numeric"
    dataframe %<>%
      filter(!(source == history_source & Date == last_hist_date))
  }
  
  
  # slice last row of historical data for copying over to forecasts
  last_hist <- dataframe %>% # run this again in case there's a new last row from the above code chunk
    filter(source == history_source) %>%
    slice(n())
  
  # get last date of historical data for comparison
  last_hist_date <- last_hist %>%
    pull(Date)
  
  # get first date of all forecast data for comparison
  first_fc_date <- dataframe %>%
    filter(source != history_source) %>%
    summarize(earliest_date = min(Date)) %>%
    pull(earliest_date)
  
  # see if there is a gap between last historical date and first forecast data
  if(first_fc_date > last_hist_date)
  {
    # obtain all forecast sources (not historical source)
    ecin_forecasts <- unique(dataframe$source[dataframe$source != history_source])
    
    # add entry for each forecast to match it up with last historical day
    for(forecast_source in ecin_forecasts)
    {
      
      new_row <- last_hist %>% mutate(source = forecast_source)
      
      dataframe <- rbind(dataframe, new_row)
      
    }
    # this orders the dates, but puts the historical data first
    source_order <- unique(dataframe$source)
    dataframe %<>% arrange(factor(source, levels = source_order), Date)
  }
  
  return(dataframe)
}

# UEC Forecasts -----------------------------------------------------------

# SINCE THIS ISN'T AUTOMATED, GETTING DATA FORMATTED THE SAME WAY
# AS THE FINAL uec_forecasts AT THE END OF THIS SECTION WILL SUFFICE

# date of responses for forecast date in app (insert in "YYYY-MM-DD" format)
uec_date <- as.Date("2023-12-01")
saveRDS(uec_date, paste0(app_folder_path, "uec_date.rds"))

uec_responses <- read_excel("december_uec_responses.xlsx")

uec_responses %<>%
  select(starts_with("Q"))

colnames(uec_responses) <- uec_responses[1, ]

uec_responses %<>%
  slice(-c(1, 2))

extra_text <- "US Economic Indicators:\r\n\r\nPoint - What is your best estimate for the following economic variables, to the nearest tenth of a percent?\r\n\r\nRange - Provide a range that you‚Äôre 80% confident will contain the actual value. - "

colnames(uec_responses) <- c("US December 2023 Recession?", "UT December 2023 Recession?",
                             "US Recession Probability", "UT Recession Probability",
                             "2024 CPI Forecast", "2024 CPI Range", "2025 CPI Forecast", "2025 CPI Range",
                             "2024 US UR Forecast", "2024 US UR Range", "2025 US UR Forecast", "2025 US UR Range",
                             "2024 GDP Forecast", "2024 GDP Range", "2025 GDP Forecast", "2025 GDP Range",
                             "2024 NPI Forecast", "2024 NPI Range", "2025 NPI Forecast", "2025 NPI Range",
                             "2024 UT UR Forecast", "2024 UT UR Range", "2025 UT UR Forecast", "2025 UT UR Range",
                             "2024 Emp Forecast", "2024 Emp Range", "2025 Emp Forecast", "2025 Emp Range",
                             "2024 Annual Pay Forecast", "2024 Annual Pay Range", "2025 Annual Pay Forecast", "2025 Annual Pay Range",
                             "2024 Taxable Sales Forecast", "2024 Taxable Sales Range", "2025 Taxable Sales Forecast", "2025 Taxable Sales Range",
                             "2024 HPI Forecast", "2024 HPI Range", "2025 HPI Forecast", "2025 HPI Range",
                             "2024 Population Forecast", "2024 Population Range", "2025 Population Forecast", "2025 Population Range")

uec_responses %<>%
  select(matches(".+"))


# Summarize data
uec_responses %<>% 
  mutate(`US December 2023 Recession?` = ifelse(`US December 2023 Recession?` == "Yes", 1, 0)) %>%
  mutate(`UT December 2023 Recession?` = ifelse(`UT December 2023 Recession?` == "Yes", 1, 0))


# fix messed up values
uec_responses$`2024 HPI Forecast`[6] <- -0.018
uec_responses[] <- lapply(uec_responses, as.numeric)

columns_to_fix <- colnames(uec_responses)[5:ncol(uec_responses)]
uec_responses[6, columns_to_fix] <- uec_responses[6, columns_to_fix] * 100
uec_responses[2, columns_to_fix] <- uec_responses[2, columns_to_fix] * 100


# actual summarization
uec_responses %<>%
  summarize(across(contains("?"), ~mean(., na.rm = T)),
            across(!contains("?"), ~median(., na.rm = T)))

# indicators
uec_dec23_forecasts <- uec_responses %>%
  select(contains("Forecast")) %>%
  pivot_longer(everything(), names_to = "indicator") %>%
  mutate(value = value / 100)



# March Forecasts
uec_mar24_forecasts <- tibble(
  indicator = uec_dec23_forecasts$indicator,
  value = c(3, 2.5,
            4, 4,
            2.3, 2,
            5.1, 5.1,
            2.9, 3,
            2, 1.8,
            4, 3.6,
            3.5, 3.7,
            2, 2.3,
            1.5, 1.5
  ) / 100
)

# date of responses for forecast date in app (insert in "YYYY-MM-DD" format)
uec_date <- as.Date("2024-03-01")
saveRDS(uec_date, paste0(app_folder_path, "uec_date.rds"))

# for consistent naming across all indicators
uec_forecasts <- uec_mar24_forecasts

# * Source Time Scale Availability Key ------------------------------------
# INDICATOR (Time Aggregation Scale for plot): source (details/scale availability)
# CPI (Quarterly): BLS history (monthly to 1970), S&P (quarterly), Moody's (quarterly), RAWG (yearly)
# USUR (Quarterly): BLS history (monthly to 1970), S&P (quarterly), Moody's (quarterly), RAWG (yearly)
# GDP (Quarterly): BEA history (quarterly to 1975-ish), S&P (quarterly), Moody's (quarterly), RAWG (yearly)
# NPI (Quarterly): BEA history (yearly to 1970 - Moody's provides more granular historical data), Moody's (quarterly - using it as history), RAWG (yearly)
# UTUR (Quarterly): BLS history (monthly to 1976), Moody's (quarterly), RAWG (yearly)
# Emp (Quarterly): BLS history (monthly to 1990 - aggregating to quarterly and adding Moody's supplement), Moody's (quarterly), RAWG (yearly)
# Annual Pay (Yearly): BLS & RAWG history (yearly to 1976 for RAWG, 2001 for BLS - USING RAWG HISTORY), RAWG (yearly)
# Taxable Sales (Yearly): Utah Tax Commission history (monthly available for "Recent" plot, but using yearly for forecast plot), RAWG (yearly)
# HPI (Quarterly): FHFA history (quarterly to 1980-ish), Moody's (quarterly), RAWG (yearly)
# POP (Yearly): Kem C. history (yearly to 1970), Moody's (yearly), RAWG (yearly)

# * Moody's API Function --------------------------------------------------------

# Reference
# https://github.com/moodysanalytics/databuffet-api-codesamples/tree/master/R
# https://github.com/moodysanalytics/databuffet-api-codesamples/blob/master/R/Single-Series.R

get.series <- function(mnemonic, accKey, encKey,freq="0",trans="0"){
apiCommand <- paste("series?m=",utils::URLencode(mnemonic),"&freq=",freq,"&trans=",trans,sep="")
  url <- paste("https://api.economy.com/data/v1/", apiCommand, sep="")
  print(url)
  timeStamp <- format(as.POSIXct(Sys.time()), "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  hashMsg   <- paste(accKey, timeStamp, sep="")
  signature <- hmac(encKey, hashMsg, "sha256")
  
  Sys.sleep(1)
  req <- httr::GET(url, httr::add_headers("AccessKeyId" = accKey,
                                          "Signature" = signature,
                                          "TimeStamp" = timeStamp))
  
  series <- jsonlite::fromJSON(httr::content(req, as="text"))
  return(series)
}

# * S&P Path Setup --------------------------------------------------------

# initialize directory containing files
s_p500_path <- "I:/Shared/dea/Forecasting/GlobalInsight/Data"

# retrieve file names
print("Sometimes getting S&P directory can take a while...")
SPG_path <- file.info(list.files(s_p500_path,
                                 full.names = T,
                                 pattern="xlsx"))

# extract dates from file names
SPG_dates <- str_remove(rownames(SPG_path), paste0(s_p500_path, "/st"))
str_sub(SPG_dates, 5, 4) <- "-" # hacky insert dash to be recognized as date
SPG_dates <- as.yearmon(str_sub(SPG_dates, end = -8)) # just retain year and month

# get most recent file by row name based on above logic
SPG_path <- rownames(SPG_path)[which.max(SPG_dates)]

# * RAWG Path Setup -------------------------------------------------------

# initialize directory containing files
rawg_path <- "I:/Shared/dea/Forecasting/06Indicators"

# retrieve names of all files (including both ecin and hist)
print("Sometimes getting RAWG directory can take a while...")
rawg_direc <- file.info(list.files(rawg_path, 
                                  full.names = T,
                                  pattern="xlsx")) %>%
  mutate(paths = row.names(.)) %>% # file name is the row indexing - make it its own column
  select(paths)

# reset index
row.names(rawg_direc) <- NULL


# reg express for ecin files
ecin_pattern <- "/ecin\\d{4} \\d{2}\\.xlsx$"
# filter files by desirable file name
ecin_paths <- rawg_direc[grep(ecin_pattern, rawg_direc$paths), ]

# extract dates from file names
ecin_dates <- str_remove(ecin_paths, paste0(rawg_path, "/ecin"))
ecin_dates <- str_replace(ecin_dates, " ", "-") # turn into date recognizable
ecin_dates <- as.yearmon(str_sub(ecin_dates, end = -6)) # just retain year and month

# only keep most recent Oct or Feb RAWG file (Richie)
feb_oct_ecins <- ecin_dates[as.integer(format(ecin_dates, "%m")) == 2 |
                              as.integer(format(ecin_dates, "%m")) == 10]
recent_ecin_date <- max(feb_oct_ecins)

# format back into filepath-recognizable format
# don't have the luxury of simplicity as in the S&P process due to more complex
# filtering by specific month
recent_ecin_date <- format(recent_ecin_date, "%Y-%m")
recent_ecin_date <- str_replace(recent_ecin_date, "-", " ")
ecin_path <- ecin_paths[grep(recent_ecin_date, ecin_paths)]

# access appropriate sheet
rawg_ecin_file <- read_excel(ecin_path, sheet = "sasout")

# * RAWG History Path Setup -----------------------------------------------

# filter files by desirable file name (using same directory as above so it's not called twice)
hist_paths <- rawg_direc[grep("history", rawg_direc$paths), ]

# extract dates from file names
hist_dates <- str_remove(hist_paths, paste0(rawg_path, "/history "))
hist_dates <- str_replace(hist_dates, " ", "-") # turn into date recognizable
hist_dates <- as.yearmon(str_sub(hist_dates, end = -6)) # just retain year and month


# get most recent file based on above logic
hist_path <- hist_paths[which.max(hist_dates)]

# access appropriate sheet
rawg_hist_file <- read_excel(hist_path, sheet = "rawg")

# RECESSION INDICATORS ----------------------------------------------------------

# * FRED Recession Dates ----------------------------------------------------

# retrieve recession dates
FRED_recession_dates <- map("USREC", fredr,
                            observation_start = start_date,
                            observation_end = today,
                            realtime_start =today,
                            realtime_end = today,
                            frequency = "q",
                            aggregation_method = "sum")

FRED_recession_dates <- FRED_recession_dates[[1]]

# clean up, standardize, and format
FRED_recession_dates %<>%
  as_tibble() %>%
  rename(Date = date, Recession = value) %>%
  select(Date, Recession) %>%
  mutate(Recession = ifelse(Recession > 0, 1, 0))


# date to chain dollars to
recession_graph_date_comparison <- FRED_recession_dates %>%
  filter(Recession == 1) %>%
  summarize(latest_date = max(Date)) %>%
  pull()

# * FRED GDP ----------------------------------------------------------------

# retrieve GDP data from FRED
FRED_gdp <- map("GDPC1", fredr,
                observation_start = start_date,
                observation_end = today,
                realtime_start =today,
                realtime_end = today,
                frequency = "q",
                aggregation_method = "avg")

FRED_gdp <- FRED_gdp[[1]]

# clean up, standardize, and format
FRED_gdp %<>%
  as_tibble() %>%
  rename(Date = date, GDP = value) %>%
  select(Date, GDP)

gdp_2020 <- FRED_gdp %>%
  filter(Date == recession_graph_date_comparison) %>%
  select(GDP) %>%
  pull()

FRED_gdp %<>%
  mutate(adj_GDP = (GDP * 100) / gdp_2020) %>%
    select(-GDP)

# * FRED Non-Farm Employment ------------------------------------------------

# retrieve employment data from FRED
FRED_emp <- map("PAYEMS", fredr,
                observation_start = start_date,
                observation_end = today,
                realtime_start =today,
                realtime_end = today,
                frequency = "q",
                aggregation_method = "avg")

FRED_emp <- FRED_emp[[1]]

# clean up and format
FRED_emp %<>%
  as_tibble() %>%
  rename(Date = date, Non_Farm_Emp = value) %>%
  select(Date, Non_Farm_Emp)

# get 2020 value for chaining
emp_2020 <- FRED_emp %>%
  filter(Date == recession_graph_date_comparison) %>%
  select(Non_Farm_Emp) %>%
  pull()

# standardize
FRED_emp %<>%
  mutate(adj_emp = (Non_Farm_Emp * 100) / emp_2020) %>%
    select(-Non_Farm_Emp)

# * FRED Disposable Personal Income -----------------------------------------

# retrieve income data from FRED
FRED_pers_income <- map("DSPIC96", fredr,
                        observation_start = start_date,
                        observation_end = today,
                        realtime_start =today,
                        realtime_end = today,
                        frequency = "q",
                        aggregation_method = "avg")

FRED_pers_income <- FRED_pers_income[[1]]

# clean up and format
FRED_pers_income %<>%
  as_tibble() %>%
  rename(Date = date, Personal_Income = value) %>%
  select(Date, Personal_Income)

# get 2020 value for chaining
pers_income_2020 <- FRED_pers_income %>%
  filter(Date == recession_graph_date_comparison) %>%
  select(Personal_Income) %>%
  pull()

# standardize
FRED_pers_income %<>%
  mutate(adj_pers_income = (Personal_Income * 100) / pers_income_2020) %>%
    select(-Personal_Income)

# * FRED PCE (data only back to 2007) ----------------------------------------------------------------

# retrieve PCE data from FRED
FRED_pce <- map("PCEC96", fredr,
                observation_start = start_date,
                observation_end = today,
                realtime_start =today,
                realtime_end = today,
                frequency = "q",
                aggregation_method = "avg")

FRED_pce <- FRED_pce[[1]]

# clean up and format
FRED_pce %<>%
  as_tibble() %>%
  rename(Date = date, PCE = value) %>%
  select(Date, PCE)

# get 2020 value for chaining
pce_2020 <- FRED_pce %>%
  filter(Date == recession_graph_date_comparison) %>%
  select(PCE) %>%
  pull()

# standardize
FRED_pce %<>%
  mutate(adj_pce = (PCE * 100) / pce_2020) %>%
    select(-PCE)

# * FRED Industrial Production ----------------------------------------------

# retrieve production data from FRED
FRED_indust_prod <- map("INDPRO", fredr,
                        observation_start = start_date,
                        observation_end = today,
                        realtime_start =today,
                        realtime_end = today,
                        frequency = "q",
                        aggregation_method = "avg")

FRED_indust_prod <- FRED_indust_prod[[1]]

# clean up and format
FRED_indust_prod %<>%
  as_tibble() %>%
  rename(Date = date, Production = value) %>%
  select(Date, Production)

# get 2020 value for chaining
indust_prod_2020 <- FRED_indust_prod %>%
  filter(Date == recession_graph_date_comparison) %>%
  select(Production) %>%
  pull()

# standardize
FRED_indust_prod %<>%
  mutate(adj_indust_prod = (Production * 100) / indust_prod_2020) %>%
    select(-Production)

# * Join data from all series and save --------------------------------------
rec_indicators <- FRED_recession_dates %>%
  full_join(FRED_gdp) %>%
  full_join(FRED_emp) %>%
  full_join(FRED_pers_income) %>%
  full_join(FRED_pce) %>%
  full_join(FRED_indust_prod)

# format as desired
colnames(rec_indicators) <- c("Date",
                              "Recession",
                              "Real GDP",
                              "Non-Farm Employment",
                              "Real Disposable Personal Income",
                              "Real PCE",
                              "Industrial Production")

# declutter
rm(FRED_gdp,
   FRED_emp,
   FRED_pers_income,
   FRED_pce,
   FRED_indust_prod)


# PLOT PREP

# get dates of recessions for geom_rects
recessions <- rec_indicators %>%
  select(Date, Recession) %>%
  drop_na %>%
  as_tibble()

# save recession dates rds
saveRDS(recessions, paste0(app_folder_path, "recession/recessions.rds"))

# save start date for app to see
saveRDS(start_date, paste0(app_folder_path, "recession/start_date.rds"))



# get last date of historical data
last_FRED_date <- rec_indicators$Date[nrow(rec_indicators)]

# determine if the last piece of historical data is a complete quarter
# we can't know what a quarter's indicator is until it is over
# catch quarterly-dated data overlapping on quarter
if(as.yearqtr(last_FRED_date) == quarter)
{
  rec_indicators %<>% slice(-n())
}



# format indicators for plotting
rec_indicators %<>%select(-Recession) %>%
  pivot_longer(cols = !Date,
               names_to = "indicator",
               values_to = "value") %>%
  as_tibble()

# factoring indicators makes their manual coloring in the plot more cooperative
rec_indicators$indicator <- factor(rec_indicators$indicator, levels = unique(rec_indicators$indicator))

# save rec_indicator rds
saveRDS(rec_indicators, paste0(app_folder_path, "recession/rec_indicators.rds"))

# CPI % CHANGE -----------------------------------------------------------------

# * BLS CPI Actuals -------------------------------------------------------

# note registered users can only request up to 20 years of data per query from BLS API, hence for loop
bls_start_year <- 1969 # to start % change calculations in 1970
cpi_hist <- NULL

# iterate through 20 year increments to pull all inflation data from 1969 - current
for(i in 0:2){
  payload <- list(
    'seriesid' = "CUSR0000SA0",
    'startyear' = bls_start_year + 20*i, # increase at 20 year increments
    'endyear' = bls_start_year + 19 + 20*i, # set end of range 19 years apart
    'registrationKey'= bls_api)
  
  response <- blsAPI(payload, return_data_frame = T)
  
  cpi_hist <- bind_rows(cpi_hist, response)
}


# format dates and values and organize
cpi_hist %<>%
  mutate(Date = as.Date(as.yearmon(paste(year, str_remove(period, "M"), sep = "-")))) %>%# create single date
  mutate(value = as.numeric(value)) %>%
  select(Date, value) %>%
  arrange(Date) %>% # arrange by date
  as_tibble()


# create and save CPI history table (to have access to monthly changes)
cpi_hist_table <- cpi_hist %>%
  mutate(change = (value - lag(value, 12)) / lag(value, 12)) %>%
  filter(year(Date) > bls_start_year) %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  select(Date, `CPI Actual` = change)

# save table
saveRDS(cpi_hist_table, paste0(app_folder_path, "cpi/cpi_hist_table.rds"))


# calculate 4 quarter % change
cpi_hist %<>%
  mutate(Date = as.Date(as.yearqtr(Date))) %>% # connect the month of each quarter to its appropriate quarter start date
  group_by(Date) %>%
  summarize(value = mean(value)) %>%
  mutate(change = (value - lag(value, 4)) / lag(value, 4))

# format for export
cpi_hist %<>%
  filter(year(Date) > bls_start_year) %>% # filter only on years with % change value
  mutate(source = "CPI Actual") %>%
  select(Date, source, change)

# * S&P CPI -----------------------------------------------

# *** S&P annual CPI -----------------------------------------------------------------

# this is for a display table only, not the plot
spg_cpia_table <- read_excel(SPG_path, sheet = "P&W1A", skip = 2)

# select data of interest
spg_cpia_table %<>%
  rename(id = DATE, description = `...2`) %>%
  filter(id == "PCCPI") %>%
  pivot_longer(cols = !c(id, description),
               names_to = "Year",
               values_to = "change") %>%
  select(Year, change)

# filter on the next 3 years and format for export
spg_cpia_table %<>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= year & Year < year + 3) %>%
  mutate(change = paste0(round(change, 1), "%")) %>%
  rename(`Calendar Year` = Year, `CPI Forecast` = change)

# save annual table
saveRDS(spg_cpia_table, paste0(app_folder_path, "cpi/spg_cpia_table.rds"))

# *** S&P quarterly CPI -------------------------------------------------------

# read in quarter-specific data
spg_cpi <- read_excel(SPG_path, sheet = "P&W1Q", skip = 2)

# select data of interest
spg_cpi %<>%
  rename(id = DATE, description = `...2`) %>%
  filter(id == "PCCPI") %>%
  pivot_longer(cols = !c(id, description),
               names_to = "Date",
               values_to = "change") %>%
  select(Date, change)

# some fun string manipulation to get Date formatted :)
spg_cpi %<>%
  mutate(Date = as.Date(paste0(sub(":", "-", substr(Date, 1, 5)), # first part of string changing : to -, leaving quarter alone
                               as.character((as.numeric(substr(Date, 6, 6)) * 3) - 2), # changes quarter to appropriate starting month of quarter
                               "-01"))) # adds arbitrary 1st of the month as the date

# filter on the next 3 years and format for export
spg_cpi %<>%
  filter(year(Date) >= year & year(Date) < year + 3) %>%
  mutate(source = "S&P Global Forecast") %>%
  select(Date, source, change)

# create and format quarterly table
spg_cpiq_table <- spg_cpi %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(change = paste0(round(change, 1), "%")) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select("Calendar Quarter" = Quarter,
         "CPI Forecast" = change)

# save table
saveRDS(spg_cpiq_table, paste0(app_folder_path, "cpi/spg_cpiq_table.rds"))


# change plot source data to standardized format
spg_cpi %<>%
  mutate(change = change / 100)

# * Moody's CPI -----------------------------------------------------------

# *** Initial Pull ------------------------------------------------------------
moody_cpi <- get.series("FCPIU.IUSA", ACC_KEY, ENC_KEY)

moody_cpi <- data.frame("CPI"=moody_cpi$data$data,
                  "Date"=seq(from=as.Date(moody_cpi$startDate),
                             to=as.Date(moody_cpi$endDate),
                             by="quarter")) %>%
  select(Date, CPI)

# *** Annual Table ------------------------------------------------------------
moody_cpia_table <- moody_cpi %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(CPI = mean(CPI)) %>%
  ungroup() %>%
  mutate(change = (CPI - lag(CPI, 1)) / lag(CPI, 1)) %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  filter(Year >= year & Year < year +3) %>%
  rename("Calendar Year" = Year,
         "CPI Forecast" = change)

# save annual table
saveRDS(moody_cpia_table, paste0(app_folder_path, "cpi/moody_cpia_table.rds"))

# *** Quarterly Table ---------------------------------------------------------

# fix quarters, calculate change, and filter by year
moody_cpi %<>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(Quarter = case_when(grepl("-07-01", Date) ~ Quarter - 0.25,
                             grepl("-10-01", Date) ~ Quarter - 0.25,
                             TRUE ~ Quarter)) %>%
  mutate(change = ((CPI-lag(CPI, 4)) / lag(CPI, 4))) %>%
  filter(year(Date) >= year & year(Date) < year + 3)

# create and format quarterly table
moody_cpiq_table <- moody_cpi %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select("Calendar Quarter" = Quarter,
         "CPI Forecast" = change)

# save table
saveRDS(moody_cpiq_table, paste0(app_folder_path, "cpi/moody_cpiq_table.rds"))


# format date to more familiar months, source, and organize
moody_cpi %<>%
  mutate(Date = as.Date(Quarter)) %>%
  mutate(source = "Moody's Analytics Forecast") %>%
  select(Date, source, change)

# * RAWG CPI ----------------------------------------------------------------
rawg_cpi <- rawg_ecin_file %>%
  rename(Date = `...1`) %>%
  select(Date, CPI) %>% # select appropriate column
  mutate(change = ((CPI - lag(CPI))/lag(CPI))) %>% # calculate change
  select(-CPI) %>%
  filter(Date >= year & Date < year + 3) # filter by year

# get date of forecast for descriptive timeframe in plot data
rawg_date <- as.yearmon(substr(ecin_path, 44, nchar(ecin_path) - 5), format = "%Y %m")
rawg_month <- format(rawg_date, "%B") # just in case
rawg_year <- format(rawg_date, "%Y") # just in case
rawg_date_string <- paste("RAWG", rawg_date)

rawg_cpi_table <- rawg_cpi %>%
  transmute(`Calendar Year` = Date,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_cpi_table, paste0(app_folder_path, "cpi/rawg_cpi_table.rds"))


# force 4 quarters into each year
rawg_cpi %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))

# standardize format and source
rawg_cpi %<>%
  mutate(Date = as.Date(Date)) %>%
  mutate(source = paste("RAWG", rawg_date)) %>%
  select(Date, source, change)

# * UEC CPI -----------------------------------------------------------------
uec_cpi <- tibble(
  Date = c(year,
           year + 1),
  change = c(uec_forecasts %>%
                filter(indicator == "2024 CPI Forecast") %>%
                select(value) %>%
                pull(),
             uec_forecasts %>%
                filter(indicator == "2025 CPI Forecast") %>%
                select(value) %>%
                pull()
             )
  )


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_cpi_table <- rbind(uec_cpi, c(year + 2, NA)) %>%
  transmute(`Calendar Year` = Date,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_cpi_table, paste0(app_folder_path, "cpi/uec_cpi_table.rds"))


# force 4 quarters into each year for plot data
uec_cpi %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))


# standardize format and source
uec_cpi %<>%
  mutate(Date = as.Date(Date)) %>%
  mutate(source = paste("Economic Council", format(uec_date, "%b %Y"))) %>%
  select(Date, source, change)

# * Blue Chip CPI ---------------------------------------------------------
if(include_blue_chip){
  blue_chip_cpi <- tibble(
    `Calendar Year` = c(year, year + 1, year + 2),
    `Forecast` = c(ifelse(is.na(cpi_current_year_estimate),
                               NA,
                               paste0(cpi_current_year_estimate, "%")
                               ),
                       ifelse(is.na(cpi_next_year_estimate),
                               NA,
                               paste0(cpi_next_year_estimate, "%")
                               ),
                       ifelse(is.na(cpi_two_year_estimate),
                               NA,
                               paste0(cpi_two_year_estimate, "%")
                               )
                       )
  )
}else{blue_chip_cpi <- NULL}

saveRDS(blue_chip_cpi, paste0(app_folder_path, "cpi/blue_chip_cpi.rds"))

# * Combine All CPI Data ----------------------------------------------------
all_cpi <- rbind(cpi_hist,
                 spg_cpi,
                 moody_cpi,
                 rawg_cpi,
                 uec_cpi)

# declutter
rm(cpi_hist,
   spg_cpi,
   moody_cpi,
   rawg_cpi,
   uec_cpi)


# back filling forecasts to avoid plot gaps
all_cpi <- forecast_backfill("CPI Actual", all_cpi)

# factoring indicators to make plot coloring more cooperative
all_cpi$source <- factor(all_cpi$source, levels = unique(all_cpi$source))

# save cpi rds
saveRDS(all_cpi, paste0(app_folder_path, "cpi/all_cpi.rds"))

# US UR ----------------------------------------------------

# * BLS US UR Actuals -------------------------------------------------------
bls_start_year <- 1970 # set start year
response <- NULL # define/clear up response
usur_hist <- NULL # define data frame

# bls call for loop
for(i in 0:2){
  payload <- list(
    'seriesid' = "LNS14000000",
    'startyear' = bls_start_year + 20*i, 
    'endyear' = bls_start_year + 19 + 20*i,
    'registrationKey'= bls_api)
  
  response <- blsAPI(payload, return_data_frame = T)
  
  usur_hist <- bind_rows(usur_hist, response)
}

# format dates and values and organize
usur_hist %<>%
  mutate(Date = as.Date(as.yearmon(paste(year, str_remove(period, "M"), sep = "-")))) %>%
  mutate(value = as.numeric(value)) %>%
  select(Date, value) %>%
  arrange(Date) %>%
  as_tibble()


# create USUR history table (to have access to monthly changes)
usur_hist_table <- usur_hist %>%
  mutate(`UR Actual` = paste0(value, "%")) %>%
  select(-value)

# save table
saveRDS(usur_hist_table, paste0(app_folder_path, "usur/usur_hist_table.rds"))


# calculate quarter averages
usur_hist %<>%
  mutate(Date = as.Date(as.yearqtr(Date))) %>% # connect the month of each quarter to its appropriate quarter start date
  group_by(Date) %>%
  summarize(value = mean(value)) %>%
  ungroup()

# format for export
usur_hist %<>%
  mutate(source = "Unemployment Rate Actual") %>%
  select(Date, source, value)

# * S&P US UR -----------------------------------------------

# *** S&P annual US UR -----------------------------------------------------------------

# this is for a display table only, not the plot
spg_usura_table <- read_excel(SPG_path, sheet = "Sup1A", skip = 2)

# select data of interest
spg_usura_table %<>%
  as_tibble %>%
  rename(id = DATE, description = `...2`) %>%
  filter(id == "RUC") %>%
  pivot_longer(cols = where(is.numeric), # necessary in case column value introduces something weird
               names_to = "Year",
               values_to = "value") %>%
  select(Year, value)

# filter on the next 3 years and format for export
spg_usura_table %<>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= year & Year < year + 3) %>%
  mutate(value = paste0(round(value, 1), "%")) %>%
  rename(`Calendar Year` = Year, `UR Forecast` = value)

# save annual table
saveRDS(spg_usura_table, paste0(app_folder_path, "usur/spg_usura_table.rds"))

# *** S&P quarterly US UR -------------------------------------------------------
spg_usur <- read_excel(SPG_path, sheet = "Sup1Q", skip = 2)

# select data of interest
spg_usur %<>%
  rename(id = DATE, description = `...2`) %>%
  filter(id == "RUC") %>%
  pivot_longer(cols = !c(id, description, `...3`),
               names_to = "Date",
               values_to = "value") %>%
  select(Date, value)

# some fun string manipulation to get Date formatted :)
spg_usur %<>%
  mutate(Date = as.Date(paste0(sub(":", "-", substr(Date, 1, 5)), # first part of string changing : to -, leaving quarter alone
                               as.character((as.numeric(substr(Date, 6, 6)) * 3) - 2), # changes quarter to appropriate starting month of quarter
                               "-01"))) # adds arbitrary 1st of the month as the date

# filter on the next 3 years and format for export
spg_usur %<>%
  filter(year(Date) >= year & year(Date) < year + 3) %>%
  mutate(source = "S&P Global Forecast") %>%
  select(Date, source, value)

# create and format quarterly table
spg_usurq_table <- spg_usur %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(value = paste0(round(value, 1), "%")) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select("Calendar Quarter" = Quarter,
         "UR Forecast" = value)

# save table
saveRDS(spg_usurq_table, paste0(app_folder_path, "usur/spg_usurq_table.rds"))

# * Moody's US UR -----------------------------------------------------------

# *** Initial Pull ------------------------------------------------------------
moody_usur <- get.series("FLBR.IUSA", ACC_KEY, ENC_KEY)

moody_usur <- data.frame("value"=moody_usur$data$data,
                        "Date"=seq(from=as.Date(moody_usur$startDate),
                                   to=as.Date(moody_usur$endDate),
                                   by="quarter"))

# fix quarters and filter by year
moody_usur %<>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(Quarter = case_when(grepl("-07-01", Date) ~ Quarter - 0.25,
                             grepl("-10-01", Date) ~ Quarter - 0.25,
                             TRUE ~ Quarter)) %>%
  filter(year(Date) >= year & year(Date) < year + 3)

# *** Annual Table ------------------------------------------------------------
moody_usura_table <- moody_usur %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize("UR Forecast" = paste0(round(mean(value), 1), "%")) %>%
  ungroup() %>%
  rename("Calendar Year" = Year)

# save annual table
saveRDS(moody_usura_table, paste0(app_folder_path, "usur/moody_usura_table.rds"))

# *** Quarterly Table ---------------------------------------------------------
moody_usurq_table <- moody_usur %>%
  mutate(value = paste0(round(value, 1), "%")) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select("Calendar Quarter" = Quarter,
         "UR Forecast" = value)

# save quarterly table
saveRDS(moody_usurq_table, paste0(app_folder_path, "usur/moody_usurq_table.rds"))


# format date to more familiar months, source, and organize
moody_usur %<>%
  mutate(Date = as.Date(Quarter)) %>%
  mutate(source = "Moody's Analytics Forecast") %>%
  select(Date, source, value)

# * RAWG US UR ---------------------------------------------------------------
rawg_usur <- rawg_ecin_file %>%
  rename(Date = `...1`, value = RUC) %>%
  select(Date, value) %>%
  filter(Date >= year & Date < year + 3)

rawg_usur_table <- rawg_usur %>%
  transmute(`Calendar Year` = Date,
            {{rawg_date_string}} := paste0(round(value, 1), "%"))

# save RAWG table
saveRDS(rawg_usur_table, paste0(app_folder_path, "usur/rawg_usur_table.rds"))


# force 4 quarters into each year
rawg_usur %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))

# standardize format and source
rawg_usur %<>%
  mutate(Date = as.Date(Date)) %>%
  mutate(source = paste("RAWG", rawg_date)) %>%
  select(Date, source, value)

# * UEC US UR ----------------------------------------------------------------
uec_usur <- tibble(
  Date = c(year,
           year + 1),
  value = c((uec_forecasts %>%
                filter(indicator == "2024 US UR Forecast") %>%
                select(value) %>%
                pull()) * 100,
             (uec_forecasts %>%
                filter(indicator == "2025 US UR Forecast") %>%
                select(value) %>%
                pull()) * 100
  )
)


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_usur_table <- rbind(uec_usur %>%
  transmute(`Calendar Year` = Date,
            Forecast = paste0(round(value, 1), "%")),
  c(year + 2, NA)
  )

saveRDS(uec_usur_table, paste0(app_folder_path, "usur/uec_usur_table.rds"))


# force 4 quarters into each year for plot data
uec_usur %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))


# standardize format and source
uec_usur %<>%
  mutate(Date = as.Date(Date)) %>%
  mutate(source = paste("Economic Council", format(uec_date, "%b %Y"))) %>%
  select(Date, source, value)

# * Blue Chip US UR -------------------------------------------------------
if(include_blue_chip){
  blue_chip_usur <- tibble(
    `Calendar Year` = c(year, year + 1, year + 2),
    `Forecast` = c(ifelse(is.na(ur_current_year_estimate),
                          NA,
                          paste0(ur_current_year_estimate, "%")
                          ),
                   ifelse(is.na(ur_next_year_estimate),
                          NA,
                          paste0(ur_next_year_estimate, "%")
                          ),
                   ifelse(is.na(ur_two_year_estimate),
                          NA,
                          paste0(ur_two_year_estimate, "%")
                          )
                   )
    )
}else{blue_chip_usur <- NULL}

saveRDS(blue_chip_usur, paste0(app_folder_path, "usur/blue_chip_usur.rds"))

# * Combine All US UR Data ---------------------------------------------------
all_usur <- rbind(usur_hist,
                 spg_usur,
                 moody_usur,
                 rawg_usur,
                 uec_usur)

# declutter
rm(usur_hist,
   spg_usur,
   moody_usur,
   rawg_usur,
   uec_usur)


# back filling forecasts to avoid plot gaps
all_usur <- forecast_backfill("Unemployment Rate Actual", all_usur)

# factoring for manual coloring scale
all_usur$source <- factor(all_usur$source, levels = unique(all_usur$source))

# save usur rds
saveRDS(all_usur, paste0(app_folder_path, "usur/all_usur.rds"))

# GDP % CHANGE ---------------------------------------------------------------------

# * BEA GDP Actuals (DON'T RUN THIS API CALL TWICE IN ONE MINUTE!)  --------
bea_search <- head(beaSearch("Real Gross Domestic Product, Chained Dollars", beaKey = bea_api))

# it tells you what API call to make - copy/paste from desired row
bea_api_call <- bea_search$apiCall[1]

# automate api arguments
table_name <- bea_search$TableID[1]
datasetname <- bea_search$DatasetName[1]
seriesCode <- bea_search$SeriesCode[1]
# try to match ?beaGet example

# for loop for each year
bea_start_year <- 1969 # to start % change calculations in 1970
gdp_hist <- NULL

for(i in bea_start_year:(year)){
  response <- beaGet(list('UserID' = bea_api, 
                          'Method' = 'GetData', 
                          'datasetname' = datasetname, 
                          'Frequency' = 'Q',
                          'TableName' = table_name,
                          'Year' = i))
  
  # check for empty data frame, indicating further data isn't available, and break so it doesn't crash
  if(nrow(response) == 0)
  {
    break
  }
  
  response %<>%
    as_tibble() %>%
    filter(SeriesCode == seriesCode) %>% # filter on GDP series
    select(starts_with("DataValue")) %>%
    pivot_longer(cols = everything(),
                 names_to = "Date",
                 values_to = "gdp") %>%
    select(Date, gdp)
  
  gdp_hist <- bind_rows(gdp_hist, response)
}

# format quarters into dates
gdp_hist %<>%
  mutate(Date = as.Date(as.yearqtr(substr(Date, 11, nchar(Date)))))

# calculate YoY (quarterly) change
gdp_hist %<>%
  mutate(gdp = gdp * 1000000) %>% # (came from query in millions)
  mutate(change = (gdp - lag(gdp, 4)) / lag(gdp, 4))

# filter only on years with % change value
gdp_hist %<>%
  filter(Date > bea_start_year)

# source history data
gdp_hist %<>%
  mutate(source = "GDP Actual % Change") %>%
  select(Date, source, gdp, change)

# * S&P GDP -----------------------------------------------------------------

# *** S&P annual GDP -----------------------------------------------------------------

# this is for a display table only, not the plot
spg_gdpa_table <- read_excel(SPG_path, sheet = "Sum4A", skip = 2)

# select data of interest
spg_gdpa_table %<>%
  rename(id = DATE, description = `...2`) %>%
  filter(id == "PCGDPR") %>%
  pivot_longer(cols = !c(id, description),
               names_to = "Year",
               values_to = "change") %>%
  select(Year, change)

# format year and filter on next 3 years
spg_gdpa_table %<>%
  mutate(Year = as.integer(Year)) %>%
  arrange(Year) %>%
  filter(Year >= year & Year < year + 3)

# format for export
spg_gdpa_table %<>%
  mutate(change = paste0(round(change, 1), "%")) %>%
  rename(`Calendar Year` = Year, `GDP Forecast` = change)

# save table
saveRDS(spg_gdpa_table, paste0(app_folder_path, "gdp/spg_gdpa_table.rds"))

# *** S&P quarterly GDP -------------------------------------------------------
spg_gdp <- read_excel(SPG_path, sheet = "Sum4Q", skip = 2)

# select data of interest
spg_gdp %<>%
  rename(id = DATE, description = `...2`) %>%
  filter(id == "PCGDPR") %>%
  pivot_longer(cols = !c(id, description),
               names_to = "Date",
               values_to = "change") %>%
  select(Date, change)

# some fun string manipulation to get Date formatted :)
spg_gdp %<>%
  mutate(Date = as.Date(paste0(sub(":", "-", substr(Date, 1, 5)), # first part of string changing : to -, leaving quarter alone
                               as.character((as.numeric(substr(Date, 6, 6)) * 3) - 2), # changes quarter to appropriate starting month of quarter
                               "-01"))) # adds arbitrary 1st of the month as the date

# filter on the next 3 years
spg_gdp %<>%
  arrange(Date) %>%
  filter(year(Date) >= year & year(Date) < year + 3)

spg_gdpq_table <- spg_gdp %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(change = paste0(round(change, 1), "%")) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select("Calendar Quarter" = Quarter, "GDP Forecast" = change)

# save quarterly table
saveRDS(spg_gdpq_table, paste0(app_folder_path, "gdp/spg_gdpq_table.rds"))


# format and source plot data
spg_gdp %<>%
  transmute(Date,
            source = "S&P Global Forecast",
            gdp = NA,
            change = change / 100)

# * Moody's GDP -------------------------------------------------------------

# *** Initial Pull --------------------------------------------------------
moody_gdp <- get.series("FGDP$.IUSA", ACC_KEY, ENC_KEY)

moody_gdp <- data.frame("GDP"=moody_gdp$data$data,
                         "Date"=seq(from=as.Date(moody_gdp$startDate),
                                    to=as.Date(moody_gdp$endDate),
                                    by="quarter")) %>%
  select(Date, GDP)

# *** Annual Table --------------------------------------------------------

# get mean GDPs for years from quarters
moody_gdpa_table <- moody_gdp %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(GDP = mean(GDP)) %>%
  ungroup() %>%
  mutate(change = (GDP - lag(GDP, 1)) / lag(GDP, 1)) %>% # calculate YoY quarterly changes
  filter(Year >= year & Year < year + 3) # filter to next three years

# format for export
moody_gdpa_table %<>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  mutate(GDP = dollar(GDP, accuracy = 0.01)) %>%
  rename("Calendar Year" = Year,
         "GDP Level" = GDP,
         "GDP Change" = change)

# save table
saveRDS(moody_gdpa_table, paste0(app_folder_path, "gdp/moody_gdpa_table.rds"))

# *** Quarterly Table -----------------------------------------------------

# fix quarters
moody_gdp %<>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(Quarter = case_when(grepl("-07-01", Date) ~ Quarter - 0.25,
                             grepl("-10-01", Date) ~ Quarter - 0.25,
                             TRUE ~ Quarter)) %>%
  mutate(change = (GDP - lag(GDP, 4)) / lag(GDP, 4)) %>% # calculate change
  filter(year(Date) >= year & year(Date) < year + 3) %>% # filter to next three years
  select(Quarter, GDP, change)

# create and format quarterly table
moody_gdpq_table <- moody_gdp %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  mutate(GDP = dollar(GDP, accuracy = 0.01)) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select(`Calendar Quarter` = Quarter,
         `GDP Level` = GDP,
         `GDP Change` = change)

# save table
saveRDS(moody_gdpq_table, paste0(app_folder_path, "gdp/moody_gdpq_table.rds"))


# source, format, and organize plot data
moody_gdp %<>%
  transmute(Date = as.Date(Quarter),
            source = "Moody's Analytics Forecast",
            gdp = NA,
            change)

# * RAWG GDP ----------------------------------------------------------------
rawg_gdp <- rawg_ecin_file %>%
  rename(Date = `...1`) %>%
  select(Date, GDPR) %>%
  mutate(change = (GDPR - lag(GDPR)) / lag(GDPR)) %>% # calculate change
  filter(Date >= year & Date < year + 3) # filter by year

rawg_gdp_table <- rawg_gdp %>%
  transmute(`Calendar Year` = Date,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_gdp_table, paste0(app_folder_path, "gdp/rawg_gdp_table.rds"))


# force 4 quarters into each year
rawg_gdp %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))

# source plot data
rawg_gdp %<>%
  transmute(Date = as.Date(Date),
            source = paste("RAWG", rawg_date),
            gdp = NA,
            change)

# * UEC GDP -----------------------------------------------------------------
uec_gdp <- tibble(
  Date = c(year,
           year + 1),
  change = c(uec_forecasts %>%
               filter(indicator == "2024 GDP Forecast") %>%
               select(value) %>%
               pull(),
             uec_forecasts %>%
               filter(indicator == "2025 GDP Forecast") %>%
               select(value) %>%
               pull()
             )
  )


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_gdp_table <- rbind(uec_gdp, c(year + 2, NA)) %>%
  transmute(`Calendar Year` = Date,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_gdp_table, paste0(app_folder_path, "gdp/uec_gdp_table.rds"))


# force 4 quarters into each year for plot data
uec_gdp %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))


# standardize format and source
uec_gdp %<>%
  transmute(Date = as.Date(Date),
            source = paste("Economic Council", format(uec_date, "%b %Y")),
            gdp = NA,
            change)

# * Blue Chip GDP ---------------------------------------------------------
if(include_blue_chip){
  blue_chip_gdp <- tibble(
    `Calendar Year` = c(year, year + 1, year + 2),
    `Forecast` = c(ifelse(is.na(gdp_current_year_estimate),
                          NA,
                          paste0(gdp_current_year_estimate, "%")
                          ),
                   ifelse(is.na(gdp_next_year_estimate),
                          NA,
                          paste0(gdp_next_year_estimate, "%")
                          ),
                   ifelse(is.na(gdp_two_year_estimate),
                          NA,
                          paste0(gdp_two_year_estimate, "%")
                          )
                   )
    )
}else{blue_chip_gdp <- NULL}

saveRDS(blue_chip_gdp, paste0(app_folder_path, "gdp/blue_chip_gdp.rds"))

# * Combine All GDP Data ----------------------------------------------------
all_gdp <- rbind(gdp_hist,
                 spg_gdp,
                 moody_gdp,
                 rawg_gdp,
                 uec_gdp)

# declutter
rm(gdp_hist,
   spg_gdp,
   moody_gdp,
   rawg_gdp,
   uec_gdp)


# back filling forecasts to avoid plot gaps
all_gdp <- forecast_backfill("GDP Actual % Change", all_gdp)

# factoring for manual color scale
all_gdp$source <- factor(all_gdp$source, levels = unique(all_gdp$source))

# save gdp rds
saveRDS(all_gdp, paste0(app_folder_path, "gdp/all_gdp.rds"))

# NPI % CHANGE ------------------------------------------------------------

# * Moody's NPI (History and Forecast) --------------------------------------

# *** Initial Pull --------------------------------------------------------
moody_npi <- get.series("FYPQ.UT", ACC_KEY, ENC_KEY)

moody_npi <- data.frame("NPI"=moody_npi$data$data,
                        "Date"=seq(from=as.Date(moody_npi$startDate),
                                   to=as.Date(moody_npi$endDate),
                                   by="quarter")) %>%
  transmute(Date, NPI = NPI * 1000000) %>% # convert from millions
  # fix quarters
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(Quarter = case_when(grepl("-07-01", Date) ~ Quarter - 0.25,
                             grepl("-10-01", Date) ~ Quarter - 0.25,
                             TRUE ~ Quarter)) %>%
  mutate(Date = as.Date(Quarter)) %>% # fix dates
  filter(year(Date) >= 1969) %>%
  mutate(change = (NPI - lag(NPI, 4)) / lag(NPI, 4)) %>%
  drop_na() # get rid of un-laggable dates

# *** Historical Data -----------------------------------------------------
npi_hist <- moody_npi %>%
  filter(Date < as.Date(quarter)) %>%
  transmute(Date,
            source = "Personal Income % Change",
            NPI,
            change)
  
# *** Quarterly Table -----------------------------------------------------

# create and format quarterly table
moody_npiq_table <- moody_npi %>%
  filter(year(Date) < year + 3 & Quarter >= quarter) %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  mutate(NPI = dollar(NPI, accuracy = 0.01)) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select(`Calendar Quarter` = Quarter,
         `PI Level` = NPI,
         `PI Change` = change)

# save table
saveRDS(moody_npiq_table, paste0(app_folder_path, "npi/moody_npiq_table.rds"))

# *** Annual Table --------------------------------------------------------
moody_npia_table <- moody_npi %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(NPI = mean(NPI)) %>%
  ungroup() %>%
  mutate(change = (NPI - lag(NPI, 1)) / lag(NPI, 1))

moody_npia_table %<>%
  filter(Year >= year & Year < year + 3) %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  mutate(NPI = dollar(NPI, accuracy = 0.01)) %>%
  rename(`Calendar Year` = Year,
         `PI Level` = NPI,
         `PI Change` = change)

# save annual table
saveRDS(moody_npia_table, paste0(app_folder_path, "npi/moody_npia_table.rds"))


# source and organize plot data
moody_npi %<>%
  filter(Quarter >= quarter & year(Date) < year + 3) %>%
  transmute(Date,
            source = "Moody's Analytics Forecast",
            NPI = NA,
            change)

# * RAWG NPI ----------------------------------------------------------------
rawg_npi <- rawg_ecin_file %>%
  rename(Date = `...1`) %>%
  select(Date, CYI) %>%
  mutate(change = (CYI - lag(CYI)) / lag(CYI)) %>%
  filter(Date >= year & Date < year + 3)

rawg_npi_table <- rawg_npi %>%
  transmute(`Calendar Year` = Date,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_npi_table, paste0(app_folder_path, "npi/rawg_npi_table.rds"))


# force 4 quarters into each year (take out when using yearly data)
rawg_npi %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))

# format and source plot data
rawg_npi %<>% # add below command for changing back to yearly data
  # mutate(Date = as.integer(Date)) %>%# bea only has annual data, so time frame is just years anyway - keeping things consistent
  transmute(Date = as.Date(Date),
            source = paste("RAWG", rawg_date),
            NPI = NA,
            change)

# * UEC NPI -----------------------------------------------------------------
uec_npi <- tibble(
  Date = c(year,
           year + 1),
  change = c(uec_forecasts %>%
               filter(indicator == "2024 NPI Forecast") %>%
               select(value) %>%
               pull(),
             uec_forecasts %>%
               filter(indicator == "2025 NPI Forecast") %>%
               select(value) %>%
               pull()
  )
)


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_npi_table <- rbind(uec_npi, c(year + 2, NA)) %>%
  transmute(`Calendar Year` = Date,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_npi_table, paste0(app_folder_path, "npi/uec_npi_table.rds"))


# force 4 quarters into each year for plot data
uec_npi %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))


# standardize format and source
uec_npi %<>%
  transmute(Date = as.Date(Date),
            source = paste("Economic Council", format(uec_date, "%b %Y")),
            NPI = NA,
            change)

# * Combine All NPI Data ----------------------------------------------------
all_npi <- rbind(npi_hist,
                 moody_npi,
                 rawg_npi,
                 uec_npi)

# declutter
rm(npi_hist,
   moody_npi,
   rawg_npi,
   uec_npi)


# back filling forecasts to avoid plot gaps
all_npi <- forecast_backfill("Personal Income % Change", all_npi)

# factoring for manual coloring
all_npi$source <- factor(all_npi$source, levels = unique(all_npi$source))

# save npi rds
saveRDS(all_npi, paste0(app_folder_path, "npi/all_npi.rds"))

# UT UR -------------------------------------------------------------------

# * BLS UT UR Actuals -------------------------------------------------------
bls_start_year <- 1970 # not calculating change this time
response <- NULL
utur_hist <- NULL

# bls call for loop
for(i in 0:2){
  payload <- list(
    'seriesid' = "LASST490000000000003",
    'startyear' = bls_start_year + 20*i, 
    'endyear' = bls_start_year + 19 + 20*i,
    'registrationKey'= bls_api)
  
  response <- blsAPI(payload, return_data_frame = T)
  
  utur_hist <- bind_rows(utur_hist, response)
}

# format dates and values and organize
utur_hist %<>%
  mutate(Date = as.Date(as.yearmon(paste(year, str_remove(period, "M"), sep = "-")))) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(source = "Unemployment Rate Actual") %>%
  select(Date, source, value) %>%
  arrange(Date) %>% # arrange in descending order
  as_tibble()


# create and save UTUR history table
utur_hist_table <- utur_hist %>%
  mutate(`UR Actual` = paste0(value, "%")) %>%
  select(-source, -value)

saveRDS(utur_hist_table, paste0(app_folder_path, "utur/utur_hist_table.rds"))


# calculate quarter averages
utur_hist %<>%
  mutate(Date = as.Date(as.yearqtr(Date))) %>% # connect the month of each quarter to its appropriate quarter start date
  group_by(Date) %>%
  summarize(value = mean(value)) %>%
  ungroup()

# format for export
utur_hist %<>%
  mutate(source = "Unemployment Rate Actual") %>%
  select(Date, source, value)

# * Moody's UT UR -----------------------------------------------------------

# *** Initial Pull ------------------------------------------------------------
moody_utur <- get.series("FLBR.UT", ACC_KEY, ENC_KEY)

moody_utur <- data.frame("value"=moody_utur$data$data,
                         "Date"=seq(from=as.Date(moody_utur$startDate),
                                    to=as.Date(moody_utur$endDate),
                                    by="quarter"))

# fix quarters and filter by year
moody_utur %<>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(Quarter = case_when(grepl("-07-01", Date) ~ Quarter - 0.25,
                             grepl("-10-01", Date) ~ Quarter - 0.25,
                             TRUE ~ Quarter)) %>%
  filter(year(Date) >= year & year(Date) < year + 3)

# *** Annual Table ------------------------------------------------------------
moody_utura_table <- moody_utur %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize("UR Forecast" = paste0(round(mean(value), 1), "%")) %>%
  ungroup() %>%
  rename("Calendar Year" = Year)

# save annual table
saveRDS(moody_utura_table, paste0(app_folder_path, "utur/moody_utura_table.rds"))

# *** Quarterly Table ---------------------------------------------------------
moody_uturq_table <- moody_utur %>%
  mutate(value = paste0(round(value, 1), "%")) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  select("Calendar Quarter" = Quarter,
         "UR Forecast" = value)

# save quarterly table
saveRDS(moody_uturq_table, paste0(app_folder_path, "utur/moody_uturq_table.rds"))


# format date to more familiar months, source, and organize
moody_utur %<>%
  mutate(Date = as.Date(Quarter)) %>%
  mutate(source = "Moody's Analytics Forecast") %>%
  select(Date, source, value)

# * RAWG UT UR ---------------------------------------------------------------
rawg_utur <- rawg_ecin_file %>%
  rename(Date = `...1`, value = UR) %>%
  select(Date, value) %>%
  filter(Date >= year & Date < year + 3)

rawg_utur_table <- rawg_utur %>%
  transmute(`Calendar Year` = Date,
            {{rawg_date_string}} := paste0(round(value, 1), "%"))

# save RAWG table
saveRDS(rawg_utur_table, paste0(app_folder_path, "utur/rawg_utur_table.rds"))


# force 4 quarters into each year
rawg_utur %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))

# format and source plot data
rawg_utur %<>%
  mutate(Date = as.Date(Date)) %>%
  mutate(source = paste("RAWG", rawg_date)) %>%
  select(Date, source, value)

# * UEC UT UR -----------------------------------------------------------------
uec_utur <- tibble(
  Date = c(year,
           year + 1),
  value = c((uec_forecasts %>%
               filter(indicator == "2024 UT UR Forecast") %>%
               select(value) %>%
               pull()) * 100,
            (uec_forecasts %>%
               filter(indicator == "2025 UT UR Forecast") %>%
               select(value) %>%
               pull()) * 100
  )
)


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_utur_table <- rbind(uec_utur %>%
                          transmute(`Calendar Year` = Date,
                                    Forecast = paste0(round(value, 1), "%")),
                        c(year + 2, NA)
                        )

saveRDS(uec_utur_table, paste0(app_folder_path, "utur/uec_utur_table.rds"))


# force 4 quarters into each year for plot data
uec_utur %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))


# standardize format and source
uec_utur %<>%
  mutate(Date = as.Date(Date)) %>%
  mutate(source = paste("Economic Council", format(uec_date, "%b %Y"))) %>%
  select(Date, source, value)

# * Combine All UT UR Data ---------------------------------------------------
all_utur <- rbind(utur_hist,
                  moody_utur,
                  rawg_utur,
                  uec_utur)

# declutter
rm(utur_hist,
   moody_utur,
   rawg_utur,
   uec_utur)


# back filling forecasts to avoid plot gaps
all_utur <- forecast_backfill("Unemployment Rate Actual", all_utur)

# factoring for manual coloring
all_utur$source <- factor(all_utur$source, levels = unique(all_utur$source))

# save utur rds
saveRDS(all_utur, paste0(app_folder_path, "utur/all_utur.rds"))

# UT EMP % CHANGE (measured in avg. monthly numbers for each year) --------

# * BLS UT Emp Actuals ------------------------------------------------------

# # The "SMS49000000000000001" series is adjusted, and only goes back to 1990.
# The "SMU49000000000000001" series is unadjusted, and goes back to 1970.
# There is a saved "both_emps.rds" showing the differences between the numbers,
# which are very much negligible, especially after YO change calculation (which is what we're after).
# To stick with seasonally adjusted, we'll use BLS's 1990-current adjusted data
# (which is identical to that of Moody's), and then add in Moody's adjusted 1969-1990)
# Sticking to adjusted for the sake of being consistent for the Council.

bls_start_year <- 1990
emp_hist <- NULL

for(i in 0:1){
  payload <- list(
    'seriesid' = "SMS49000000000000001", # this is seasonally adjusted
    'startyear' = bls_start_year + 20*i, # increase at 20 year increments
    'endyear' = bls_start_year + 19 + 20*i, # set end of range 19 years apart
    'registrationKey'= bls_api)

  response <- blsAPI(payload, return_data_frame = T)

  emp_hist <- bind_rows(emp_hist, response)
}

emp_hist %<>%
  mutate(Date = as.Date(as.yearmon(paste(year, str_remove(period, "M"), sep = "-")))) %>%
  mutate(emp = as.numeric(value)) %>%
  select(Date, emp) %>%
  arrange(Date) %>%
  mutate(emp = emp * 1000) %>% # use straightforward units (number currently in thousands)
  as_tibble()

# convert to quarterly data
emp_hist %<>%
  mutate(Date = as.Date(as.yearqtr(Date))) %>% # connect the month of each quarter to its appropriate quarter start date
  group_by(Date) %>%
  summarize(emp = round(mean(emp), 0)) %>%
  ungroup()

# calculate quarterly YoY change
emp_hist %<>%
  mutate(change = (emp - lag(emp, 4)) / lag(emp, 4)) %>%
  filter(year(Date) > bls_start_year)

# source and format history plot data
emp_hist %<>%
  transmute(Date,
            source = "Employment Actual % Change",
            emp,
            change)

## ADDING MOODY'S QUARTERLY DATA SINCE PLOT WILL BE AGGREGATED TO QUARTERLY ANYWAY
## TABLE WILL ALSO BE REPORTED QUARTERLY, WHICH IS ALREADY SIGNIFICANTLY MORE
## GRANULAR THAN THE PREVIOUS YEARLY SCALE.
## Since we're not reporting monthly data, essentially pointless to not just
## pull this data from Moody's anyway and leave out consulting BLS altogether
## like was done with NPI.

# * Moody's Emp -------------------------------------------------------------

# *** Initial Pull --------------------------------------------------------
moody_emp <- get.series("FET.UT", ACC_KEY, ENC_KEY)

moody_emp <- data.frame("emp"=moody_emp$data$data,
                        "Date"=seq(from=as.Date(moody_emp$startDate),
                                   to=as.Date(moody_emp$endDate),
                                   by="quarter")) %>%
  transmute(Date, emp = emp * 1000) # format to correct number

# fix quarters and dates
moody_emp %<>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(Quarter = case_when(grepl("-07-01", Date) ~ Quarter - 0.25,
                             grepl("-10-01", Date) ~ Quarter - 0.25,
                             TRUE ~ Quarter)) %>%
  mutate(Date = as.Date(Quarter)) %>%
  arrange(Date) %>%
  mutate(change = (emp - lag(emp, 4)) / lag(emp, 4)) %>%
  slice(-(1:4)) %>%
  as_tibble()

# *** Annual Table ------------------------------------------------------------
moody_empa_table <- moody_emp %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(emp = round(mean(emp), 0)) %>%
  ungroup() %>%
  mutate(change = (emp - lag(emp)) / lag(emp)) %>%
  filter(Year >= year & Year < year + 3)

# format table
moody_empa_table %<>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  rename("Calendar Year" = Year,
         "Employment Level" = emp,
         "Employment Change" = change)

# save table
saveRDS(moody_empa_table, paste0(app_folder_path, "emp/moody_empa_table.rds"))

# *** Finish Historical Data --------------------------------------------------
moody_emp_hist <- moody_emp %>%
  filter(year(Date) < 1991) %>%
  transmute(Date,
            source = "Employment Actual % Change",
            emp = round(emp, 0), # use only whole numbers
            change)

emp_hist <- rbind(moody_emp_hist, emp_hist)
# declutter
rm(moody_emp_hist)

# *** Quarterly Table ---------------------------------------------------------

# filter, source, and organize plot data
moody_emp %<>%
  filter(year(Date) >= year & year(Date) < year + 3) %>%
  mutate(source = "Moody's Analytics Forecast")

# create and format table
moody_empq_table <- moody_emp %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  mutate(emp = round(emp, 0)) %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  select(`Calendar Quarter` = Quarter,
         `Employment Level` = emp,
         `Employment Change` = change)

# save quarterly table
saveRDS(moody_empq_table, paste0(app_folder_path, "emp/moody_empq_table.rds"))


# format moody_emp plot data
moody_emp %<>%
  transmute(Date,
            source,
            emp = NA,
            change)
  
# * RAWG Emp ----------------------------------------------------------------
rawg_emp <- rawg_ecin_file %>%
  rename(Date = `...1`) %>%
  select(Date, CYEMP) %>%
  mutate(change = (CYEMP - lag(CYEMP)) / lag(CYEMP)) %>%
  filter(Date >= year & Date < year + 3) # filter by year

rawg_emp_table <- rawg_emp %>%
  transmute(`Calendar Year` = Date,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_emp_table, paste0(app_folder_path, "emp/rawg_emp_table.rds"))


# force 4 quarters into each year
rawg_emp %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))

# source plot data
rawg_emp %<>%
  transmute(Date = as.Date(Date),
            source = paste("RAWG", rawg_date),
            emp = NA,
            change)

# * UEC Emp -----------------------------------------------------------------
uec_emp <- tibble(
  Date = c(year,
           year + 1),
  change = c(uec_forecasts %>%
               filter(indicator == "2024 Emp Forecast") %>%
               select(value) %>%
               pull(),
             uec_forecasts %>%
               filter(indicator == "2025 Emp Forecast") %>%
               select(value) %>%
               pull()
             )
  )


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_emp_table <- rbind(uec_emp, c(year + 2, NA)) %>%
  transmute(`Calendar Year` = Date,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_emp_table, paste0(app_folder_path, "emp/uec_emp_table.rds"))


# force 4 quarters into each year for plot data
uec_emp %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))


# standardize format and source
uec_emp %<>%
  transmute(Date = as.Date(Date),
            source = paste("Economic Council", format(uec_date, "%b %Y")),
            emp = NA,
            change)

# * Combine All Emp Data ----------------------------------------------------
all_emp <- rbind(emp_hist,
                 moody_emp,
                 rawg_emp,
                 uec_emp)

# declutter
rm(emp_hist,
   moody_emp,
   rawg_emp,
   uec_emp)


# back filling forecasts to avoid plot gaps
all_emp <- forecast_backfill("Employment Actual % Change", all_emp)

# factoring for manual coloring
all_emp$source <- factor(all_emp$source, levels = unique(all_emp$source))

# save emp rds
saveRDS(all_emp, paste0(app_folder_path, "emp/all_emp.rds"))

# AVG ANNUAL PAY ----------------------------------------------------------

# * RAWG Annual Pay Actuals ---------------------------------------

# Explanation for choosing RAWG historical numbers over BLS is in section at end of script

# get RAWG pay history data (back to 1976)
pay_hist <- rawg_hist_file %>%
  rename(Year = `YR`, pay = CYAVW) %>%
  select(Year, pay) %>%
  filter(Year != 0 & !is.na(pay)) %>%
  as_tibble()

# calculate historical change
pay_hist %<>%
  mutate(change = (pay - lag(pay)) / lag(pay)) %>%
  filter(complete.cases(.) & Year < year) # drop NAs and limit "history" to before current year

# format and organize
pay_hist %<>%
  transmute(Year,
            source = "Average Annual Pay % Change",
            pay,
            change)

# * RAWG Pay ----------------------------------------------------------------

# RAWG historical file contains these values anyway, but we use this file because
# the historical file could be subject to forecast improvements more often than
# this one, which is updated only when RAWG meets semi-annually based on file
# selection logic in this script's setup.
rawg_pay <- rawg_ecin_file %>%
  rename(Year = `...1`) %>%
  select(Year, Level = CYAVW) %>%
  mutate(change = (Level - lag(Level)) / lag(Level)) %>%
  filter(Year >= year & Year < year + 3)

rawg_pay_table <- rawg_pay %>%
  transmute(`Calendar Year` = Year,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_pay_table, paste0(app_folder_path, "pay/rawg_pay_table.rds"))


rawg_pay %<>%
  transmute(Year,
            source = paste("RAWG", rawg_date),
            pay = NA,
            change)

# * UEC Pay ---------------------------------------------------------------
uec_pay <- tibble(
  Year = c(year,
           year + 1),
  change = c(uec_forecasts %>%
               filter(indicator == "2024 Annual Pay Forecast") %>%
               select(value) %>%
               pull(),
             uec_forecasts %>%
               filter(indicator == "2025 Annual Pay Forecast") %>%
               select(value) %>%
               pull()
             )
  )


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_pay_table <- rbind(uec_pay, c(year + 2, NA)) %>%
  transmute(`Calendar Year` = Year,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_pay_table, paste0(app_folder_path, "pay/uec_pay_table.rds"))


# standardize format and source
uec_pay %<>%
  transmute(Year,
            source = paste("Economic Council", format(uec_date, "%b %Y")),
            pay = NA,
            change)

# * Combine All Annual Pay Data -------------------------------------------
all_pay <- rbind(pay_hist,
                 rawg_pay,
                 uec_pay)

rm(pay_hist,
   rawg_pay,
   uec_pay)


# change "Year" to "Date" to work with function
colnames(all_pay) <- c("Date", "source", "pay", "change")

# back filling forecasts to avoid plot gaps
all_pay <- forecast_backfill("Average Annual Pay % Change", all_pay)

# change "Date" back to "Year" to remind me I'm working only with years
colnames(all_pay) <- c("Year", "source", "pay", "change")

# factoring for manual scale
all_pay$source <- factor(all_pay$source, levels = unique(all_pay$source))

# save pay rds
saveRDS(all_pay, paste0(app_folder_path, "pay/all_pay.rds"))

# UT TAXABLE SALES % CHANGE -----------------------------------------------

# * Utah Tax Commission Monthly Actuals -----------------------------------

# url for page of downloads
tax_page <- read_html("https://tax.utah.gov/econstats/sales")


# get links using css
monthly_links <- tax_page %>% html_elements(css = "#monthlybr-sales-tab a")

# obtain all relevant link file names
monthly_anchors <- NULL
inside_quote_pattern <- '"([^"]*)"'
for(link in monthly_links)
{
  current_file <- stringr::str_extract(as.character(link), inside_quote_pattern)
  current_file <- substr(current_file, 2, nchar(current_file) - 1)
  monthly_anchors <- c(monthly_anchors, current_file)
}


# select monthly sales history anchor file
monthly_sales_anchor <- monthly_anchors[grep("historical", monthly_anchors)]

# download file
download.file(url = paste0("https://tax.utah.gov", monthly_sales_anchor), destfile = "monthly_sales_hist.xlsx", mode = "wb")

# read in file
monthly_sales_hist <- read_excel("monthly_sales_hist.xlsx",
                               sheet = "Table 1",
                               skip = 5)

# add up all categories for each month
monthly_sales_hist %<>%
  mutate(Date = as.Date(Month)) %>%
  group_by(Date) %>%
  summarize(sales = sum(`Taxable Sales`)) %>%
  ungroup()

# calculate change
monthly_sales_hist %<>%
  mutate(change = (sales - lag(sales, 12)) / lag(sales, 12)) %>%
  filter(!is.na(change))

# save monthly plot data
saveRDS(monthly_sales_hist, paste0(app_folder_path, "sales/monthly_sales_hist.rds"))

# * Utah Tax Commission Annual Actuals ------------------------------------

# *** Pull In Old Sales Data ----------------------------------------------

# access all links using css
annual_links <- tax_page %>% html_elements(css = "#annualbr-sales-tab a")

# obtain all relevant link file names
annual_anchors <- NULL
inside_quote_pattern <- '"([^"]*)"'
for(link in annual_links)
{
  current_file <- stringr::str_extract(as.character(link), inside_quote_pattern)
  current_file <- substr(current_file, 2, nchar(current_file) - 1)
  annual_anchors <- c(annual_anchors, current_file)
}


# select old sales annual history anchor file
old_sales_anchor <- annual_anchors[grep("1978-1997-annual-sales-sic", annual_anchors)]

# download file
download.file(url = paste0("https://tax.utah.gov", old_sales_anchor), destfile = "old_sales.xlsx", mode = "wb")

# read in file
old_sales <- read_excel("old_sales.xlsx",
                        sheet = "State Major SIC Category")

# combine sales across all categories for each year
old_sales %<>%
  group_by(Year) %>%
  summarize(sales = sum(`Taxable Sales`)) %>%
  ungroup()

# *** Pull In Recent Sales Data -------------------------------------------

# select recent sales history anchor file
recent_sales_anchor <- annual_anchors[grep("historical", annual_anchors)]

# download file
download.file(url = paste0("https://tax.utah.gov", recent_sales_anchor), destfile = "recent_sales.xlsx", mode = "wb")

# read in file
recent_sales <- read_excel("recent_sales.xlsx",
                        sheet = "Table 2TR", skip = 5)

# keep only totals of each year
recent_sales %<>% 
  rename(category = `Economic Sector (NAICS Code)`) %>% 
  filter(category == "STATE TOTAL") %>% 
  select(-category)

# pivot on year
recent_sales %<>% 
  pivot_longer(cols = everything(),
               names_to = "Year",
               values_to = "sales") %>% 
  mutate(Year = as.numeric(str_remove(Year, "CY")))

# *** Combine Annual Sales Histories --------------------------------------
sales_hist <- rbind(old_sales, recent_sales)

# calculate change
sales_hist %<>%
  mutate(change = (sales - lag(sales)) / lag(sales)) %>%
  filter(!is.na(change))


# create and format history table
annual_sales_hist_table <- sales_hist %>%
  mutate(sales = dollar(sales)) %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  select(Date = Year,
         `Taxable Sales Actual` = sales,
         `Taxable Sales Change` = change)

# save table
saveRDS(annual_sales_hist_table, paste0(app_folder_path, "sales/annual_sales_hist_table.rds"))


# finish formatting annual historical plot data
sales_hist %<>%
  transmute(Year,
            source = "Taxable Sales Actual % Change",
            sales,
            change)

# * RAWG Taxable Sales ----------------------------------------------------
rawg_tax_sales <- rawg_ecin_file %>%
  rename(Year = `...1`) %>%
  select(Year, Level = TTSAL) %>%
  mutate(Level = Level * 1000000) %>%
  mutate(change = (Level - lag(Level)) / lag(Level)) %>%
  filter(Year >= year & Year < year + 3)

# # create and format RAWG taxable sales table
# rawg_tax_sales_table <- rawg_tax_sales %>%
#   mutate(Level = dollar(Level),
#          change = percent(change, accuracy = 0.1)) %>%
#   select(`Calendar Year` = Year,
#          Level,
#          Change = change)

rawg_tax_sales_table <- rawg_tax_sales %>%
  transmute(`Calendar Year` = Year,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_tax_sales_table, paste0(app_folder_path, "sales/rawg_tax_sales_table.rds"))


rawg_tax_sales %<>%
  transmute(Year,
            source = paste("RAWG", rawg_date),
            sales = NA,
            change)

# * UEC Taxable Sales -----------------------------------------------------
uec_tax_sales <- tibble(
  Year = c(year,
           year + 1),
  change = c(uec_forecasts %>%
               filter(indicator == "2024 Taxable Sales Forecast") %>%
               select(value) %>%
               pull(),
             uec_forecasts %>%
               filter(indicator == "2025 Taxable Sales Forecast") %>%
               select(value) %>%
               pull()
  )
)


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_tax_sales_table <- rbind(uec_tax_sales, c(year + 2, NA)) %>%
  transmute(`Calendar Year` = Year,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_tax_sales_table, paste0(app_folder_path, "sales/uec_tax_sales_table.rds"))


# standardize format and source
uec_tax_sales %<>%
  transmute(Year,
            source = paste("Economic Council", format(uec_date, "%b %Y")),
            sales = NA,
            change)

# * Combine All Taxable Sales Data ----------------------------------------
all_taxable_sales <- rbind(sales_hist,
                           rawg_tax_sales,
                           uec_tax_sales)

# declutter
rm(sales_hist,
   rawg_tax_sales,
   uec_tax_sales)


# change "Year" to "Date" to work with function
colnames(all_taxable_sales) <- c("Date", "source", "sales", "change")

# back filling forecasts to avoid plot gaps
all_taxable_sales <- forecast_backfill("Taxable Sales Actual % Change", all_taxable_sales)

# change "Date" back to "Year" to remind me I'm working only with years
colnames(all_taxable_sales) <- c("Year", "source", "sales", "change")

# factoring for manual scale
all_taxable_sales$source <- factor(all_taxable_sales$source, levels = unique(all_taxable_sales$source))

# save taxable sales rds
saveRDS(all_taxable_sales, paste0(app_folder_path, "sales/all_taxable_sales.rds"))

# UT HPI % CHANGE ---------------------------------------------------------

# * FHFA HPI Actuals ------------------------------------------------------
fhfa_page <- read_html("https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx#qat")

# get links with css
hpi_page_links <- fhfa_page %>% html_elements(css = ".ms-rteTableOddCol-4 a")

# obtain all link file names
hpi_anchors <- NULL
inside_quote_pattern <- '"([^"]*)"'
for(link in hpi_page_links)
{
  current_file <- stringr::str_extract(as.character(link), inside_quote_pattern)
  current_file <- substr(current_file, 2, nchar(current_file) - 1)
  hpi_anchors <- c(hpi_anchors, current_file)
}

# get desired anchor/url
hpi_url <- hpi_anchors[grep("HPI_AT_state.csv", hpi_anchors, ignore.case = TRUE)]

# download csv
download.file(url = paste0("https://www.fhfa.gov", hpi_url), destfile = "hpi_hist.csv", mode = "wb")

# read in csv file
hpi_hist <- read_csv("hpi_hist.csv", col_names = c("state", "Year", "Quarter", "hpi"))

# clean up data frame
hpi_hist %<>%
  filter(state =="UT") %>%
  select(-state)

# create Date column
hpi_hist %<>%
  mutate(Date = as.Date(as.yearqtr(paste0(Year, " Q", Quarter)))) %>%
  select(Date, hpi)

# calculate change
hpi_hist %<>%
  mutate(change = (hpi - lag(hpi, 4)) / lag(hpi, 4)) %>%
  filter(complete.cases(.))

# format and organize history data
hpi_hist %<>%
  transmute(Date,
            source = "HPI Actual % Change",
            hpi,
            change)

# * Moody's HPI -----------------------------------------------------------

# *** Initial Pull --------------------------------------------------------
moody_hpi <- get.series("FXHOFHOPI.UT", ACC_KEY, ENC_KEY)

moody_hpi <- data.frame("hpi"=moody_hpi$data$data,
                        "Date"=seq(from=as.Date(moody_hpi$startDate),
                                   to=as.Date(moody_hpi$endDate),
                                   by="quarter"))

# *** Annual Table --------------------------------------------------------

# get mean HPIs for years from quarters and calculate change
moody_hpia_table <- moody_hpi %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(hpi = mean(hpi)) %>%
  ungroup() %>%
  mutate(change = (hpi - lag(hpi)) / lag(hpi)) %>%
  filter(Year >= year & Year < year + 3)

# format for export
moody_hpia_table %<>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  mutate(hpi = round(hpi, 1)) %>%
  rename("Calendar Year" = Year,
         "HPI Level" = hpi,
         "HPI Change" = change)

# save table
saveRDS(moody_hpia_table, paste0(app_folder_path, "hpi/moody_hpia_table.rds"))

# *** Quarterly Table -----------------------------------------------------

# fix quarters
moody_hpi %<>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  mutate(Quarter = case_when(grepl("-07-01", Date) ~ Quarter - 0.25,
                             grepl("-10-01", Date) ~ Quarter - 0.25,
                             TRUE ~ Quarter))

# calculate change and filter
moody_hpi %<>%
  mutate(change = (hpi - lag(hpi, 4)) / lag(hpi, 4)) %>%
  filter(year(Date) >= year & year(Date) < year + 3) %>%
  select(Quarter, hpi, change)

# create and format quarterly table
moody_hpiq_table <- moody_hpi %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  mutate(hpi = round(hpi, 1)) %>%
  mutate(Quarter = as.character(Quarter)) %>% # so it saves correctly
  rename(`Calendar Quarter` = Quarter,
         `HPI Level` = hpi,
         `HPI Change (YoY)` = change)

# save table
saveRDS(moody_hpiq_table, paste0(app_folder_path, "hpi/moody_hpiq_table.rds"))


# source, format, and organize plot data
moody_hpi %<>%
  transmute(Date = as.Date(Quarter),
            source = "Moody's Analytics Forecast",
            hpi = NA,
            change)

# * RAWG HPI --------------------------------------------------------------
rawg_hpi <- rawg_ecin_file %>%
  rename(Date = `...1`) %>%
  select(Date, HPI) %>%
  mutate(change = (HPI - lag(HPI)) / lag(HPI)) %>%
  filter(Date >= year & Date < year + 3) %>% # filter by year
  select(Date, change)

rawg_hpi_table <- rawg_hpi %>%
  transmute(`Calendar Year` = Date,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_hpi_table, paste0(app_folder_path, "hpi/rawg_hpi_table.rds"))

# force 4 quarters into each year
rawg_hpi %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))

# source plot data
rawg_hpi %<>%
  transmute(Date = as.Date(Date),
            source = paste("RAWG", rawg_date),
            hpi = NA,
            change)

# * UEC HPI ---------------------------------------------------------------
uec_hpi <- tibble(
  Date = c(year,
           year + 1),
  change = c(uec_forecasts %>%
               filter(indicator == "2024 HPI Forecast") %>%
               select(value) %>%
               pull(),
             uec_forecasts %>%
               filter(indicator == "2025 HPI Forecast") %>%
               select(value) %>%
               pull()
             )
  )


# rbinding NA is included so forecast summary table flows smoothly in app script
uec_hpi_table <- rbind(uec_hpi, c(year + 2, NA)) %>%
  transmute(`Calendar Year` = Date,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_hpi_table, paste0(app_folder_path, "hpi/uec_hpi_table.rds"))


# force 4 quarters into each year for plot data
uec_hpi %<>%
  mutate(Date = as.yearqtr(Date)) %>%
  slice(rep(row_number(), each = 4)) %>%
  mutate(Date = case_when(row_number() %% 4 == 1 ~ Date,
                          row_number() %% 4 == 2 ~ Date + 0.25,
                          row_number() %% 4 == 3 ~ Date + 0.5,
                          row_number() %% 4 == 0 ~ Date + 0.75))


# standardize format and source
uec_hpi %<>%
  transmute(Date = as.Date(Date),
            source = paste("Economic Council", format(uec_date, "%b %Y")),
            hpi = NA,
            change)

# * Combine All HPI Data --------------------------------------------------
all_hpi <- rbind(hpi_hist,
                 moody_hpi,
                 rawg_hpi,
                 uec_hpi)

# declutter
rm(hpi_hist,
   moody_hpi,
   rawg_hpi,
   uec_hpi)


# back filling forecasts to avoid plot gaps
all_hpi <- forecast_backfill("HPI Actual % Change", all_hpi)

# factoring for manual coloring scale
all_hpi$source <- factor(all_hpi$source, levels = unique(all_hpi$source))

# save pay rds
saveRDS(all_hpi, paste0(app_folder_path, "hpi/all_hpi.rds"))

# UT POP % CHANGE ---------------------------------------------------------

# * Kem C. Gardner UT Pop Actuals -----------------------------------------

# url for page of downloads
kem_page <- read_html("https://gardner.utah.edu/utah-economy/data-downloads-utah-economy/")

# access all links using xpath
pop_page_links <- kem_page %>% html_nodes(xpath = "//article/*/h2//a")

# obtain all link file names
pop_anchors <- NULL
inside_quote_pattern <- '"([^"]*)"'
for(link in pop_page_links)
{
  current_file <- stringr::str_extract(as.character(link), inside_quote_pattern)
  current_file <- substr(current_file, 2, nchar(current_file) - 1)
  pop_anchors <- c(pop_anchors, current_file)
}

# select specific anchor of interest
# (this is assumes they upload available data by the time you run this script)
pop_anchors <- pop_anchors[grep(as.character(year), pop_anchors)]
pop_url <- pop_anchors[grep("table", pop_anchors, ignore.case = TRUE)]

# download file
download.file(url = pop_url, destfile = "pop_hist.xlsx", mode = "wb")

# read in file
pop_hist <- read_excel("pop_hist.xlsx", sheet = "2.1")

# drop source info in file and filter
pop_hist %<>% 
  filter(grepl("^\\d+$", Year)) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 1970)

# remove new line characters inside column names
colnames(pop_hist) <- sub("\r\n", " ", colnames(pop_hist))


# format plot data
pop_hist %<>%
    transmute(Year,
              source = "Population Actual % Change",
              pop = `July 1st Population`,
              change = `Annual Percent Change`)

# * Moody's Pop -----------------------------------------------------------
moody_pop <- get.series("FPOPCA.UT", ACC_KEY, ENC_KEY)

moody_pop <- data.frame("pop"=moody_pop$data$data * 1000,
                        "Date"=seq(from=as.Date(moody_pop$startDate),
                                   to=as.Date(moody_pop$endDate),
                                   by="year"))

moody_pop %<>%
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>% # format year
  select(Year, pop) %>%
  mutate(change = (pop - lag(pop)) / lag(pop)) %>% # calculate change
  filter(Year >= year & Year < year + 3)


# create and format Moody's population table
moody_pop_table <- moody_pop %>%
  mutate(change = percent(change, accuracy = 0.1)) %>%
  select(`Calendar Year` = Year,
         `Population Level` = pop,
         `Population Change` = change)

# save table
saveRDS(moody_pop_table, paste0(app_folder_path, "pop/moody_pop_table.rds"))


# format Moody's population plot data
moody_pop %<>%
  transmute(Year,
            source = "Moody's Analytics Forecast",
            pop = NA, # add this column for column consistency (using pop in tooltip later)
            change)

# * RAWG Pop --------------------------------------------------------------
rawg_pop <- rawg_ecin_file %>%
  rename(Year = `...1`) %>%
  select(Year, POP) %>%
  mutate(POP = POP * 1000) %>%
  mutate(change = (POP - lag(POP)) / lag(POP)) %>%
  filter(Year >= year & Year < year + 3) # filter by year

rawg_pop_table <- rawg_pop %>%
  transmute(`Calendar Year` = Year,
            {{rawg_date_string}} := percent(change, accuracy = 0.1))

# save RAWG table
saveRDS(rawg_pop_table, paste0(app_folder_path, "pop/rawg_pop_table.rds"))

# source plot data
rawg_pop %<>%
  transmute(Year,
            source = paste("RAWG", rawg_date),
            pop = NA, # add this column for column consistency (using pop in tooltip later)
            change)

# * UEC Pop ---------------------------------------------------------------
uec_pop <- tibble(
  Year = c(year,
           year + 1,
           year + 2),
  source = paste("Economic Council", format(uec_date, "%b %Y")),
  pop = NA,
  change = c(uec_forecasts %>%
               filter(indicator == "2024 Population Forecast") %>%
               select(value) %>%
               pull(),
             uec_forecasts %>%
                filter(indicator == "2025 Population Forecast") %>%
                select(value) %>%
                pull(),
             NA)
  )

uec_pop_table <- uec_pop %>%
  transmute(`Calendar Year` = Year,
            Change = percent(change, accuracy = 0.1))

saveRDS(uec_pop_table, paste0(app_folder_path, "pop/uec_pop_table.rds"))

# * Gardner Unofficial Pop ------------------------------------------------
gardner_pop <- tibble(
  Year = c(year,
           year + 1,
           year + 2),
  source = paste("Gardner Institute Unofficial Estimate", date_received),
  pop = NA, # add this column for column consistency (using pop in tooltip later)
  change = c(current_year_estimate,
             next_year_estimate,
             two_year_estimate)
)

gardner_pop_table <- gardner_pop %>%
  transmute(`Calendar Year` = Year,
            Change = percent(change, accuracy = 0.1))

saveRDS(gardner_pop_table, paste0(app_folder_path, "pop/gardner_pop_table.rds"))

# * Combine All Pop Data --------------------------------------------------
all_pop <- rbind(pop_hist,
                 moody_pop,
                 rawg_pop,
                 uec_pop)
if(include_gardner){all_pop <- rbind(all_pop, gardner_pop)}

# declutter
rm(pop_hist,
   moody_pop,
   rawg_pop,
   gardner_pop)


# change "Year" to "Date" to work with function
colnames(all_pop) <- c("Date", "source", "pop", "change")

# back filling forecasts to avoid plot gaps
all_pop <- forecast_backfill("Population Actual % Change", all_pop)

# change "Date" back to "Year" to remind me I'm working only with years
colnames(all_pop) <- c("Year", "source", "pop", "change")

# factoring for manual scale
all_pop$source <- factor(all_pop$source, levels = unique(all_pop$source))

# save pay rds
saveRDS(all_pop, paste0(app_folder_path, "pop/all_pop.rds"))

# OTHER REF ---------------------------------------------------------------

# * NPI queries if yearly (BEA-sourced) is desired instead of quarterly (which we're getting from Moody's) ----
# was originally located right above "Moody's NPI" as the history source (except for part indicated otherwise)
# # *** BEA NPI Actuals (DON'T RUN THIS API CALL TWICE IN ONE MINUTE!)  ------
# print("Sleeping for 1 minute before next BEA API call...")
# Sys.sleep(60)
# 
# regional_alias <- "Regional"
# table_name <- "SAINC1"
# 
# # just for information in understanding the api
# print("For reference, showing available dataset parameters for regional data.")
# beaParams(beaKey = bea_api, regional_alias)
# 
# # get linecodes of parameter values
# linecode <- beaParamVals(beaKey = bea_api, regional_alias, "LineCode")$ParamValue
# 
# # pull linecode of interest
# linecode <- as.integer(linecode %>%
#                          filter(str_detect(Desc, table_name) & str_detect(Desc, "Personal income")) %>%
#                          select(Key) %>%
#                          slice(1) %>%
#                          pull()
# )
# 
# # for loop for each year
# bea_start_year <- 1969 # to start % change calculations in 1970
# npi_hist <- NULL
# 
# for(i in bea_start_year:year){
#   response <- beaGet(list('UserID' = bea_api, 
#                           'Method' = 'GetData', 
#                           'datasetname' = regional_alias, 
#                           # 'Frequency' = 'Q', # no quarterly data, default to annual
#                           'TableName' = table_name,
#                           'Year' = i,
#                           'LineCode' = linecode,
#                           "GeoFips" = 49000), # geographic code for Utah
#                      
#                      asWide = FALSE)
#   
#   # check for error, indicating further data isn't available
#   if("character" %in% class(response))
#   {
#     break
#   }
#   
#   # get rid of duplicates? (Alexis)
#   response %<>% distinct()
#   
#   npi_hist <- bind_rows(npi_hist, response)
#   
# }
# 
# # format and organize desired columns
# npi_hist %<>%
#   mutate(Date = as.integer(TimePeriod)) %>%
#   rename(NPI = DataValue) %>%
#   select(Date, NPI)
# 
# # calculate change
# npi_hist %<>%
#   mutate(change = (NPI - lag(NPI, 1)) / lag(NPI, 1))
# 
# # format date
# npi_hist %<>%
#   filter(Date > bea_start_year) # filter only on years with % change value
# 
# # create and format NPI history table
# npi_hist_table <- npi_hist %>%
#   mutate(NPI = dollar(NPI, accuracy = 0.01),
#          change = percent(change, accuracy = 0.1)) %>%
#   rename(`PI Actuals` = NPI, `PI Change` = change)
# 
# # save table
# saveRDS(npi_hist_table, paste0(app_folder_path, "npi/npi_hist_table.rds"))
# 
# 
# # source plot data
# npi_hist %<>%
#   mutate(source = "Personal Income % Change") %>%
#   select(Date, source, change)



# # this came right before filtering, sourcing, and organizing Moody's plot data
# # (the very last pipe in the Moody's section)
# # to account for missing the most recent complete year of data from BEA
# # add in missing historical data if needed
# if(max(npi_hist$Date) < year - 1)
# {
#   # UPDATE npi_hist
#   
#   # get moody_npi change for missing date
#   recent_change_gap <- moody_npi %>%
#     filter(Year == year - 1) %>%
#     pull(change)
#   
#   # create new row with moody's data
#   new_row <- data.frame(Date = year - 1,
#                         source = "Personal Income % Change",
#                         change = recent_change_gap)
#   
#   # add moody's data into npi_hist
#   npi_hist <- rbind(npi_hist, new_row) %>% arrange(Date)
#   
#   
#   # UPDATE npi_hist_table
#   
#   # get moody_npi value for missing date
#   recent_npi_gap <- moody_npi %>%
#     filter(Year == year - 1) %>%
#     pull(NPI)
#   
#   # create new row with moody's data
#   new_row <- data.frame(Date = year - 1,
#                         actuals = dollar(recent_npi_gap, accuracy = 0.01),
#                         change = percent(recent_change_gap, accuracy = 0.1)) %>%
#     rename(`PI Actuals` = actuals,
#            `PI Change` = change)
#   
#   # add moody's data into npi_hist_table
#   npi_hist_table <- rbind(npi_hist_table, new_row) %>% arrange(Date)
#   
#   # resave npi_hist_table
#   saveRDS(npi_hist_table, paste0(app_folder_path, "npi/npi_hist_table.rds"))
#   
# }
#   
# * See Section Notes for "BLS/RAWG Annual Pay Actuals" below -------------

# # *** BLS/RAWG Annual Pay Actuals -----------------------------------------
# 
# #
# # Current version of the script uses only RAWG historical data instead of the
# # below combination of historical data for a few reasons:
# # (1) BLS's public annual pay series is different from the one DWS acquires privately,
# # which is the one RAWG uses.
# # (2) The public BLS data also only goes back to 2001.
# # (3) When merging public BLS data with RAWG history, there was a consistent
# # discrepancy (roughly $500-$1000) between the two sources, which created a
# # problem in 2001's YoY change when the switch was made (see both_pay.png in
# # folder for comparison)
# #
# # Worth noting that although RAWG goes off of DWS numbers, they still are not
# # identical because DWS conducts backward revisions that aren't reflected in
# # RAWG's history file. So actual numbers may be negligibly different, but YoY
# # change is about the same (RAWG historical data ends up just being the most
# # convenient complete data)
# #
# #
# # For this (the previous) approach, a combination of the historical data was used,
# # as BLS was considered the most authoritative and therefore used whenever possible.
# # This is what the script below does to try and resolve some of the issues:
# # BLS data was used as far back as possible (2001), and then RAWG data was
# # pulled in and used to fill in earlier dates. BLS also seemed to be delayed in
# # updating recent data points, so RAWG data was also used to fill in recent dates.
# # The discrepancy described above was realized, and measures were taken to try
# # and manually reconcile the recent values only (see note below in script).
# # Ultimately, it was determined that the RAWG data was a better reference anyway
# # since the UEC is so closely associated with RAWG, and it's conveniently more complete.
# #
# # This could be used to entirely replace the history section currently included
# # in the Annual Pay indicator.
# 
# 
# # get BLS pay history data (back to 2001 only)
# bls_start_year <- 1976
# bls_pay_hist <- NULL
# 
# for(i in 0:2){
#   payload <- list(
#     'seriesid' = "ENU4900050010",
#     'startyear' = bls_start_year + 20*i, # increase at 20 year increments
#     'endyear' = bls_start_year + 19 + 20*i, # set end of range 19 years apart
#     'registrationKey'= bls_api)
# 
#   response <- blsAPI(payload, return_data_frame = T)
# 
#   bls_pay_hist <- bind_rows(bls_pay_hist, response)
# }
# 
# # Remove unwanted columns and order by year
# bls_pay_hist %<>%
#   select(Year = "year", pay = value) %>%
#   mutate(Year = as.numeric(Year),
#          pay = as.numeric(pay)) %>%
#   arrange(Year) %>%
#   as_tibble()
# 
# 
# 
# # get RAWG pay history data (back to 1976)
# rawg_pay_hist <- rawg_hist_file %>%
#   rename(Year = `YR`, pay = CYAVW) %>%
#   select(Year, pay) %>%
#   filter(Year != 0 & !is.na(pay)) %>%
#   as_tibble()
# 
# 
# 
# # combine pay histories
# pay_hist <- full_join(rawg_pay_hist,
#                           bls_pay_hist,
#                           by = "Year",
#                           suffix = c(".rawg", ".bls")) %>%
#   select(Year,
#          rawg_pay_hist = pay.rawg,
#          bls_pay_hist = pay.bls)
# 
# 
# # Calculating exponentially smoothed average difference for imputing missing RECENT value(s):
# # (note that where this difference will be utilized in the first ifelse() statment
# # below, there is no alteration included for more ANCIENT values - consider adding
# # an unsmoothed average to the more ancient values to avoid this undesired jump)
# pay_hist %<>%
#   mutate(diff = bls_pay_hist - rawg_pay_hist)
# 
# diff_values <- pay_hist %>% filter(!is.na(diff)) %>% select(diff) %>% pull()
# 
# # for exponentially smoothed average
# exp_sequence <- exp(seq(0, 1, length.out = length(diff_values)))
# weighting <- exp_sequence / sum(exp_sequence)
# # calculate exponentially weighted diff to account for time-wise increase
# bls_extra <- sum(weighting * diff_values)
# 
# 
# # calculate official Pay column
# pay_hist %<>%
#   mutate(pay = ifelse(is.na(bls_pay_hist),
#                       rawg_pay_hist,
#                       bls_pay_hist)) %>%
#   # although jumping down to revised numbers isn't a problem because of this next mutate,
#   # jumping up from RAWG to BLS isn't smoothed from 2000 to 2001 because the
#   # added constant isn't included in the mutate above like it is in this next part.
#   mutate(pay = ifelse(is.na(bls_pay_hist) & Year > 2000,
#                       rawg_pay_hist + bls_extra,
#                       pay)) # so that jumping back down to unrevised RAWG data isn't too ugly
# 
# 
# pay_hist %<>%
#   select(Year, pay) %>%
#   mutate(change = (pay - lag(pay)) / lag(pay)) %>%
#   filter(complete.cases(.) & Year < year) # drop NAs and limit "history" to before current year
# 
# pay_hist %<>%
#   transmute(Year,
#             source = "Average Annual Pay % Change",
#             pay,
#             change)