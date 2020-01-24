library(dplyr)
library(acs)
library(datapkg)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Per Capita Income by County
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", raw_location))
x2016_files <- dir(path_to_raw_data, recursive=T, pattern = "ACS") 

#Get state data
geography=geo.make(state=09)
yearlist=c(2009:2018)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

tables <- c("", "A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("All", "White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                         table.number=paste0("B19301", tbl), col.names=col.names, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    pc_income <- acsSum(data, 1, "Per Capita Income")
    estimates <- data.table(
            geo, 
            estimate(pc_income),
            year,
            race,
            "Number",
            "Per Capita Income"
        )
    moes <- data.table(
            geo,
            standard.error(pc_income) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "County", "FIPS",
            "Total",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(estimates, numberNames)
    setnames(moes, numberNames)
    numbersData.melt <- melt(
            rbind(estimates, moes),
            id.vars=c("County", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Per Capita Income",
            variable.factor = F,
            value.name="Value",
            value.factor = F
    )
    inter_data <- rbind(inter_data, numbersData.melt)
  }
  state_data <- rbind(state_data, inter_data)
}

#Get county data
geography=geo.make(state=09, county="*")   

county_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                         table.number=paste0("B19301", tbl), col.names=col.names, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$NAME <- gsub(", Connecticut", "", geo$NAME)
    geo$county <- gsub("^", "09", geo$county)
    geo$state <- NULL
    pc_income <- acsSum(data, 1, "Per Capita Income")
    estimates <- data.table(
            geo, 
            estimate(pc_income),
            year,
            race,
            "Number",
            "Per Capita Income"
        )
    moes <- data.table(
            geo,
            standard.error(pc_income) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "County", "FIPS",
            "Total",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(estimates, numberNames)
    setnames(moes, numberNames)
    numbersData.melt <- melt(
            rbind(estimates, moes),
            id.vars=c("County", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Per Capita Income",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    
    inter_data <- rbind(inter_data, numbersData.melt)
  }
  county_data <- rbind(county_data, inter_data)
}
# 
# #process 2016 data
# x2016_data <- data.frame(stringsAsFactors = FALSE)
# for (i in 1:length(x2016_files)) {
#   data <- read.acs(paste0(path_to_raw_data, "/", x2016_files[i]), endyear=2016, span=5)
#   race <- races[i]
#   year <- data@endyear
#   year <- paste(year-4, year, sep="-")
#   geo <- data@geography
#   geo$Geography <- gsub(", Connecticut", "", geo$Geography)
#   geo$Id2 <- gsub("^", "0", geo$Id2)
#   geo$Id <- NULL
#   pc_income <- acsSum(data, 1, "Per Capita Income")
#   estimates <- data.table(
#             geo, 
#             estimate(pc_income),
#             year,
#             race,
#             "Number",
#             "Median Household Income"
#         )
#     moes <- data.table(
#             geo,
#             standard.error(pc_income) * 1.645,
#             year,
#             race,
#             "Number",
#             "Margins of Error"
#         )
#     numberNames <- c(
#             "County", "FIPS",
#             "Total",
#             "Year",
#             "Race/Ethnicity",
#             "Measure Type",
#             "Variable"
#          )
#     setnames(estimates, numberNames)
#     setnames(moes, numberNames)
#     numbersData.melt <- melt(
#             rbind(estimates, moes),
#             id.vars=c("County", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
#             variable.name="Per Capita Income",
#             variable.factor = F,
#             value.name="Value",
#             value.factor = F
#     )
#     x2016_data <- rbind(x2016_data, numbersData.melt)  
# }

pcap_income <- rbind(county_data, state_data)

pcap_income$`Per Capita Income` <- NULL

pcap_income$Value <- round(pcap_income$Value, 2)

pcap_income <- pcap_income %>% 
  select(County, FIPS, Year, `Race/Ethnicity`, `Measure Type`, Variable, Value) %>% 
  arrange(County, Year, `Race/Ethnicity`, `Measure Type`)

pcap_income$Value[pcap_income$Value < 0] = -9999

write.table (
  pcap_income,
  file.path(getwd(), "data", "per_capita_income_county_2018.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)

