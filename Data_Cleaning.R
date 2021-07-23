library(readr)
library(tidyverse)
library(stringr)
library(lubridate)

###############################
#### 1. LOADING IN DATAFRAMES ####


PrisonCovid <- read_csv("Data/UCLA_Historical-Data_CA.csv")
CACovid <- read_csv("Data/CA_COVID_Daily.csv")
CDCR <- read_csv("Data/CDCR/CDCR_Weekly_Cleaned.csv")
CDCR_Names <- data.frame(Name = unique(CDCR$Name))
Covid_Names <- data.frame(Name = unique(PrisonCovid$Name))
Rates_Demo <- read_csv("Data/rates_demo_mod.csv")

######################
#### 2. Prison COVID ####
PrisonCovid <- PrisonCovid %>% mutate(Name = str_to_upper(Name), 
                          County = str_to_title(County),
                          Date = as.Date(Date, "%Y-%m-%d"))
# add month and year column for merging
PrisonCovid <- PrisonCovid %>% mutate(Year = as.numeric(str_extract(as.character(Date), '[:digit:]{4}(?=-)')),
                          Month =as.numeric(str_extract(as.character(Date), '(?<=(2020|2021)-)[:digit:]{2}')))
# filter to only include prisons 
PrisonCovid <- PrisonCovid %>% filter(Jurisdiction == "state")

# select only variables of interest 
PrisonCovid <- PrisonCovid %>% select(Jurisdiction, Name, County, Date, Year, Month, 
                       Residents.Confirmed, Staff.Confirmed,
                       Residents.Deaths, Staff.Deaths,
                       Residents.Population, Staff.Population,
                       Latitude, Longitude, County.FIPS, Age,
                       Gender, Capacity, BJS.ID
                       )
####################
#### 3. Rates_Demo ####
Rates_Demo <- Rates_Demo %>% mutate(Date = mdy(Date),
                      Week = mdy(Week))
Rates_Demo <- Rates_Demo %>% select(Name, Date, Week, Gender, 
                                    female_prison_adm_rate, male_prison_adm_rate, 
                                    white_prison_adm_rate)
###############               
#### 3. CDCR ####

# split institution name
CDCR <- CDCR %>% mutate(Name = str_extract(Institution, ".*(?= \\()"),
                        Date = mdy(Date))
# adding month and year columns
CDCR <- CDCR %>% mutate(Year = as.numeric(str_extract(as.character(Date), '[:digit:]{4}(?=-)')),
       Month =as.numeric(str_extract(as.character(Date), '(?<=(2020|2021)-)[:digit:]{2}'))) 

# adding jurisdiction column
CDCR <- CDCR %>% mutate(Jurisdiction = "state")

# uppercase name for future merging
CDCR <- CDCR %>% mutate(Name = str_to_upper(Name))


# fix name formatting on some prisons
CDCR$Name[CDCR$Name == "CALIFORNIA HEALTH CARE FACILITY - STOCKTON"] <- 
  "CALIFORNIA HEALTH CARE FACILITY"
CDCR$Name[CDCR$Name == "CALIFORNIA STATE PRISON, CORCORAN"] <- 
  "CALIFORNIA STATE PRISON CORCORAN"
CDCR$Name[CDCR$Name == "CALIFORNIA STATE PRISON, LOS ANGELES COUNTY"] <- 
  "CALIFORNIA STATE PRISON LOS ANGELES"
CDCR$Name[CDCR$Name == "CALIFORNIA STATE PRISON, SACRAMENTO"] <- 
  "CALIFORNIA STATE PRISON SACRAMENTO"
CDCR$Name[CDCR$Name == "CALIFORNIA STATE PRISON, SOLANO"] <- 
  "CALIFORNIA STATE PRISON SOLANO"
CDCR$Name[CDCR$Name == "CENTRAL CALIFORNIA WOMEN'S FACILITY"] <- 
  "CENTRAL CALIFORNIA WOMENS FACILITY"
CDCR$Name[CDCR$Name == "CALIFORNIA SUBSTANCE ABUSE TREATMENT FACILITY"] <- 
  "SUBSTANCE ABUSE TREATMENT FACILITY"
CDCR$Name[CDCR$Name == "SAN QUENTIN STATE PRISON"] <- 
  "CALIFORNIA STATE PRISON SAN QUENTIN"


## Create Week variable for merging with  PrisonCovid
# create variables that hold the unique weeks / days of each dataset
PrisonCovid_dates <- ymd(unique(PrisonCovid$Date))
CDCR_weeks <- (unique(CDCR$Date))

# now create "Week" variable that assigns the appropriate week 
# to each unique date in the time series data
time_series_weeks <- data.frame(cbind("Date"=as.character(PrisonCovid_dates),
                                      "Week"=character(length(PrisonCovid_dates))))

for(i in 1:length(PrisonCovid_dates)){
  # calculate difference between date and vector of weeks
  difference <- PrisonCovid_dates[i] - CDCR_weeks
  
  # find the minimum positive value of the differences (this the week the date belongs to)
  # however, if difference is 0, then the week is equal to the date
  min_pos_diff <- min(difference[difference>=0])
  
  # now add appropriate week to time_series_weeks variable
  time_series_weeks[i,2] <- as.character(CDCR_weeks[which(difference == min_pos_diff)])
}

time_series_weeks <- time_series_weeks[order(time_series_weeks$Date),]

# merge back into PrisonCovid
PrisonCovid$Date <- as.character(PrisonCovid$Date)

# now we can merge the weeks into PrisonCovid:
PrisonCovid <- merge(PrisonCovid, time_series_weeks, by="Date")

# fix date name for merging
CDCR <- CDCR %>% rename(Week = Date)

## deciding which facilities need gender updates
cdcr_genders <- CDCR %>% group_by(Name, Gender) %>% summarise(mean(Percent_Occupied))
cdcr_genders[duplicated(cdcr_genders$Name),]

prison_genders <- PrisonCovid %>% group_by(Name, Gender) %>% summarise(mean(Residents.Population))
prison_genders %>% filter(Name %in% cdcr_genders[duplicated(cdcr_genders$Name),]$Name)
  # only need to mix FOLOM and CA Medical 

## clean FULSOM and CA Medical to include males and females
# subset of CDCR data containing rows for Folsom State Prison and combine data for males + females
CDCR_folsom <- CDCR %>%
  filter(Name == "FOLSOM STATE PRISON") %>% 
  group_by(Week) %>% 
  summarize(Total_Population = sum(Total_Population),
            Design_Capacity = sum(Design_Capacity),
            Staffed_Capacity = sum(Staffed_Capacity),
            Year = mean(Year),
            Month = mean(Month)) %>% 
  mutate(Percent_Occupied = Total_Population / Design_Capacity * 100,
         Gender = "Mixed",
         Name = "FOLSOM STATE PRISON",
         Institution = "FOLSOM STATE PRISON",
         Jurisdiction = "state") %>% 
  select(Week, Institution, Total_Population, Design_Capacity, 
         Percent_Occupied, Staffed_Capacity, Gender, Name, Year, Month, 
         Jurisdiction)

CDCR_CAmedical <- CDCR %>%
  filter(Name == "CALIFORNIA MEDICAL FACILITY") %>% 
  group_by(Week) %>% 
  summarize(Total_Population = sum(Total_Population),
            Design_Capacity = sum(Design_Capacity),
            Staffed_Capacity = sum(Staffed_Capacity),
            Year = mean(Year),
            Month = mean(Month)) %>% 
  mutate(Percent_Occupied = Total_Population / Design_Capacity * 100,
         Gender = "Mixed",
         Name = "CALIFORNIA MEDICAL FACILITY",
         Institution = "CALIFORNIA MEDICAL FACILITY",
         Jurisdiction = "state") %>% 
  select(Week, Institution, Total_Population, Design_Capacity, 
         Percent_Occupied, Staffed_Capacity, Gender, Name, Year, Month, 
         Jurisdiction)


# replace old Folsom State Prison data with this new combined data in CDCR data
CDCR <- CDCR %>% filter(Name != "FOLSOM STATE PRISON" & Name != "CALIFORNIA MEDICAL FACILITY")
CDCR <- rbind(CDCR, CDCR_folsom, CDCR_CAmedical)


##################################################
#### 4. PrisonCovid = PrisonCovid Merged w/ CDCR ####

# convert to character for merging
CDCR$Week <- as.character(CDCR$Week)

## merge 2 options: inner and left join (I will model with the inner join)
PrisonCovid.inner <- PrisonCovid %>% inner_join(CDCR, by = c("Name", "Gender", "Week", "Year"))
PrisonCovid.left  <- PrisonCovid %>% left_join(CDCR, by = c("Name","Gender", "Week", "Year"))

## check to make sure NO double names
test <- PrisonCovid.inner[which(PrisonCovid.inner$Month.x != PrisonCovid.inner$Month.y),]
test <- PrisonCovid.inner[which(PrisonCovid.inner$Gender.x != PrisonCovid.inner$Gender.y),]
  # should return empty data frame
  # it's okay that month.x != month.y cuz some weeks include multiple months 

## merge in race and gender data 
# briefly change dates back to character
Rates_Demo <- Rates_Demo %>% mutate(Date = as.character(Date), Week = as.character(Week))
PrisonCovid.inner <- PrisonCovid.inner %>% left_join(Rates_Demo, by = c("Name", "Date", "Week", "Gender"))
PrisonCovid.inner <- PrisonCovid.inner %>% mutate(Date = ymd(Date), Week = ymd(Week))

## fill in missing values with most recent value
PrisonCovid.inner <- PrisonCovid.inner %>% arrange(Date)%>% group_by(Name) %>% 
  mutate(Residents.Confirmed.F = Residents.Confirmed,
         male_prison_adm_rate.F = male_prison_adm_rate,
         female_prison_adm_rate.F = female_prison_adm_rate,
         white_prison_adm_rate.F = white_prison_adm_rate) %>% fill(Residents.Confirmed.F,
                                                                 male_prison_adm_rate.F,
                                                                 female_prison_adm_rate.F,
                                                                 white_prison_adm_rate.F) 

## add daily change variable from cumulative 
CalculateChange <- function(vec){
  return(vec - c(0,vec[-(length(vec))]))
}
PrisonCovid.inner <- PrisonCovid.inner %>% mutate(Residents.Confirmed_DailyChange = CalculateChange(Residents.Confirmed.F)) 



## save data 
write.csv(PrisonCovid.inner, "Data/PrisonCovid_innerjoin.csv")





