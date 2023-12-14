library(tidyverse)
library(haven)
library(sf)
library(lutz)
library(suncalc)
library(hms)
library(extrafont)
library(hrbrthemes)
options(timeout=300)
get_fars <- function(year){
  fars_url <- paste0("https://static.nhtsa.gov/nhtsa/downloads/FARS/",
                     year,
                     "/National/FARS",
                     year,
                     "NationalSAS.zip")
  temp_file <- tempfile()
  download.file(fars_url, destfile = temp_file)
  unzip(temp_file, exdir = paste0("data/", year), overwrite = TRUE)
}

walk(2021, safely(get_fars))


read_FARS <- function(year, table_type){
  read_sas(paste0("data/", year, "/FARS", year, "NationalSAS/",table_type, ".sas7bdat"))
}

read_FARS_pre_2020 <-  function(year, table_type){
  read_sas(paste0("data/", year, "/", table_type, ".sas7bdat"))
}

accident_00_19 <- map2_dfr(2000:2019, "accident", read_FARS_pre_2020)
accident_20_21 <- map2_dfr(2020:2021, "accident", read_FARS)

person_16_19 <- map2_dfr(2016:2019, "person", read_FARS_pre_2020)
person_20_21 <- map2_dfr(2020:2021, "person", read_FARS)

person <- bind_rows(person_16_19, person_20_21)
accident <- bind_rows(accident_16_19, accident_20_21)

accident <- accident |> 
  filter(LONGITUD < 180 | LATITUDE < 70)

peds <- person |> 
  filter(PER_TYP == 5 & INJ_SEV == 4)

peds <- peds |> 
  left_join(accident, by = join_by(ST_CASE, MONTH, DAY, HOUR, MINUTE))

peds_2 <- peds |> 
  filter(MINUTE != 99 & !is.na(LATITUDE)) |> #remove crashes with missing time, coords
  mutate(
    crash_time = ymd_hm(paste(YEAR, MONTH, DAY, HOUR, MINUTE, sep = "-")),
    crash_date = ymd(paste(YEAR, MONTH, DAY, sep = "-"))) 

  
# create sf object
peds_sf <- st_as_sf(peds_2, coords = c("LONGITUD", "LATITUDE"), remove = FALSE) #keep lat/lon columns    
peds_sf$tz <- tz_lookup(peds_sf, method = "accurate")

# Function to convert times to UTC
convert_to_utc <- function(datetime, timezone) {
  # Set the time zone of the datetime object
  datetime <- force_tz(datetime, tzone = timezone) #assign time zone
  
  # Convert the datetime to UTC
  datetime_utc <- with_tz(datetime, tzone = "UTC") #convert to time zone
  
  return(as_datetime(datetime_utc))
}

peds_sf$crash_time_utc <- mapply(convert_to_utc, datetime = peds_sf$crash_time, timezone = peds_sf$tz)


peds_sf <- peds_sf |> 
  mutate(crash_time_utc = as_datetime(crash_time_utc),
         crash_date_utc = date(crash_time_utc),
         crash_time_utc_hms = hms::as_hms(crash_time_utc))


peds_3 <-  peds_sf |> 
  rowwise() |> 
  mutate(sun_0 = getSunlightTimes(date = crash_date, lat = LATITUDE, lon = LONGITUD),
         sun_plus_1 = getSunlightTimes(date = crash_date + 1, lat = LATITUDE, lon = LONGITUD),
         sun_minus_1 = getSunlightTimes(date = crash_date - 1, lat = LATITUDE, lon = LONGITUD),
         sunrise_0 = sun_0$sunrise,
         sunrise_plus_1 = sun_plus_1$sunrise,
         sunrise_minus_1 = sun_minus_1$sunrise,
         sunset_0 = sun_0$sunset,
         sunset_plus_1 = sun_plus_1$sunset,
         sunset_minus_1 = sun_minus_1$sunset)



peds4 <- peds_3 |> 
  mutate(time_from_sunrise_0 = crash_time_utc - sunrise_0,
         time_from_sunset_0 = crash_time_utc - sunset_0,
         time_from_sunrise_minus_1 = crash_time_utc - sunrise_minus_1,
         time_from_sunset_minus_1 = crash_time_utc - sunset_minus_1,
         time_from_sunrise_plus_1 = crash_time_utc - sunrise_plus_1,
         time_from_sunset_plus_1 = crash_time_utc - sunset_plus_1,
         time_from_sunrise_min = case_when(
           min(abs(time_from_sunrise_0), abs(time_from_sunrise_minus_1), abs(time_from_sunrise_plus_1)) == abs(time_from_sunrise_0) ~ time_from_sunrise_0, 
           min(abs(time_from_sunrise_0), abs(time_from_sunrise_minus_1), abs(time_from_sunrise_plus_1)) == abs(time_from_sunrise_minus_1) ~ time_from_sunrise_minus_1,
           min(abs(time_from_sunrise_0), abs(time_from_sunrise_minus_1), abs(time_from_sunrise_plus_1)) == abs(time_from_sunrise_plus_1) ~ time_from_sunrise_plus_1),
         time_from_sunset_min = case_when(
           min(abs(time_from_sunset_0), abs(time_from_sunset_minus_1), abs(time_from_sunset_plus_1)) == abs(time_from_sunset_0) ~ time_from_sunset_0, 
           min(abs(time_from_sunset_0), abs(time_from_sunset_minus_1), abs(time_from_sunset_plus_1)) == abs(time_from_sunset_minus_1) ~ time_from_sunset_minus_1,
           min(abs(time_from_sunset_0), abs(time_from_sunset_minus_1), abs(time_from_sunset_plus_1)) == abs(time_from_sunset_plus_1) ~ time_from_sunset_plus_1)
         ) |> 
  relocate(starts_with("time_from"), starts_with("crash_time"), starts_with("suns"), starts_with("sunr"))

# need to calculate sunrise and sunset for the day before and the day after
# and then just use the smallest value

state_fips <- tigris::fips_codes |> 
  distinct(state_code, .keep_all = T) |> 
  mutate(state_code = as.numeric(state_code)) |> 
  select(state_code, state)

peds4 |> 
  left_join(state_fips, by = join_by(STATE.x == state_code)) |> 
  ggplot(aes(as.numeric(time_from_sunset_min, units = "hours"))) +
  geom_histogram(binwidth = .25) +
  geom_vline(xintercept = 0, color = "red", alpha = 0.3) +
  # facet_wrap(~YEAR) +
  ylab("Pedestrian fatalities") +
  xlab("Hours from sunset") +
  labs(title = "Crashes are highly concentrated right after sunset",
       subtitle = "Fatal pedestrian crashes in the US, 2016-2021; 15-minute bins",
       caption = "Data: Fatality Analysis Reporting System (FARS)\nVisualization: Harald Kliems") +
  theme_ipsum(base_family = "Roboto Condensed")
  




x |> 
  mutate(sunrise_bins = cut_width(as.numeric(time_from_sunrise), width = 3600)) |> 
  summarize(n(), .by = sunrise_bins)
  ggplot2(aes())

library(tmap)
tm_shape(peds_sf) +
  tm_dots()

sunriset(peds_sf[1,], dateTime = peds_sf$crash_time[1])

peds_sf |> 
  rowwise() |> 

  # Set the time zone of the datetime object
  datetime <- with_tz(datetime, tzone = timezone)

# Convert the datetime to UTC
datetime_utc <- with_tz("2021-01-01 01:08:00", tzone = "America/Chicago")
