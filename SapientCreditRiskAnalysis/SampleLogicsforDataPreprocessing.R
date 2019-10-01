#Sample logic to convert the days of birth into age

library(eeptools)
todays_date <- Sys.Date()
sampledf <- data.frame(days_birth=c(-19000, -15322, -11120, -11824, -22102))
sampledf$days_birth <- abs(sampledf$days_birth)
sampledf$dob <- as.Date(as.character(todays_date),format = "%Y-%m-%d") - sampledf$days_birth
age <- age_calc(sampledf$dob, enddate = Sys.Date(), units = "years", precise = TRUE)
sampledf$age = round(as.numeric(age), digits = 0)



library(eeptools)
todays_date <- Sys.Date()
sampledf <- data.frame(days_birth=c(-826, 365243, -61, -4467, 365243, -1955))
sampledf$days_birth <- abs(sampledf$days_birth)
sampledf$dob <- as.Date(as.character(todays_date),format = "%Y-%m-%d") - sampledf$days_birth
age <- age_calc(sampledf$dob, enddate = Sys.Date(), units = "years", precise = TRUE)
sampledf$age = round(as.numeric(age), digits = 0)
