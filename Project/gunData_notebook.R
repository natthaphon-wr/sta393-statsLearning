# Import Library ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)

# Data Preparation -------------------------------------------------------------
gun_dataset <- read_csv("dataset_gun.csv")
summary(gun_dataset)
colSums(is.na(gun_dataset))

## Remove unused columns ----------------------------------
# Computation Method and Notes 
gun_data <- gun_dataset %>% select(-c("Computation method", "Notes"))

## Rename columns  ----------------------------------------
# for easy to use
gun_data <- gun_data %>% rename(Regulation = "The regulation of guns in the United Nations is categorised as", 
                                GunsPer100Persons = "Estimate of civilian firearms per 100 persons",
                                GunCount = "Estimate of firearms in civilian possession",
                                Deaths = "Deaths by firearm",
                                SuicideRate = "Suicide Rate by Firearm",
                                UnintentionalDeaths = "Unintentional Deaths by Firearms",
                                RatePoliceKill = "Rate Police Killing (per 10M)")
# Important Note:
# 1. SuicideRate per 100K
# 2. UnintentionalDeaths per 100K
# 3. RatePoliceKill per 10M

## Population Column -----------------------------------------
na_pop <- gun_data[is.na(gun_data$Population),]
# Found that these country have a lot NA value in another columns too, so remove these country.
gun_data <- gun_data[!is.na(gun_data$Population),]
colSums(is.na(gun_data))

## GunCount Column -----------------------------------------
na_gunCount <- gun_data[is.na(gun_data$GunCount),]
# Found that these country have a lot NA value in another columns too, so remove these country.
gun_data <- gun_data[!is.na(gun_data$GunCount),]
colSums(is.na(gun_data))

## Regulation Column -----------------------------------------
# There are lots of NA, I will change NA value to unknown
gun_data$Regulation <- gun_data$Regulation %>% replace_na('unknown')
gun_data <- gun_data %>% mutate_at(c('Regulation'), as.factor)
summary(gun_data)
colSums(is.na(gun_data))

## Register and Unregistered firearms ------------------------
notna_reg <- gun_data[!is.na(gun_data$`Registered firearms`),]
notna_reg <- notna_reg %>% 
  mutate(SumFirearms = `Registered firearms` + `Unregistered firearms`,
         SumCheck = SumFirearms==GunCount,
         RegisterRatio = `Registered firearms`/GunCount,
         UnregisterRatio = `Unregistered firearms`/GunCount)
summary(notna_reg)
hist(notna_reg$RegisterRatio)
hist(notna_reg$UnregisterRatio)
regisRate_mean <- mean(notna_reg$RegisterRatio)
unregisRate_mean <- mean(notna_reg$UnregisterRatio)

# So I will use registered ratio and unregistered ratio (instead of number), 
#   and replace NA value with mean ratio
colSums(is.na(gun_data))
gun_data$`Registered firearms` <- gun_data$`Registered firearms` %>% replace_na(0)
gun_data$`Unregistered firearms` <- gun_data$`Unregistered firearms` %>% replace_na(0)
colSums(is.na(gun_data))

gun_data <- gun_data %>%
  mutate(RegisterRatio = `Registered firearms`/GunCount,
         UnregisterRatio = `Unregistered firearms`/GunCount) %>%
  select(-c('Registered firearms', 'Unregistered firearms'))
colSums(is.na(gun_data))
summary(gun_data)

gun_data <- gun_data %>% mutate(
  RegisterRatio = case_when(RegisterRatio==0 ~ regisRate_mean, 
                            RegisterRatio>0 ~ RegisterRatio))
gun_data <- gun_data %>% mutate(
  UnregisterRatio = case_when(UnregisterRatio==0 ~ unregisRate_mean,
                              UnregisterRatio>0 ~ UnregisterRatio)
)
colSums(is.na(gun_data))
summary(gun_data)

## Deaths, Gun Death Rate, SuicideRate, UnintentionalDeaths Columns -------
# Not sure Deaths colum means, it's seem different from Gun Death Rate.
# For example, Deaths is 0, but Gun Death Rate isn't 0.
na_deaths <- gun_data[is.na(gun_data$Deaths),]
notna_deaths <- gun_data[!is.na(gun_data$Deaths),]
# There is only 1 country that haven't NA value in Gun Death Rate while
#   other columns are NA value.

hist(notna_deaths$Deaths)
deaths_median <- median(notna_deaths$Deaths)
deaths_mean <- mean(notna_deaths$Deaths)

hist(notna_deaths$`Gun Death Rate`)
gunDeathsRate_median <- median(notna_deaths$`Gun Death Rate`)
gunDeathsRate_mean <- mean(notna_deaths$`Gun Death Rate`)

hist(notna_deaths$SuicideRate)
suicideRate_median <- median(notna_deaths$SuicideRate)
suicideRate_mean <- mean(notna_deaths$SuicideRate)

hist(notna_deaths$UnintentionalDeaths)
unintDeaths_median <- median(notna_deaths$UnintentionalDeaths)
unintDeaths_mean <- mean(notna_deaths$UnintentionalDeaths)

# I will replace NA value with median value for each columns.
colSums(is.na(gun_data))
gun_data$Deaths <- gun_data$Deaths %>% replace_na(deaths_median)
gun_data$`Gun Death Rate` <- gun_data$`Gun Death Rate` %>% replace_na(gunDeathsRate_median)
gun_data$SuicideRate <- gun_data$SuicideRate %>% replace_na(suicideRate_median)
gun_data$UnintentionalDeaths <- gun_data$UnintentionalDeaths %>% replace_na(unintDeaths_median)
colSums(is.na(gun_data))

## Data Year Police Killing Column -------------------------
# It's about year that collect data, so I will remove this.
gun_data <- gun_data %>% select(-c("Data Year Police Killing"))
colSums(is.na(gun_data))

## Police Killings, RateProliceKill -------------------------
na_police <- gun_data[is.na(gun_data$`Police Killings`),]
colSums(is.na(na_police))
notna_police <- gun_data[!is.na(gun_data$`Police Killings`),]

hist(notna_police$`Police Killings`)
policeKill_median <- median(notna_police$`Police Killings`)
policeKill_mean <- mean(notna_police$`Police Killings`)

hist(notna_police$RatePoliceKill)
ratePoliceKill_median <- median(notna_police$RatePoliceKill)
ratePoliceKill_mean <- mean(notna_police$RatePoliceKill)

# I will replace NA value with median value for these 2 columns.
colSums(is.na(gun_data))
gun_data$`Police Killings` <- gun_data$`Police Killings` %>% replace_na(policeKill_median)
gun_data$RatePoliceKill <- gun_data$RatePoliceKill %>% replace_na(ratePoliceKill_median)

## Final Dataset ----------------------------------------------
prep_data <- gun_data
summary(prep_data)
colSums(is.na(prep_data))
# There are 13 variables, 227 observations, and no NA values.


# Data Exploration -------------------------------------------------------------

