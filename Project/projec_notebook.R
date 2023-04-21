# Import Library ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)

# Data Preparation -------------------------------------------------------------
gun_data <- read_csv("dataset_gun.csv")
summary(gun_data)
colSums(is.na(gun_data))

## Remove unused columns ----------------------------------
# Computation Method and Notes 
gun_data <- gun_data %>% select(-c("Computation method", "Notes"))

## Rename columns  ----------------------------------------
# for easy to use
gun_data <- gun_data %>% rename(Regulation = "The regulation of guns in the United Nations is categorised as", 
                                GunsPer100Persons = "Estimate of civilian firearms per 100 persons",
                                GunCount = "Estimate of firearms in civilian possession",
                                Deaths = "Deaths by firearm",
                                SuicideRate = "Suicide Rate by Firearm",
                                UnintentionalDeaths = "Unintentional Deaths by Firearms",
                                RatePoliceKill = "Rate Police Killing (per 10M)")

## Population Column -----------------------------------------
na_pop <- gun_data[is.na(gun_data$Population),]
# Found that these country have a lot NA value in another columns too, 
#   so remove these country.
gun_data <- gun_data[!is.na(gun_data$Population),]
colSums(is.na(gun_data))

## Regulation Column -----------------------------------------
# There are lots of NA, I will change NA value to unknown
gun_data$Regulation <- gun_data$Regulation %>% replace_na('unknown')
gun_data <- gun_data %>% mutate_at(c('Regulation'), as.factor)

## Gun Count Column ------------------------------------------
fix(gun_data) #change gun count to 0 from -

## Register and Unregistered firearms ------------------------
fix(gun_data) #remove est.
notna_reg <- gun_data[!is.na(gun_data$`Registered firearms`),]
notna_reg <- notna_reg %>% 
  mutate_at(c('Registered firearms', 'Unregistered firearms', 'GunCount'), as.numeric) %>% 
  mutate(SumFirearms = `Registered firearms` + `Unregistered firearms`,
         SumCheck = SumFirearms==GunCount,
         RegisterRatio = `Registered firearms`/GunCount,
         UnregisterRatio = `Unregistered firearms`/GunCount)
summary(notna_reg)
regisRate_mean <- mean(notna_reg$RegisterRatio)
unregisRate_mean <- mean(notna_reg$UnregisterRatio)

# So I will use registered ratio and unregistered ratio (instead of number), 
#   and replace NA value with mean ratio
colSums(is.na(gun_data))
summary(gun_data)
gun_data$`Registered firearms` <- gun_data$`Registered firearms` %>% replace_na(0)
gun_data$`Unregistered firearms` <- gun_data$`Unregistered firearms` %>% replace_na('0')

gun_data2 <- gun_data %>% 
  mutate_at(c('Registered firearms', 'Unregistered firearms', 'GunCount'), as.numeric) %>%
  mutate(RegisterRatio = `Registered firearms`/GunCount,
         UnregisterRatio = `Unregistered firearms`/GunCount) %>% 
  select(-c('Registered firearms', 'Unregistered firearms'))
          
gun_data2[is.na(gun_data2$`Registered firearms`),]

gun_data2 <- gun_data2 %>% mutate(
  `Registered firearms` = case_when(0 ~ regisRate_mean)
)









