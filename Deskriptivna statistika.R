
podaci<-read.csv("agriculture.csv", stringsAsFactors = FALSE)
podaci
str(podaci)


install.packages("Hmisc")
library(Hmisc)


summary(podaci$Year)
var(podaci$Year)
sd(podaci$Year)
describe(podaci$Year)

#
summary(podaci$Average_Temperature_C)
var(podaci$Average_Temperature_C)
sd(podaci$Average_Temperature_C)
describe(podaci$Average_Temperature_C)

ks.test(podaci$Average_Temperature_C, "pnorm", mean= mean(podaci$Average_Temperature_C), sd=sd(podaci$Average_Temperature_C))
hist(podaci$Average_Temperature_C)

#
summary(podaci$Total_Precipitation_mm)
var(podaci$Total_Precipitation_mm)
sd(podaci$Total_Precipitation_mm)
describe(podaci$Total_Precipitation_mm)

ks.test(podaci$Total_Precipitation_mm, "pnorm", mean= mean(podaci$Total_Precipitation_mm), sd=sd(podaci$Total_Precipitation_mm))
hist(podaci$Total_Precipitation_mm)

#
summary(podaci$CO2_Emissions_MT)
var(podaci$CO2_Emissions_MT)
sd(podaci$CO2_Emissions_MT)
describe(podaci$CO2_Emissions_MT)

ks.test(podaci$CO2_Emissions_MT, "pnorm", mean= mean(podaci$CO2_Emissions_MT), sd=sd(podaci$CO2_Emissions_MT))
hist(podaci$CO2_Emissions_MT)

#
summary(podaci$Crop_Yield_MT_per_HA)
var(podaci$Crop_Yield_MT_per_HA)
sd(podaci$Crop_Yield_MT_per_HA)
describe(podaci$Crop_Yield_MT_per_HA)

ks.test(podaci$Crop_Yield_MT_per_HA, "pnorm", mean= mean(podaci$Crop_Yield_MT_per_HA), sd=sd(podaci$Crop_Yield_MT_per_HA))
hist(podaci$Crop_Yield_MT_per_HA)

#
summary(podaci$Extreme_Weather_Events)
var(podaci$Extreme_Weather_Events)
sd(podaci$Extreme_Weather_Events)
describe(podaci$Extreme_Weather_Events)

ks.test(podaci$Extreme_Weather_Events, "pnorm", mean= mean(podaci$Extreme_Weather_Events), sd=sd(podaci$Extreme_Weather_Events))
hist(podaci$Extreme_Weather_Events)

#
summary(podaci$Irrigation_Access_.)
var(podaci$Irrigation_Access_.)
sd(podaci$Irrigation_Access_.)
describe(podaci$Irrigation_Access_.)

ks.test(podaci$Irrigation_Access_., "pnorm", mean= mean(podaci$Irrigation_Access_.), sd=sd(podaci$Irrigation_Access_.))
hist(podaci$Irrigation_Access_.)

#
summary(podaci$Pesticide_Use_KG_per_HA)
var(podaci$Pesticide_Use_KG_per_HA)
sd(podaci$Pesticide_Use_KG_per_HA)
describe(podaci$Pesticide_Use_KG_per_HA)

ks.test(podaci$Pesticide_Use_KG_per_HA, "pnorm", mean= mean(podaci$Pesticide_Use_KG_per_HA), sd=sd(podaci$Pesticide_Use_KG_per_HA))
hist(podaci$Pesticide_Use_KG_per_HA)

#
summary(podaci$Fertilizer_Use_KG_per_HA)
var(podaci$Fertilizer_Use_KG_per_HA)
sd(podaci$Fertilizer_Use_KG_per_HA)
describe(podaci$Fertilizer_Use_KG_per_HA)

ks.test(podaci$Fertilizer_Use_KG_per_HA, "pnorm", mean= mean(podaci$Fertilizer_Use_KG_per_HA), sd=sd(podaci$Fertilizer_Use_KG_per_HA))
hist(podaci$Fertilizer_Use_KG_per_HA)

#
summary(podaci$Soil_Health_Index)
var(podaci$Soil_Health_Index)
sd(podaci$Soil_Health_Index)
describe(podaci$Soil_Health_Index)

ks.test(podaci$Soil_Health_Index, "pnorm", mean= mean(podaci$Soil_Health_Index), sd=sd(podaci$Soil_Health_Index))
hist(podaci$Soil_Health_Index)

#
summary(podaci$Economic_Impact_Million_USD)
var(podaci$Economic_Impact_Million_USD)
sd(podaci$Economic_Impact_Million_USD)
describe(podaci$Economic_Impact_Million_USD)

ks.test(podaci$Economic_Impact_Million_USD, "pnorm", mean= mean(podaci$Economic_Impact_Million_USD), sd=sd(podaci$Economic_Impact_Million_USD))
hist(podaci$Economic_Impact_Million_USD)
