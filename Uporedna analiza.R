data<-read.csv("agriculture.csv", stringsAsFactors = FALSE)

data24<-subset(data, Year==2024)
data14<-subset(data, Year==2014)

#izbacujem char varijabale
str(data24)
data24$Year<-NULL
data24$Crop_Type<-NULL
data24$Country<-NULL
data24$Region<-NULL
data24$Adaptation_Strategies<-NULL

str(data14)
data14$Year<-NULL
data14$Crop_Type<-NULL
data14$Country<-NULL
data14$Region<-NULL
data14$Adaptation_Strategies<-NULL



#AVERAGE_TEMPERATURE
length(data24$Average_Temperature_C)
length(data14$Average_Temperature_C)

test_temp<-t.test(data14$Average_Temperature_C, data24$Average_Temperature_C, paired = TRUE)
test_temp
#Od 2014. do 2024. godine temperatura se u proseku povecala za 0.89 stepeni celzijusa



#TOTAL_PRECIPITATION
length(data24$Total_Precipitation_mm)
length(data14$Total_Precipitation_mm)

test_prec<-t.test(data14$Total_Precipitation_mm, data24$Total_Precipitation_mm, paired = TRUE)
test_prec
#Od 2014. do 2024. godine, kolicina padavina se smanjila za 42.56mm (mm= 1 litar vode po 1 kvadratnom metru)



#CO2_EMISSION
length(data24$CO2_Emissions_MT)
length(data14$CO2_Emissions_MT)

test_co2<-t.test(data14$CO2_Emissions_MT, data24$CO2_Emissions_MT, paired = TRUE)
test_co2
#od 2014. d0 2024. emisija C02 je u proseku porasla za 1.03 metrickih tona(1MT= 1000kg)



#CROP_YIELD
length(data24$Crop_Yield_MT_per_HA)
length(data14$Crop_Yield_MT_per_HA)

test_crop<-t.test(data14$Crop_Yield_MT_per_HA, data24$Crop_Yield_MT_per_HA, paired = TRUE)
test_crop
#Prinos useva od 2014. do 2024. godine se u proseku smanjio za 0.1 tona po HA



#EXTREME_WEATHER_EVENTS
length(data24$Extreme_Weather_Events)
length(data14$Extreme_Weather_Events)

test_extr<-t.test(data14$Crop_Yield_MT_per_HA, data24$Crop_Yield_MT_per_HA, paired = TRUE)
test_extr
#Razlika u broju ektremnih vremenskih prilika u proseku izmedju 2014. i 2024. godine je manja od 1 (0.1)



#IRRIGATION_ACCESS
length(data24$Irrigation_Access_.)
length(data14$Irrigation_Access_.)

test_irrigation<-t.test(data14$Irrigation_Access_., data24$Irrigation_Access_., paired = TRUE)
test_irrigation
#Dostupnost navodnjavanja se u 2024. godini povecala za 0.25 jedinica 



#Pesticide_Use_KG_per_HA 
length(data24$Pesticide_Use_KG_per_HA)
length(data14$Pesticide_Use_KG_per_HA)

test_pesticide<-t.test(data14$Pesticide_Use_KG_per_HA, data24$Pesticide_Use_KG_per_HA, paired = TRUE)
test_pesticide
#Prosecna kolicina koriscenih pesticida se od 2014. do 2024. godine smanjila za 0.8 kg po HA



#Fertilizer_Use_KG_per_HA
length(data24$Fertilizer_Use_KG_per_HA)
length(data14$Fertilizer_Use_KG_per_HA)

test_fertilizer<-t.test(data14$Fertilizer_Use_KG_per_HA, data24$Fertilizer_Use_KG_per_HA, paired = TRUE)
test_fertilizer
#Prosecna kolicina koriscenog djubriva se od 2014. do 2024. godine smanjila za 2 kg po HA



#Soil_Health_Index
length(data24$Soil_Health_Index)
length(data14$Soil_Health_Index)

test_soil<-t.test(data14$Soil_Health_Index, data24$Soil_Health_Index, paired = TRUE)
test_soil
#Indeks zdravlja zemlje se od 2014. do 2024. godine u proseku smanjio za 0.42 jedinice




#Economic_Impact_Million_USD
length(data24$Economic_Impact_Million_USD)
length(data14$Economic_Impact_Million_USD)

test_economic<-t.test(data14$Economic_Impact_Million_USD, data24$Economic_Impact_Million_USD, paired = TRUE)
test_economic
#Prosecni ulozen novac u poljoprivredu se od 2014. do 2024. godine smanjio za 28.89 miliona dolara