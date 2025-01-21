
podaci<-read.csv("agriculture.csv", stringsAsFactors = FALSE)
podaci
str(podaci)

#iz bovog data seta izbacujem nenumericke varijable
noviPodaci<-podaci
noviPodaci$Country<-NULL
noviPodaci$Region<-NULL
noviPodaci$Adaptation_Strategies<-NULL
noviPodaci$Crop_Type<-NULL
noviPodaci$Extreme_Weather_Events<-NULL
str(noviPodaci)

summary(noviPodaci$Year)
table(noviPodaci$Year)

#najstarija godina za koju imamo podatke je 1990, a najnovija 2024, tako da cu porediti modele za ove dve godine
#_________________________________________________________________________________________________________________

#Linearni model 2024
podaci24<- subset(noviPodaci, Year==2024)
str(podaci24)
podaci24$Year<-NULL

apply(podaci24,2, function(x) sum(is.na(x)))
#nema nedostajucih vrednosti

library(corrplot)
matrica24<-cor(podaci24)
corrplot(matrica24, method = "number", type = "upper", diag = FALSE)
#najveci stepen korelacije je sa varijablom Economic_Impact_Million_USD
#izbacujem varijable Soil_Health_Index i Irrigation_Access_.
podaci24$Soil_Health_Index<-NULL
podaci24$Irrigation_Access_.<-NULL


library(caret)
set.seed(1010)
index<-createDataPartition(podaci24$Crop_Yield_MT_per_HA, p=0.8, list = FALSE)
trening<-podaci24[index,]
test<-podaci24[-index,]

lm24<-lm(Crop_Yield_MT_per_HA~., data = trening)
summary(lm24)
#Medijana reziduala je -0.1194
#najmanja vrednost je -1.0143, a najveca 2.1528
#Statisticki znacajne varijable koje uticu na prinos su Economic_Impact_Million_USD(***) i Average_Temperature_C(*)
#Podeseni koeficijent determinacije je 0.5668, sto znaci da je 56.68% varijabiliteta zavisne varijable objasnjeno nezavisnim

library(car)
vif(lm24)
#Vrednosti multikolinearnosti za sve varijable su manji od 1.1 pa ih necu izbacivati

lm24.predict<-predict(lm24, test)
rss24<-sum((lm24.predict-test$Crop_Yield_MT_per_HA)^2)

rmse24<-sqrt(rss24/nrow(test))
rmse24
#koren srednje kvadratne greske
#vrednost 0.7684615 je ocekivana greska modela

rmse24/mean(test$Crop_Yield_MT_per_HA)

#Kada podelimo ovu vrednost sa srednjom vrednosti zavisne promenjive iz skupa test podataka dobijamo 0.3559487, sto znaci da je
#ocekivana greska modela 35.56%

plot(lm24)




#Linearni model 2014

podaci14<-subset(noviPodaci, Year==2014)
str(podaci14)

podaci14$Year<-NULL

apply(podaci14,2, function(x) sum(is.na(x)))
#nema NA vrednosti

matrica14<-cor(podaci14)
corrplot(matrica14, method = "number", type="upper", diag = FALSE)
#najveci stepen korelacije je sa varijablom Economic_Impact_Million_USD
#izbacujem varijable Soil_Health_Index i Irrigation_Access_.
podaci14$Soil_Health_Index<-NULL
podaci14$Irrigation_Access_.<-NULL

library(caret)
ind<-createDataPartition(podaci14$Crop_Yield_MT_per_HA, p=0.8, list = FALSE)
train_set<-podaci14[ind,]
test_set<-podaci14[-ind,]

lm14<-lm(Crop_Yield_MT_per_HA~., train_set)
summary(lm14)
#najmanja vrednost reziduala je -1.0846, a najveca 2.0904
#Statisticki znacajne varijable koje uticu na prinos su Economic_Impact_Million_USD(***) i Average_Temperature_C(*)
#Podeseni koeficijent determinacije je 0.5496, sto znaci da je 54.96% varijabiliteta zavisne varijable objasnjeno nezavisnim

vif(lm14)
#Vresnosti multikolinearnosti za sve varijable su manji od 1.1 pa necu nista izbacivati

lm14.predict<-predict(lm14, test_set)
rss14<-sum((lm14.predict-test_set$Crop_Yield_MT_per_HA)^2)

rmse14<-sqrt(rss14/nrow(test_set))
rmse14

#koren srednje kvadratne greske
#vrednost 0.7488218 
rmse14/mean(test_set$Crop_Yield_MT_per_HA)
#Kada podelimo ovu vrednost sa srednjom vrednosti zavisne promenjive iz skupa test podataka dobijamo 0.3022544, sto znaci da je
#ocekivana greska modela 30.22%

plot(lm24)
plot(lm14)
