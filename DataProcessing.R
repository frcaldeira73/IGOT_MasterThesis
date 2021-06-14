
## ---------------------------
##
## Script name: DataProcessing
##
##keywords: openweather, climatic data, normalize
##
## Purpose of script: This script created normalized mensal climatic data from file of
## openweather (https://openweathermap.org/history-bulk)  That's a csv file with hour data.
## the data is normalized to be used it Machine Learning algorithms for example.
##
## Author: Francisco Caldeira
##
## Version:1.0
## Date Created: 07-12-2020
##
## Copyright:Modelling wheat productivity combining wheather data and Earth observation data © 2021 by Francisco Caldeira is licensed under CC BY 4.0
##
## Returns: 2 csv file, with climatic data by month, normalized and not normalized
##
## Email: francisco.caldeira@ine.pt
##
## ---------------------------
##
## Notes: This script has three arguments
## SourceFile -> It's the openWaether csv file and path
## OutfileNormalized -> It's the new file with climatic data for month and path Normalized
## OutfileNotNormalized -> It's the new file with climatic data for month and path not Normalized
## ---------------------------


  

DataProcessing <- function(SourceFile, OutfileNormalized, OutfileNotNormalized, years, WheatProduction = c()){
#----------------
#DATA PREPARATION
#----------------
#Creating a dataframe from OurDataset 
myData <- read.csv(SourceFile, header = TRUE) 



#Convert into date format
x <- as.Date(myData$dt_iso)

#Getting the year
year <- as.numeric(format(x, '%Y')) 
toString(year)
head (year)

#Getting the month
month <- as.numeric(format(x, '%m'))
toString(month)
head (month)

#Getting the day
day <- as.numeric(format(x, '%d'))
toString(day)
head (day)


#formating the month as "01" 
month2 <- formatC(month, width=2, flag="0", format="d")
head (month2)

#formating the day as "01" 
day2 <- formatC(day, width=2, flag="0", format="d")
head (day2)




#Add the columns to the existing data frame
myData <- cbind(myData,year,month2,day2) 

head(sample(myData))
#View(myData)

#drop columnsby name # ERROS AQUI
#myData = subset(myData, select = -c(month2,day2) )
#myData = subset(myData, select = -c(month,day,year,year.1) )

#View data in tabular format
#View (myData)

#Setting new column names
colnames(myData)[which(names(myData) == "month2")] <- "month"
colnames(myData)[which(names(myData) == "day2")] <- "day"

#Creating the date and month_year column to summarize
library(tidyr)
myData <- unite(myData, data, c(day, month, year),sep="/", remove=FALSE, na.rm = FALSE)
myData <- unite(myData, Month_Year, c(month, year),sep="/", remove=FALSE, na.rm = FALSE)


#Save a version of the data
#write.csv(myData,"C:\\lab\\mestrado\\TESE\\DADOS\\OpenWeather\\BEJA_01.csv", row.names = TRUE)


#--------------
#DATA ANALYSIS
#--------------

#Plotting Analysys

#Histogram
hist (myData$temp, xlab="Temperatura (cº)", ylab = "Frequência", main = "Distribuição da Temperatura", col ="aquamarine3")

#Boxplot 
boxplot (myData$temp, ylab="Temperatura (cº)", main =  "Temperatura na estação meteorológica de Beja", col= "burlywood")




#---------------------
#Variable preparation
#---------------------

# Variable List
# 01. Precipitação mensal (rain_1h) RainByDay // RainByMonth
# 02. Temperatura Média (temp)  TempAVGByDay // TempAVGByMonth
# 03. Temperatura Mínima (temp_min) TempMINByDay // TempMINByMonth
# 04. Temperatura Máxima (temp_max) TempMAXByDay // TempMAXByMonth
# 05. Cloudiness (clouds_all) CloudsAVGByDay // CloudsAVGByMonth
#  Velocidade do Vento ??
#  Direção do vento ??
# 06. Humidade(humidity) //HumidityAVGByMonth
# 07. 15 Setembro a 15 Outubro com Chuva
# 08. Abril com 100mm chuva (Entre 75 e 125mm)
# 09. Maio com chuva
# 10. Junho sem Chuva
# 11. Março e Abril entre 15º e 20º
# 12. Maio e Junho entre 16º e 20º
# 13. Nº dias Maio e Junho > 30º e vento de Este
# 14. Nº dias Maio e Junho > 30º  
# 15. Nº horas com Temperatura mínima negativa Dezembro - Possibilidade de Geadas
# 16. Nº horas com Temperatura mínima negativa Janeiro - Possibilidade de Geadas 
# 17. Nº horas com Temperatura mínima negativa Fevereiro - Possibilidade de Geadas 


library(dplyr)
library(rlist)

#-----------------------------------------------------------------------
#Variable 01 Precipitação mensal (rain_1h) - RainByDay // RainByMonth
#-----------------------------------------------------------------------

RainByDay  = myData %>%
  group_by(data) %>%
  summarise(rain = sum(rain_1h, na.rm = TRUE),Month_Year = first(Month_Year) )


#typeof(RainByDay)

RainByDay <-RainByDay[order(as.Date( RainByDay$data, format="%d/%m/%Y")),]

#RainByDay  = summarise(myData, Precipitacao=sum(rain_1h, na.rm=T))

#View(RainByDay)
#View (myData)

RainByMonth = RainByDay %>%
  group_by(Month_Year) %>%
  summarise(rain = sum(rain, na.rm = TRUE))
#View(RainByMonth)

RainByMonth <-RainByMonth[order(as.Date( RainByMonth$Month_Year, format="%m/%Y")),]
#View(RainByMonth)

#-------------------------------------------------------------------------
#Variable 2 Temperatura Média (temp) -TempAVGByDay // TempAVGByMonth
#-------------------------------------------------------------------------

TempAVGByDay  = myData %>%
  group_by(data) %>%
  summarise(tempAVG = mean(temp, na.rm = TRUE),Month_Year = first(Month_Year) )

TempAVGByDay <-TempAVGByDay[order(as.Date( TempAVGByDay$data, format="%d/%m/%Y")),]

 

TempAVGByMonth = TempAVGByDay %>%
  group_by(Month_Year) %>%
  summarise(tempAVG = mean(tempAVG, na.rm = TRUE))
 

TempAVGByMonth <-TempAVGByMonth[order(as.Date( TempAVGByMonth$Month_Year, format="%m/%Y")),]
#View(TempAVGByMonth)


#-------------------------------------------------------------------------
#Variable 3 Temperatura Mínima (temp_min) -TempMINByDay // TempMINByMonth
#-------------------------------------------------------------------------


TempMINByDay  = myData %>%
  group_by(data) %>%
  summarise(tempMIN = min(temp_min, na.rm = TRUE),Month_Year = first(Month_Year) )

TempMINByDay <-TempMINByDay[order(as.Date( TempMINByDay$data, format="%d/%m/%Y")),]

#View (TempMINByDay)

TempMINByMonth = TempMINByDay %>%
  group_by(Month_Year) %>%
  summarise(tempMIN = mean(tempMIN, na.rm = TRUE))


TempMINByMonth <-TempMINByMonth[order(as.Date( TempMINByMonth$Month_Year, format="%m/%Y")),]
#View(TempMINByMonth)


#-------------------------------------------------------------------------
#Variable 4 Temperatura Máxima (temp_max) - TempMAXByDay // TempMAXByMonth
#-------------------------------------------------------------------------

TempMAXByDay  = myData %>%
  group_by(data) %>%
  summarise(tempMAX = max(temp_max, na.rm = TRUE),Month_Year = first(Month_Year) )

TempMAXByDay <-TempMAXByDay[order(as.Date( TempMINByDay$data, format="%d/%m/%Y")),]

#View (TempMAXByDay)

TempMAXByMonth = TempMAXByDay %>%
  group_by(Month_Year) %>%
  summarise(tempMAX = mean(tempMAX, na.rm = TRUE))


TempMAXByMonth <-TempMAXByMonth[order(as.Date( TempMAXByMonth$Month_Year, format="%m/%Y")),]
#View(TempMAXByMonth)




#-------------------------------------------------------------------------
#Variable 5 Cloudiness (clouds) CloudsAVGByDay // CloudsAVGByMonth
#-------------------------------------------------------------------------

CloudsAVGByDay  = myData %>%
  group_by(data) %>%
  summarise(clouds_all = mean(clouds_all, na.rm = TRUE),Month_Year = first(Month_Year) )



CloudsAVGByDay <-CloudsAVGByDay[order(as.Date( CloudsAVGByDay$data, format="%d/%m/%Y")),]



CloudsAVGByMonth = CloudsAVGByDay %>%
  group_by(Month_Year) %>%
  summarise(clouds_all = mean(clouds_all, na.rm = TRUE))


CloudsAVGByMonth <-CloudsAVGByMonth[order(as.Date( CloudsAVGByMonth$Month_Year, format="%m/%Y")),]




#-------------------------------------------------------------------------
#Variable 6 Humidade(humidity) - HumidityAVGByDay // HumidityAVGByMonth
#-------------------------------------------------------------------------


HumidityAVGByDay  = myData %>%
  group_by(data) %>%
  summarise(humidity = mean(humidity, na.rm = TRUE),Month_Year = first(Month_Year) )

#View (HumidityAVGByDay)

HumidityAVGByDay <-HumidityAVGByDay[order(as.Date( HumidityAVGByDay$data, format="%d/%m/%Y")),]



HumidityAVGByMonth = HumidityAVGByDay %>%
  group_by(Month_Year) %>%
  summarise(humidity = mean(humidity, na.rm = TRUE))


HumidityAVGByMonth <-HumidityAVGByMonth[order(as.Date( HumidityAVGByMonth$Month_Year, format="%m/%Y")),]
#View(HumidityAVGByMonth)


#-------------------------------------------------------------------------
#Variable 7 15 Setembro a 15 Outubro com Chuva  vector_TotRainSeptemberOctober
#-------------------------------------------------------------------------
#View(myData)
#vector with year list
#yearList = summarise(myData, years=year)

yearList = unique(myData$year, incomparables = FALSE)
#View(yearList)
#dfyear <- subset(myData,year=='1980')
#View(dfyear)

vector_TotRainSeptemberOctober= c()

for (Each_year in yearList){
  
  if (Each_year<1985 || Each_year>=2018){
    next
  }
  #print (Each_year)
  dfyear <- subset(myData,year==Each_year)
  dfMonthSpetember <- subset(dfyear,month=='09')
  dfMontOctober <- subset(dfyear,month=='10')
  dfMontSpetember15up <- subset(dfMonthSpetember,day>='15')
  dfMontOctober15down <- subset(dfMontOctober,day<='15')
  
  dfMonthSeptemberOctober <- rbind(dfMontSpetember15up, dfMontSpetember15up)
  
  TotRainSeptemberOctober <- sum(dfMonthSeptemberOctober$rain_1h,na.rm = TRUE)
  #print (TotRainSeptemberOctober)
    vector_TotRainSeptemberOctober <- append(vector_TotRainSeptemberOctober, TotRainSeptemberOctober, after=length(vector_TotRainSeptemberOctober))
}
#View(dfMontSpetember15up)
#View (vector_TotRainSeptemberOctober)


#-------------------------------------------------------------------------
#Variable 8 Abril com 100mm de Chuva (50~150) vector_TotRainAbrilBetween
#-------------------------------------------------------------------------
#View(myData)

yearList = unique(myData$year, incomparables = FALSE)

vector_TotRainAbrilBetween= c()

for (Each_year in yearList){
  
  if (Each_year<1986 || Each_year>=2019){
    next
  }
  dfyear <- subset(myData,year==Each_year) 
  dfMonthApril <- subset(dfyear,month=='04')
  TotRainApril <- sum(dfMonthApril$rain_1h,na.rm = TRUE)
  
  #print (TotRainApril)
  if (   TotRainApril>50 &&  TotRainApril<150)  {
    vector_TotRainAbrilBetween <- append(vector_TotRainAbrilBetween, TRUE, after=length(vector_TotRainAbrilBetween)) 
  }
  else{
    vector_TotRainAbrilBetween <- append(vector_TotRainAbrilBetween, FALSE, after=length(vector_TotRainAbrilBetween))     
  }
}

#View (vector_TotRainAbrilBetween)

#-------------------------------------------------------------------------
#Variable 9 Maio com Chuva vector_RainMay (> 5mm)
#-------------------------------------------------------------------------

yearList = unique(myData$year, incomparables = FALSE)

vector_RainMay= c()

for (Each_year in yearList){
  
  if (Each_year<1986 || Each_year>=2019){
    next
  }
  dfyear <- subset(myData,year==Each_year) 
  dfMonthMay <- subset(dfyear,month=='05') 
  TotRainMay <- sum(dfMonthMay$rain_1h,na.rm = TRUE)
  #print (TotRainMay)
  
  if (TotRainMay>5)  {
    vector_RainMay <- append(vector_RainMay, TRUE, after=length(vector_RainMay)) 
  }
  else{
    vector_RainMay <- append(vector_RainMay, FALSE, after=length(vector_RainMay))     
  }  
  
}
# View (vector_RainMay)  

#-------------------------------------------------------------------------
#Variable 10 Junho sem Chuva vector_RainJune (> 0mm)
#-------------------------------------------------------------------------

yearList = unique(myData$year, incomparables = FALSE)

vector_RainJune= c()

for (Each_year in yearList){
  
  if (Each_year<1986 || Each_year>=2019){
    next
  }
  dfyear <- subset(myData,year==Each_year) 
  dfMonthJune <- subset(dfyear,month=='06') 
  TotRainJune <- sum(dfMonthJune$rain_1h,na.rm = TRUE)
  #print (TotRainJune)
  
  if (TotRainJune == 0)  {
    vector_RainJune <- append(vector_RainJune, TRUE, after=length(vector_RainJune)) 
  }
  else{
    vector_RainJune <- append(vector_RainJune, FALSE, after=length(vector_RainJune))     
  }  
  
}
 #View (vector_RainJune) 


#-------------------------------------------------------------------------
#Variable 11 dias Março e Abril entre 15º e 20º TotDaysMArsApril15to20
#-------------------------------------------------------------------------
# 
 yearList = unique(myData$year, incomparables = FALSE)
 
TotDaysMArsApril15to20= c()
 
 for (Each_year in yearList){
   
   if (Each_year<1986 || Each_year>=2019){
     next
   }
  
   dfyear <- subset(myData,year==Each_year) 
   dfMonthMars <- subset(dfyear,month=='03')
   dfMonthApril <- subset(dfyear,month=='04')
   
   dfMonthMarsApril <- rbind(dfMonthMars, dfMonthApril)
   
  TempAVGByDay  = dfMonthMarsApril %>%
   group_by(data) %>%
   summarise(tempAVG = mean(temp, na.rm = TRUE),Month_Year = first(Month_Year) )
#  
#   # TempAVGByDay <-TempAVGByDay[order(as.Date( TempAVGByDay$data, format="%d/%m/%Y")),]
  
  

 
    df1 = subset(TempAVGByDay,subset=TempAVGByDay$tempAVG >= 15 )
    df2 = subset(df1,subset=df1$tempAVG <= 20 ) 
     

    TotDaysMArsApril15to20 <- append(TotDaysMArsApril15to20, nrow(df2), after=length(TotDaysMArsApril15to20))  
 
#   
#  # dfMonthMarsApril <- subset(dfyear,month=='03') nrow(dataset)
#  
#   
  
 }
#  
#View(TotDaysMArsApril15to20)




#-------------------------------------------------------------------------
#Variable 12 dias Maio e Junho entre 16º e 20º TotDaysMayJune16to20
#-------------------------------------------------------------------------
# 
yearList = unique(myData$year, incomparables = FALSE)

TotDaysMayJune16to20= c()

for (Each_year in yearList){
  
  if (Each_year<1986 || Each_year>=2019){
    next
  }
  
  dfyear <- subset(myData,year==Each_year) 
  dfMonthMay <- subset(dfyear,month=='05')
  dfMonthJune <- subset(dfyear,month=='06')
  
  dfMonthMayJune <- rbind(dfMonthMay, dfMonthJune)
  
  TempAVGByDay  = dfMonthMayJune %>%
    group_by(data) %>%
    summarise(tempAVG = mean(temp, na.rm = TRUE),Month_Year = first(Month_Year) )
  #  
  #   # TempAVGByDay <-TempAVGByDay[order(as.Date( TempAVGByDay$data, format="%d/%m/%Y")),]
  
  
  
  
  df1 = subset(TempAVGByDay,subset=TempAVGByDay$tempAVG >= 16 )
  df2 = subset(df1,subset=df1$tempAVG <= 20 ) 
  
  
  TotDaysMayJune16to20 <- append(TotDaysMayJune16to20, nrow(df2), after=length(TotDaysMayJune16to20))  
  
  #   
  #  # dfMonthMarsApril <- subset(dfyear,month=='03') nrow(dataset)
  #  
  #   
  
}
 
#  View(TotDaysMayJune16to20)


 #-------------------------------------------------------------------------
 #Variable 13 horas Maio e Junho > 30º e vento de Este TotHoursMayJuneOver30WindEast
 #-------------------------------------------------------------------------
 
 yearList = unique(myData$year, incomparables = FALSE)
 
TotHoursMayJuneOver30WindEast= c()
 
 for (Each_year in yearList){
   
   if (Each_year<1986 || Each_year>=2019){
     next
   }
 
   dfyear <- subset(myData,year==Each_year) 
   dfMonthMay <- subset(dfyear,month=='05')
   dfMonthJune <- subset(dfyear,month=='06')
   
   dfMonthMayJune <- rbind(dfMonthMay, dfMonthJune) 
   
   dfWindUpper78 <- subset(dfMonthMayJune,wind_deg > 78)
   dfWindDown102 <- subset(dfWindUpper78,wind_deg < 102)

   
#   TempAVGByDay  = dfWindDown102 %>%
#     group_by(data) %>%
#     summarise(tempAVG = mean(temp, na.rm = TRUE),Month_Year = first(Month_Year) )
   
   
#   df1 = subset(TempAVGByDay,subset=TempAVGByDay$tempAVG >= 30 )
   df1 = subset(dfWindDown102,subset=dfWindDown102$temp_max >= 30 )
   
   TotHoursMayJuneOver30WindEast <- append(TotHoursMayJuneOver30WindEast, nrow(df1), after=length(TotHoursMayJuneOver30WindEast))  
   
   
 }
 

#View(TotHoursMayJuneOver30WindEast)
  
  #-------------------------------------------------------------------------
  #Variable 14 horas Maio e Junho > 30º  TotHoursMayJuneOver30 
  #-------------------------------------------------------------------------
  
  yearList = unique(myData$year, incomparables = FALSE)
  
  TotHoursMayJuneOver30= c()
  
  for (Each_year in yearList){
    
    if (Each_year<1986 || Each_year>=2019){
      next
    }
    
    dfyear <- subset(myData,year==Each_year) 
    dfMonthMay <- subset(dfyear,month=='05')
    dfMonthJune <- subset(dfyear,month=='06')
    
    dfMonthMayJune <- rbind(dfMonthMay, dfMonthJune) 
    
    
    
    #   TempAVGByDay  = dfWindDown102 %>%
    #     group_by(data) %>%
    #     summarise(tempAVG = mean(temp, na.rm = TRUE),Month_Year = first(Month_Year) )
    
    
    #   df1 = subset(TempAVGByDay,subset=TempAVGByDay$tempAVG >= 30 )
    df1 = subset(dfMonthMayJune,subset=dfMonthMayJune$temp_max >= 30 )
    
    TotHoursMayJuneOver30 <- append(TotHoursMayJuneOver30, nrow(df1), after=length(TotHoursMayJuneOver30))  
    
    
  }
  
#View(TotHoursMayJuneOver30)
  
  
  #-------------------------------------------------------------------------
  #Variable 15 horas Dezembro <0º  TotHoursDezemberUnder0 
  #-------------------------------------------------------------------------
  
  yearList = unique(myData$year, incomparables = FALSE)
  
  TotHoursDezemberUnder0= c()
  
  for (Each_year in yearList){
    
    if (Each_year<1985 || Each_year>=2018){
      next
    }
    
    dfyear <- subset(myData,year==Each_year) 
    dfMonthDezember <- subset(dfyear,month=='12')
 
    
    df1 = subset(dfMonthDezember,subset=dfMonthDezember$temp_min < 0 )
    
    TotHoursDezemberUnder0 <- append(TotHoursDezemberUnder0, nrow(df1), after=length(TotHoursDezemberUnder0))  
    
    
  }
  
  
  #-------------------------------------------------------------------------
  #Variable 16 horas January <0º  TotHoursJanuaryUnder0 
  #-------------------------------------------------------------------------
  
  yearList = unique(myData$year, incomparables = FALSE)
  
  TotHoursJanuaryUnder0= c()
  
  for (Each_year in yearList){
    
    if (Each_year<1986 || Each_year>=2019){
      next
    }
    
    dfyear <- subset(myData,year==Each_year) 
    dfMonthJanuary <- subset(dfyear,month=='01')
    
    
    df1 = subset(dfMonthJanuary,subset=dfMonthJanuary$temp_min < 0 )
    
    TotHoursJanuaryUnder0 <- append(TotHoursJanuaryUnder0, nrow(df1), after=length(TotHoursJanuaryUnder0))  
    
    
  }
  

  #-------------------------------------------------------------------------
  #Variable 17 horas February <0º  TotHoursFebruaryUnder0 
  #-------------------------------------------------------------------------
  
  yearList = unique(myData$year, incomparables = FALSE)
  
  TotHoursFebruaryUnder0= c()
  
  for (Each_year in yearList){
    
    if (Each_year<1986 || Each_year>=2019){
      next
    }
    
    dfyear <- subset(myData,year==Each_year) 
    dfMonthFebruary <- subset(dfyear,month=='02')
    
    
    df1 = subset(dfMonthFebruary,subset=dfMonthFebruary$temp_min < 0 )
    
    TotHoursFebruaryUnder0 <- append(TotHoursFebruaryUnder0, nrow(df1), after=length(TotHoursFebruaryUnder0))  
    
    
  }
  
  
  
  #-------------------------------------------------------------------------
  #Variable 18 horas March <0º  TotHoursMarchUnder0 
  #-------------------------------------------------------------------------
  
  yearList = unique(myData$year, incomparables = FALSE)
  
  TotHoursMarchUnder0= c()
  
  for (Each_year in yearList){
    
    if (Each_year<1986 || Each_year>=2019){
      next
    }
    
    dfyear <- subset(myData,year==Each_year) 
    dfMonthMarch <- subset(dfyear,month=='03')
    
    
    df1 = subset(dfMonthMarch,subset=dfMonthMarch$temp_min < 0 )
    
    TotHoursMarchUnder0 <- append(TotHoursMarchUnder0, nrow(df1), after=length(TotHoursMarchUnder0))  
    
    
  }
  

 
  
  #----------------------------------------------------------------------------
  #Dataframe for analysis
  #
  #Variable list
  #01 - year
  #02 - Wheat Productivity (kg/ha)
  #03 - Rain September (mm) ano -1
  #04 - Rain October (mm) ano -1
  #05 - Rain November (mm) ano -1 
  #06 - Rain December (mm) ano -1    
  #07 - Rain January (mm) ano    
  #08 - Rain February (mm) ano 
  #09 - Rain March (mm) ano 
  #10 - Rain April (mm) ano  
  #11 - Rain May (mm) ano    
  #12 - Rain June (mm) ano   
  #13 - Temperature Average September (º) ano -1
  #14 - Temperature Average October (º) ano -1
  #15 - Temperature Average November (º) ano -1  
  #16 - Temperature Average December (º) ano -1 
  #17 - Temperature Average January (º) ano    
  #18 - Temperature Average February (º) ano 
  #19 - Temperature Average March (º) ano 
  #20 - Temperature Average April (º) ano  
  #21 - Temperature Average May (º) ano    
  #22 - Temperature Average June (º) ano   
  #23 - Temperature Min September (º) ano -1
  #24 - Temperature Min October (º) ano -1
  #25 - Temperature Min November (º) ano -1  
  #26 - Temperature Min December (º) ano -1 
  #27 - Temperature Min January (º) ano    
  #28 - Temperature Min February (º) ano 
  #29 - Temperature Min March (º) ano 
  #30 - Temperature Min April (º) ano  
  #31 - Temperature Min May (º) ano    
  #32 - Temperature Min June (º) ano  
  #33 - Temperature Max September (º) ano -1
  #34 - Temperature Max October (º) ano -1
  #35 - Temperature Max November (º) ano -1  
  #36 - Temperature Max December (º) ano -1 
  #37 - Temperature Max January (º) ano    
  #38 - Temperature Max February (º) ano 
  #39 - Temperature Max March (º) ano 
  #40 - Temperature Max April (º) ano  
  #41 - Temperature Max May (º) ano    
  #42 - Temperature Max June (º) ano   
  #43 - Humidity September (º) ano -1
  #44 - Humidity October (º) ano -1
  #45 - Humidity November (º) ano -1  
  #46 - Humidity December (º) ano -1 
  #47 - Humidity January (º) ano    
  #48 - Humidity February (º) ano 
  #49 - Humidity March (º) ano 
  #50 - Humidity April (º) ano  
  #51 - Humidity May (º) ano    
  #52 - Humidity June (º) ano   
  #53 - Clouds December (%) ano -1
  #54 - Clouds January (%) ano  
  #55 - Clouds February (%) ano  
  #56 - Clouds March (%) ano    
  #57 - Clouds April (%) ano  
  #58 - Clouds May (%) ano  
  #59 - Clouds June (%) ano   
  #60 - April with 100mm of Rain (Between 50 e 150mm)
  #61 - May with Rain
  #62 - June without Rain
  #63 - Total days of March and April Temperature between 15 and 20º
  #64 - Total days of May and June Temperature between 16 and 20º  
  #65 - Total days of May and June Temperature over 30º Wind from East
  #66 - Total days of May and June Temperature over 30º  
  #67 - Total Hours of December Temperature under 0º
  #68 - Total Hours of January Temperature under 0º 
  #69 - Total Hours of February Temperature under 0º 
  
  #----------------------------------------------------------------------------
  

  
  #Variable03 - Rain September
  #View (RainByMonth)
  library(stringr)
  dfRainMonthSeptember <- RainByMonth %>% filter(str_detect(Month_Year, "09/"))
  dfRainMonthSeptember <- dfRainMonthSeptember %>% slice(7:39)
  dfRainMonthSeptember <- dfRainMonthSeptember[,2]
 
  #Variable04 - Rain October
  dfRainMonthOctober <- RainByMonth %>% filter(str_detect(Month_Year, "10/"))
  dfRainMonthOctober <- dfRainMonthOctober %>% slice(7:39)
  dfRainMonthOctober <- dfRainMonthOctober[,2]  
  

  #Variable05 - Rain November
  dfRainMonthNovember <- RainByMonth %>% filter(str_detect(Month_Year, "11/"))
  dfRainMonthNovember <- dfRainMonthNovember %>% slice(7:39)
  dfRainMonthNovember <- dfRainMonthNovember[,2]   
  
  #Variable06 - Rain December
  dfRainMonthDecember <- RainByMonth %>% filter(str_detect(Month_Year, "12/"))
  dfRainMonthDecember <- dfRainMonthDecember %>% slice(7:39)
  dfRainMonthDecember <- dfRainMonthDecember[,2]    
  
  #Variable07 - Rain January
  dfRainMonthJanuary <- RainByMonth %>% filter(str_detect(Month_Year, "01/"))
  dfRainMonthJanuary <- dfRainMonthJanuary %>% slice(8:40)
  dfRainMonthJanuary <- dfRainMonthJanuary[,2]     
  
  #Variable08 - Rain February
  dfRainMonthFebruary <- RainByMonth %>% filter(str_detect(Month_Year, "02/"))
  dfRainMonthFebruary <- dfRainMonthFebruary %>% slice(8:40)
  dfRainMonthFebruary <- dfRainMonthFebruary[,2]     
  
  #Variable09 - Rain March
  dfRainMonthMarch<- RainByMonth %>% filter(str_detect(Month_Year, "03/"))
  dfRainMonthMarch <- dfRainMonthMarch %>% slice(8:40)
  dfRainMonthMarch <- dfRainMonthMarch[,2]       
  
  #Variable10 - Rain April
  dfRainMonthApril<- RainByMonth %>% filter(str_detect(Month_Year, "04/"))
  dfRainMonthApril <- dfRainMonthApril %>% slice(8:40)
  dfRainMonthApril <- dfRainMonthApril[,2]    
  
  #Variable11 - Rain May  
  dfRainMonthMay <- RainByMonth %>% filter(str_detect(Month_Year, "05/"))
  dfRainMonthMay <- dfRainMonthMay %>% slice(8:40)
  dfRainMonthMay <- dfRainMonthMay[,2]    
  
  #Variable11 - Rain June  
  dfRainMonthJune <- RainByMonth %>% filter(str_detect(Month_Year, "06/"))
  dfRainMonthJune <- dfRainMonthJune %>% slice(8:40)
  dfRainMonthJune <- dfRainMonthJune[,2]   
  
  
  #Variable 12 - Temperature Average September (º) 
  dfTempAVGMonthSeptember <- TempAVGByMonth %>% filter(str_detect(Month_Year, "09/"))
  dfTempAVGMonthSeptember <- dfTempAVGMonthSeptember %>% slice(7:39)
  dfTempAVGMonthSeptember <- dfTempAVGMonthSeptember[,2]  
  
  #Variable13 - Temperature Average October (º) 
  dfTempAVGMonthOctober <- TempAVGByMonth %>% filter(str_detect(Month_Year, "10/"))
  dfTempAVGMonthOctober <- dfTempAVGMonthOctober %>% slice(7:39)
  dfTempAVGMonthOctober <- dfTempAVGMonthOctober[,2]  
  
 
  #Variable14 - Temperature Average November (º) 
  dfTempAVGMonthNovember <- TempAVGByMonth %>% filter(str_detect(Month_Year, "11/"))
  dfTempAVGMonthNovember <- dfTempAVGMonthNovember %>% slice(7:39)
  dfTempAVGMonthNovember <- dfTempAVGMonthNovember[,2]    
  

  #Variable15 - Temperature Average December (º) 
  dfTempAVGMonthDecember <- TempAVGByMonth %>% filter(str_detect(Month_Year, "12/"))
  dfTempAVGMonthDecember <- dfTempAVGMonthDecember %>% slice(7:39)
  dfTempAVGMonthDecember <- dfTempAVGMonthDecember[,2]    
  
  #Variable16 - Temperature Average January (º) 
  dfTempAVGMonthJanuary <- TempAVGByMonth %>% filter(str_detect(Month_Year, "01/"))
  dfTempAVGMonthJanuary <- dfTempAVGMonthJanuary %>% slice(8:40)
  dfTempAVGMonthJanuary <- dfTempAVGMonthJanuary[,2]  
  
  #Variable17 - Temperature Average February (º) 
  dfTempAVGMonthFebruary <- TempAVGByMonth %>% filter(str_detect(Month_Year, "02/"))
  dfTempAVGMonthFebruary <- dfTempAVGMonthFebruary %>% slice(8:40)
  dfTempAVGMonthFebruary <- dfTempAVGMonthFebruary[,2]   
  
  
  #Variable18 - Temperature Average March (º) 
  dfTempAVGMonthMarch<- TempAVGByMonth %>% filter(str_detect(Month_Year, "03/"))
  dfTempAVGMonthMarch <- dfTempAVGMonthMarch %>% slice(8:40)
  dfTempAVGMonthMarch <- dfTempAVGMonthMarch[,2]   
  
  
  #Variable19 - Temperature Average April (º) 
  dfTempAVGMonthApril<- TempAVGByMonth %>% filter(str_detect(Month_Year, "04/"))
  dfTempAVGMonthApril <- dfTempAVGMonthApril %>% slice(8:40)
  dfTempAVGMonthApril <- dfTempAVGMonthApril[,2]  
  
  
  #Variable20 - Temperature Average May (º) 
  dfTempAVGMonthMay <- TempAVGByMonth %>% filter(str_detect(Month_Year, "05/"))
  dfTempAVGMonthMay <- dfTempAVGMonthMay %>% slice(8:40)
  dfTempAVGMonthMay <- dfTempAVGMonthMay[,2]    
  
  #Variable21 - Temperature Average June (º)   
  dfTempAVGMonthJune <- TempAVGByMonth %>% filter(str_detect(Month_Year, "06/"))
  dfTempAVGMonthJune <- dfTempAVGMonthJune %>% slice(8:40)
  dfTempAVGMonthJune <- dfTempAVGMonthJune[,2]  
  

  #Variable 22 - Temperature Min September (º) 
  dfTempMinMonthSeptember <- TempMINByMonth %>% filter(str_detect(Month_Year, "09/"))
  dfTempMinMonthSeptember <- dfTempMinMonthSeptember %>% slice(7:39)
  dfTempMinMonthSeptember <- dfTempMinMonthSeptember[,2]  
  
  #Variable23 - Temperature Min October (º) 
  dfTempMinMonthOctober <- TempMINByMonth %>% filter(str_detect(Month_Year, "10/"))
  dfTempMinMonthOctober <- dfTempMinMonthOctober %>% slice(7:39)
  dfTempMinMonthOctober <- dfTempMinMonthOctober[,2]  
  
  
  #Variable24 - Temperature Min November (º) 
  dfTempMinMonthNovember <- TempMINByMonth %>% filter(str_detect(Month_Year, "11/"))
  dfTempMinMonthNovember <- dfTempMinMonthNovember %>% slice(7:39)
  dfTempMinMonthNovember <- dfTempMinMonthNovember[,2]    
  
  
  #Variable25 - Temperature Min December (º) 
  dfTempMinMonthDecember <- TempMINByMonth %>% filter(str_detect(Month_Year, "12/"))
  dfTempMinMonthDecember <- dfTempMinMonthDecember %>% slice(7:39)
  dfTempMinMonthDecember <- dfTempMinMonthDecember[,2]    
  
  #Variable26 - Temperature Min January (º) 
  dfTempMinMonthJanuary <- TempMINByMonth %>% filter(str_detect(Month_Year, "01/"))
  dfTempMinMonthJanuary <- dfTempMinMonthJanuary %>% slice(8:40)
  dfTempMinMonthJanuary <- dfTempMinMonthJanuary[,2]  
  
  #Variable27 - Temperature Min February (º) 
  dfTempMinMonthFebruary <- TempMINByMonth %>% filter(str_detect(Month_Year, "02/"))
  dfTempMinMonthFebruary <- dfTempMinMonthFebruary %>% slice(8:40)
  dfTempMinMonthFebruary <- dfTempMinMonthFebruary[,2]   
  
  
  #Variable28 - Temperature Min March (º) 
  dfTempMinMonthMarch<- TempMINByMonth %>% filter(str_detect(Month_Year, "03/"))
  dfTempMinMonthMarch <- dfTempMinMonthMarch %>% slice(8:40)
  dfTempMinMonthMarch <- dfTempMinMonthMarch[,2]   
  
  
  #Variable29 - Temperature Min April (º) 
  dfTempMinMonthApril<- TempMINByMonth %>% filter(str_detect(Month_Year, "04/"))
  dfTempMinMonthApril <- dfTempMinMonthApril %>% slice(8:40)
  dfTempMinMonthApril <- dfTempMinMonthApril[,2]  
  
  
  #Variable30 - Temperature Min May (º) 
  dfTempMinMonthMay <- TempMINByMonth %>% filter(str_detect(Month_Year, "05/"))
  dfTempMinMonthMay <- dfTempMinMonthMay %>% slice(8:40)
  dfTempMinMonthMay <- dfTempMinMonthMay[,2]    
  
  #Variable31 - Temperature Min June (º)   
  dfTempMinMonthJune <- TempMINByMonth %>% filter(str_detect(Month_Year, "06/"))
  dfTempMinMonthJune <- dfTempMinMonthJune %>% slice(8:40)
  dfTempMinMonthJune <- dfTempMinMonthJune[,2]     
  
  #Variable32 - Temperature Max September (º) 
  dfTempMaxMonthSeptember <- TempMAXByMonth %>% filter(str_detect(Month_Year, "09/"))
  dfTempMaxMonthSeptember <- dfTempMaxMonthSeptember %>% slice(7:39)
  dfTempMaxMonthSeptember <- dfTempMaxMonthSeptember[,2]  
  

  
  #Variable33 - Temperature Max October (º) 
  dfTempMaxMonthOctober <- TempMAXByMonth %>% filter(str_detect(Month_Year, "10/"))
  dfTempMaxMonthOctober <- dfTempMaxMonthOctober %>% slice(7:39)
  dfTempMaxMonthOctober <- dfTempMaxMonthOctober[,2]  
  
 
  #Variable34 - Temperature Max November (º) 
  dfTempMaxMonthNovember <- TempMAXByMonth %>% filter(str_detect(Month_Year, "11/"))
  dfTempMaxMonthNovember <- dfTempMaxMonthNovember %>% slice(7:39)
  dfTempMaxMonthNovember <- dfTempMaxMonthNovember[,2]    
  
  
  #Variable35 - Temperature Max December (º) 
  dfTempMaxMonthDecember <- TempMAXByMonth %>% filter(str_detect(Month_Year, "12/"))
  dfTempMaxMonthDecember <- dfTempMaxMonthDecember %>% slice(7:39)
  dfTempMaxMonthDecember <- dfTempMaxMonthDecember[,2]    
  
  #Variable36 - Temperature Max January (º) 
  dfTempMaxMonthJanuary <- TempMAXByMonth %>% filter(str_detect(Month_Year, "01/"))
  dfTempMaxMonthJanuary <- dfTempMaxMonthJanuary %>% slice(8:40)
  dfTempMaxMonthJanuary <- dfTempMaxMonthJanuary[,2]  
  
  #Variable37 - Temperature Max February (º) 
  dfTempMaxMonthFebruary <- TempMAXByMonth %>% filter(str_detect(Month_Year, "02/"))
  dfTempMaxMonthFebruary <- dfTempMaxMonthFebruary %>% slice(8:40)
  dfTempMaxMonthFebruary <- dfTempMaxMonthFebruary[,2]   
  
  
  #Variable38 - Temperature Max March (º) 
  dfTempMaxMonthMarch<- TempMAXByMonth %>% filter(str_detect(Month_Year, "03/"))
  dfTempMaxMonthMarch <- dfTempMaxMonthMarch %>% slice(8:40)
  dfTempMaxMonthMarch <- dfTempMaxMonthMarch[,2]   
  
  
  #Variable39 - Temperature Max April (º) 
  dfTempMaxMonthApril<- TempMAXByMonth %>% filter(str_detect(Month_Year, "04/"))
  dfTempMaxMonthApril <- dfTempMaxMonthApril %>% slice(8:40)
  dfTempMaxMonthApril <- dfTempMaxMonthApril[,2]  
  
  
  #Variable40 - Temperature Max May (º) 
  dfTempMaxMonthMay <- TempMAXByMonth %>% filter(str_detect(Month_Year, "05/"))
  dfTempMaxMonthMay <- dfTempMaxMonthMay %>% slice(8:40)
  dfTempMaxMonthMay <- dfTempMaxMonthMay[,2]    
  
  #Variable41 - Temperature Max June (º)   
  dfTempMaxMonthJune <- TempMAXByMonth %>% filter(str_detect(Month_Year, "06/"))
  dfTempMaxMonthJune <- dfTempMaxMonthJune %>% slice(8:40)
  dfTempMaxMonthJune <- dfTempMaxMonthJune[,2]    
  
  
  
  #Variable42 - humidity September (º) 
  dfHumidityMonthSeptember <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "09/"))
  dfHumidityMonthSeptember <- dfHumidityMonthSeptember %>% slice(7:39)
  dfHumidityMonthSeptember <- dfHumidityMonthSeptember[,2]  
  
  
  
  #Variable43 - humidity October (º) 
  dfHumidityMonthOctober <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "10/"))
  dfHumidityMonthOctober <- dfHumidityMonthOctober %>% slice(7:39)
  dfHumidityMonthOctober <- dfHumidityMonthOctober[,2]  
  
  
  #Variable44 - humidity November (º) 
  dfHumidityMonthNovember <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "11/"))
  dfHumidityMonthNovember <- dfHumidityMonthNovember %>% slice(7:39)
  dfHumidityMonthNovember <- dfHumidityMonthNovember[,2]    
  
  
  #Variable45 - humidity December (º) 
  dfHumidityMonthDecember <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "12/"))
  dfHumidityMonthDecember <- dfHumidityMonthDecember %>% slice(7:39)
  dfHumidityMonthDecember <- dfHumidityMonthDecember[,2]    
  
  #Variable46 - humidity January (º) 
  dfHumidityMonthJanuary <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "01/"))
  dfHumidityMonthJanuary <- dfHumidityMonthJanuary %>% slice(8:40)
  dfHumidityMonthJanuary <- dfHumidityMonthJanuary[,2]  
  
  #Variable47 - humidity February (º) 
  dfHumidityMonthFebruary <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "02/"))
  dfHumidityMonthFebruary <- dfHumidityMonthFebruary %>% slice(8:40)
  dfHumidityMonthFebruary <- dfHumidityMonthFebruary[,2]   
  
  
  #Variable48 - humidity March (º) 
  dfHumidityMonthMarch<- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "03/"))
  dfHumidityMonthMarch <- dfHumidityMonthMarch %>% slice(8:40)
  dfHumidityMonthMarch <- dfHumidityMonthMarch[,2]   
  
  
  #Variable49 - humidity April (º) 
  dfHumidityMonthApril<- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "04/"))
  dfHumidityMonthApril <- dfHumidityMonthApril %>% slice(8:40)
  dfHumidityMonthApril <- dfHumidityMonthApril[,2]  
  
  
  #Variable50 - humidity May (º) 
  dfHumidityMonthMay <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "05/"))
  dfHumidityMonthMay <- dfHumidityMonthMay %>% slice(8:40)
  dfHumidityMonthMay <- dfHumidityMonthMay[,2]    
  
  #Variable51 - humidity June (º)   
  dfHumidityMonthJune <- HumidityAVGByMonth %>% filter(str_detect(Month_Year, "06/"))
  dfHumidityMonthJune <- dfHumidityMonthJune %>% slice(8:40)
  dfHumidityMonthJune <- dfHumidityMonthJune[,2]    
  
  
  #Variable52 - Clouds December (%) ano -1
  dfCloudsMonthDecember <- CloudsAVGByMonth %>% filter(str_detect(Month_Year, "12/"))
  dfCloudsMonthDecember <- dfCloudsMonthDecember %>% slice(7:39)
  dfCloudsMonthDecember <- dfCloudsMonthDecember[,2]    
  
 

  #Variable53 - Clouds January (%) ano  
  dfCloudsMonthJanuary <- CloudsAVGByMonth %>% filter(str_detect(Month_Year, "01/"))
  dfCloudsMonthJanuary <- dfCloudsMonthJanuary %>% slice(8:40)
  dfCloudsMonthJanuary <- dfCloudsMonthJanuary[,2]  
  
  #Variable54 - Clouds February (%) ano 
  dfCloudsMonthFebruary <- CloudsAVGByMonth %>% filter(str_detect(Month_Year, "02/"))
  dfCloudsMonthFebruary <- dfCloudsMonthFebruary %>% slice(8:40)
  dfCloudsMonthFebruary <- dfCloudsMonthFebruary[,2]   
  
  #Variable55 - Clouds March (%) ano  
  dfCloudsMonthMarch<- CloudsAVGByMonth %>% filter(str_detect(Month_Year, "03/"))
  dfCloudsMonthMarch <- dfCloudsMonthMarch %>% slice(8:40)
  dfCloudsMonthMarch <- dfCloudsMonthMarch[,2]   
    
  #Variable56 - Clouds April (%) ano  
  dfCloudsMonthApril<- CloudsAVGByMonth %>% filter(str_detect(Month_Year, "04/"))
  dfCloudsMonthApril <- dfCloudsMonthApril %>% slice(8:40)
  dfCloudsMonthApril <- dfCloudsMonthApril[,2]  
  
  #Variable57 - Clouds May (%) ano 
  dfCloudsMonthMay <- CloudsAVGByMonth %>% filter(str_detect(Month_Year, "05/"))
  dfCloudsMonthMay <- dfCloudsMonthMay %>% slice(8:40)
  dfCloudsMonthMay <- dfCloudsMonthMay[,2] 
  
  #Variable58 - Clouds June (%) ano  
  dfCloudsMonthJune <- CloudsAVGByMonth %>% filter(str_detect(Month_Year, "06/"))
  dfCloudsMonthJune <- dfCloudsMonthJune %>% slice(8:40)
  dfCloudsMonthJune <- dfCloudsMonthJune[,2]     
  
  
  
  
  df_Analysis <- data.frame(years,WheatProduction, dfRainMonthSeptember, dfRainMonthOctober,
                            dfRainMonthNovember, dfRainMonthDecember, dfRainMonthJanuary, 
                            dfRainMonthFebruary, dfRainMonthMarch, dfRainMonthApril,
                            dfRainMonthMay,dfRainMonthJune, dfTempAVGMonthSeptember,
                            dfTempAVGMonthOctober, dfTempAVGMonthNovember, dfTempAVGMonthDecember,
                            dfTempAVGMonthJanuary, dfTempAVGMonthFebruary, dfTempAVGMonthMarch, 
                            dfTempAVGMonthApril, dfTempAVGMonthMay, dfTempAVGMonthJune,
                            dfTempMinMonthSeptember,dfTempMinMonthOctober,dfTempMinMonthNovember,
                            dfTempMinMonthDecember,dfTempMinMonthJanuary, dfTempMinMonthFebruary,
                            dfTempMinMonthMarch, dfTempMinMonthApril, dfTempMinMonthMay,
                            dfTempMinMonthJune,dfTempMaxMonthSeptember,dfTempMaxMonthOctober,
                            dfTempMaxMonthNovember,dfTempMaxMonthDecember,dfTempMaxMonthJanuary,
                            dfTempMaxMonthFebruary,dfTempMaxMonthMarch, dfTempMaxMonthApril,
                            dfTempMaxMonthMay,dfTempMaxMonthJune,dfHumidityMonthSeptember,
                            dfHumidityMonthOctober,dfHumidityMonthNovember,dfHumidityMonthDecember,
                            dfHumidityMonthJanuary,dfHumidityMonthFebruary,dfHumidityMonthMarch,
                            dfHumidityMonthApril,dfHumidityMonthMay,dfHumidityMonthJune,
                            dfCloudsMonthDecember,dfCloudsMonthJanuary,dfCloudsMonthFebruary,
                            dfCloudsMonthMarch,dfCloudsMonthApril,dfCloudsMonthMay,dfCloudsMonthJune,
                            vector_TotRainSeptemberOctober, vector_TotRainAbrilBetween, vector_RainMay,
                            vector_RainJune, TotDaysMArsApril15to20, TotDaysMayJune16to20,
                            TotHoursMayJuneOver30WindEast, TotHoursMayJuneOver30, TotHoursDezemberUnder0,
                            TotHoursJanuaryUnder0, TotHoursFebruaryUnder0)
                            #, vector_TotRainAbrilBetween, vector_RainMay)
  
  
  
  colnames(df_Analysis) <-c("Years" , "Productivity" , "RainSeptember","RainOctober",
                            "RainNovember","RainDecember", "RainJanuary",
                            "RainFebruary","RainMarch","RainApril",
                            "RainMay","RainJune", "TempAvgSeptember", 
                            "TempAvgOctober", "TempAvgNovember","TempAvgDecember",
                            "TempAvgJanuary","TempAvgFebruary", "TempAvgMarch",
                            "TempAvgApril", "TempAvgMay", "TempAvgJune",
                            "TempMinSeptember",  "TempMinOctober", "TempMinNovember",
                            "TempMinDecember", "TempMinJanuary","TempMinFebruary",
                            "TempMinMarch", "TempMinApril", "TempMinMay", 
                            "TempMinJune", "TempMaxSeptember",  "TempMaxOctober",
                            "TempMaxNovember", "TempMaxDecember", "TempMaxJanuary",
                            "TempMaxFebruary", "TempMaxMarch", "TempMaxApril",
                            "TempMaxMAy", "TempMaxJune", "HumiditySeptember", 
                            "HumidityOctober", "HumidityNovember", "HumidityDecember",
                            "HumidityJanuary","HumidityFebruary", "HumidityMarch",
                            "HumidityApril","HumidityMay", "HumidityJune", 
                            "CloudsDecember","CloudsJanuary","CloudsFebruary", "CloudsMarch",
                            "CloudsApril", "CloudsMay", "CloudsJune",
                            "RainSeptember15toOctober15","Aprilwith100mmRain",
                            "MayWithRain", "JuneWithOutRain",
                            "DaysOfMarsAndApril15to20degreesTemp","DaysOfMayAndJune16to20degreesTemp",
                            "DaysofMayAndJuneOver30WindEast", "DaysofMayAndJuneOver30",
                            "HoursDecemberUnder0degrees", "HoursJanuaryUnder0degrees", 
                            "HoursFebruaryUnder0degrees")
                            #"", ,
                            #"MayWithRain (boolean")
  


  
View (df_Analysis)
 
write.csv(df_Analysis,OutfileNotNormalized, row.names = TRUE)




#------------------------------------------------------
# column normalization
#------------------------------------------------------

#df_Analysis <- read.csv("C:\\lab\\mestrado\\TESE\\DADOS\\EO\\LandSat\\DadosClassificacaoImagem9\\LandSatData3.csv", header = TRUE) 
#df_Analysis <- read.csv("C:\\lab\\mestrado\\TESE\\DADOS\\ClimateAndNDVIData\\BEJA_01_DataForModelNotNormalized15_2.csv", header = TRUE) 


#Column with productivity not normalized
Productivity <- df_Analysis$Productivity

#Column with years not normalized
Years <- df_Analysis$Year

#column with X not normalized (this is just an id)
X <- df_Analysis$X

myDataExploring = cbind(X,Years,Productivity)

#Total number of columns
Tot_Columns <- ncol(df_Analysis)

TempDF <- df_Analysis[,4:Tot_Columns]

#Data normalization methods are used to make variables, measured in different scales, have comparable values.
#TempDFNormalized <- normalize( TempDF[,5]) 
myOriginal_DataExploring <- df_Analysis

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}




for (i in 1:ncol(myOriginal_DataExploring)) {
    ThisColumnName <- colnames(myOriginal_DataExploring)[i]
  if (colnames(myOriginal_DataExploring)[i] == "X" || colnames(myOriginal_DataExploring)[i] == "Year"  || colnames(myOriginal_DataExploring)[i] == "Productivity") {
    #do nothing brother
  } else {
    thisColumn <- normalize(myOriginal_DataExploring[,i])
    myDataExploring = cbind(myDataExploring,thisColumn)
    colnames(myDataExploring)[ncol(myDataExploring)] <- paste0(ThisColumnName)
    
  }
}

 

write.csv(myDataExploring,OutfileNormalized, row.names = TRUE)
 

View (myDataExploring)

}


MySourceFile  <- "C:\\lab\\mestrado\\TESE\\DADOS\\OpenWeather\\886dc090817068c16e683191fb2160d3.csv"
MyOutfileNormalized <- "C:\\lab\\mestrado\\TESE\\DADOS\\Beja_Normalized.csv"
MyOutfileNotNormalized <- "C:\\lab\\mestrado\\TESE\\DADOS\\Beja_NotNormalized.csv"


# Variable01 - YEAR
Myyears  <- c(1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,
            1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
            2010,2011,2012,2013,2014,2015,2016,2017,2018)

#Variable02 - Wheat Productivity(kg/ha)
MyWheatProduction <- c(1688,1755,1430,1983,1481,2177,1350,1725,2062,1433,
                     1799,1214,1033,1600,1578,817,1796,812,1577,605,2480,
                     1919,2387,1772,1515,1233,1124,1830,2225,2202,2592,2250,2713)



 
DataProcessing(SourceFile = MySourceFile, OutfileNormalized = MyOutfileNormalized, OutfileNotNormalized = MyOutfileNotNormalized, years= Myyears, WheatProduction = MyWheatProduction)
