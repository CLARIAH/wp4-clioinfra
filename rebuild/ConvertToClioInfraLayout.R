options( java.parameters = "-Xmx10g")

library(rJava)
library(WriteXLS)
library(xlsx)
library(data.table)
library(tcltk)
library(countrycode)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source('hwlfunctions.R')

AddColsForMaddision <- F
ISO2plus <- read.xlsx('../UPDATE 20210315/ISO_Codes_2_3.xlsx',sheetIndex = 1, stringsAsFactors = F)
ISO3s <- read.xlsx('../UPDATE 20210315/ISO3REF.xls',sheetIndex = 1, stringsAsFactors = F)
ClioLayoutBase<- read.xlsx('../ClioLayout.xlsx', sheetIndex = 1, startRow = 1,
                       check.names = F, stringsAsFactors = F, 
                       colClasses = c('character',"numeric","numeric","character",rep("numeric",518)))
#MAD <- read.xlsx('maddison19902011regionalaverages22.xlsx',sheetIndex = 1, stringsAsFactors = F, startRow = 1,endRow = 2)
#MAD <- as.character(MAD[1,c(2:ncol(MAD))])

if (AddColsForMaddision){
  YearsToAdd <- c(1,730,1000,1150,1280:1499)
  w1 = ncol(ClioLayout)+1
  w2 = w1+length(YearsToAdd)-1
  ClioLayout[c(w1:w2)] <- NA
  names(ClioLayout)[c(w1:w2)] <- as.character(YearsToAdd)
  
  ClioLayout <- ClioLayout[,c("Webmapper code","Webmapper numeric code","ccode","country name","start year","end year","ISO3",as.character(c(YearsToAdd,1500:2050)))]
}

ClioLayoutBase$ISO3 <- as.character(NA)

for (i in c(1:nrow(ClioLayoutBase))){
  if (length(ISO3s$iso3[which(ISO3s$Country==ClioLayoutBase$`country name`[i])])>0){
    ClioLayoutBase$ISO3[i] <- 
      ISO3s$iso3[which(ISO3s$Country==ClioLayoutBase$`country name`[i])]
  } else {
    if (ClioLayoutBase$`end year`[i]==2012){
      print(ClioLayoutBase$`country name`[i])
    }
  }
}

# in Clio template DRC Congo is ZAR (set by my script above actually)
# here I am going to use the COD convension from JL's GDP:

ClioLayoutBase$ISO3[which(grepl("DRC",ClioLayoutBase$`country name`, fixed = T))] <- 'COD'

# add PSE:
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "State of Palestine"
temp$`start year` <- 1950
temp$`end year` <- 2012
temp$ISO3 <- 'PSE'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add COK Cook Islands
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "Cook Islands"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'COK'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add GUF French Guiana
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "French Guiana"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'GUF'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add MAC Macau, China
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "Macau, China"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'MAC'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add VIR Virgin Islands
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "Virgin Islands"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'VIR'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add "ANT" Netherlands Antilles
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "Netherlands Antilles"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'ANT'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add "CYM" Cayman Islands
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "Cayman Islands"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'CYM'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add "IMN" Isle of Man
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "Isle of Man"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'IMN'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)

# add "JEY" Jersey
temp <- ClioLayoutBase[1,]
temp$`Webmapper code` <- as.character(NA)
temp$`Webmapper numeric code` <- as.numeric(NA)
temp$ccode <- as.numeric(NA)
temp$`country name` <-  "Jersey"
temp$`start year` <- as.numeric(NA)
temp$`end year` <- 2012
temp$ISO3 <- 'JEY'
ClioLayoutBase <- rbind(ClioLayoutBase,temp)


ClioLayoutBase$ISO3[which(ClioLayoutBase$`country name`=='Germany Democratic Republic')] <- "DDR"

total <- which(ClioLayoutBase$`end year`==2012)
#sort(unique(df$iso3))[which(!sort(unique(df$iso3)) %in% unique(ClioLayout$ISO3[which(ClioLayout$`end year`==2012)]))]
#  "CSK" 1992 "HKG" 1945 "PRI" 1945 "SUN" 1991 "YUG" 1946 1992
ToAdd <- which(ClioLayoutBase$`end year`==1992 & ClioLayoutBase$ISO3=='CSK')
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='HKG'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='PRI'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='REU'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='BMU'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='GLP'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='GRL'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='MTQ'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='NCL'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='GIB'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='GUM'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`end year`==1945 & ClioLayoutBase$ISO3=='PYF' & ClioLayoutBase$`Webmapper numeric code`==867))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`start year`==1946 & ClioLayoutBase$`end year`==1991 & ClioLayoutBase$ISO3=='SUN'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`start year`==1946 & ClioLayoutBase$`end year`==1992 & ClioLayoutBase$ISO3=='YUG'))
ToAdd <- c(ToAdd,which(ClioLayoutBase$`start year`==1954 & ClioLayoutBase$`end year`==1990 & ClioLayoutBase$ISO3=='DDR'))
total <- c(total,ToAdd)
ToRemove <- which(ClioLayoutBase$`start year`==1956 & ClioLayoutBase$`end year`==2012 & ClioLayoutBase$ISO3=='MAR')
ToRemove <- c(ToRemove,which(ClioLayoutBase$`start year`==1946 & ClioLayoutBase$`end year`==2012 & ClioLayoutBase$ISO3=='CAN'))
ToRemove <- c(ToRemove,which(ClioLayoutBase$`start year`==2011 & ClioLayoutBase$`end year`==2012 & ClioLayoutBase$ISO3=='SDN'))
total <- total[!total %in% ToRemove]
ttt <- ClioLayoutBase$ISO3[total]
ttt[duplicated(ttt)]
rm(ttt,ToAdd,ToRemove)

#### 24 new variables overall ####
#### Poverty CBN OK #####
pov = fread("OECD_PovertyRates_Final.csv",
            dec = ",")

setnames(pov, "ISO3", "iso3")
setnames(pov, "Year", "year")
pov = pov[!is.na(PovRate)]

ClioLayout <- ClioLayoutBase

for (i in total){
  if (ClioLayout$ISO3[i] %in% pov$iso3){
    for (j in c(1820:2018)){
      ClioLayout[i,as.character(j)] <- pov$PovRate[which(pov$iso3==ClioLayout$ISO3[i] & pov$year==j)]
    }
  } else {
    #print(ClioLayout$ISO3[i])
  }
}

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1820:2018))])
# 26268

if (!sum(ttt[as.character(c(1820:2018))])==nrow(pov)){
  stop(paste0('Not all data were transfered: ','Poverty CBN'))
} else {
  print(paste0('OK! All data were transfered: ','Poverty CBN'))
}
ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Global_Extreme_Poverty_Cost_of_Basic_Needs.xlsx",row.names = F)

#### Poverty DAD OK #####

ClioLayout <- ClioLayoutBase

for (i in total){
  if (ClioLayout$ISO3[i] %in% pov$iso3){
    for (j in c(1820:2018)){
      ClioLayout[i,as.character(j)] <- pov$PCN1.9[which(pov$iso3==ClioLayout$ISO3[i] & pov$year==j)]
    }
  } else {
    #print(ClioLayout$ISO3[i])
  }
}

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1820:2018))])
# 26268

if (!sum(ttt[as.character(c(1820:2018))])==nrow(pov)){
  stop(paste0('Not all data were transfered: ','Poverty DAD'))
} else {
  print(paste0('OK! All data were transfered: ','Poverty DAD'))
}
ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Global_Extreme_Poverty_Dollar_a_Day.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','AddColsForMaddision','total'))])

#### GDP & Pop OK ####
##### gdppc_mix #####

gdp3 = readxl::read_excel("Data_Maddison update 2020 preliminary_2.xlsx",
                  sheet = "Country data", skip = 2)
setDT(gdp3)

gdp3 = melt(
  data = gdp3, 
  id.var = "year", 
  value.name = "gdppc_mix", 
  variable.name = "iso3",
  variable.factor = FALSE)

gdp3[, Country:=countrycode(iso3, "iso3c", "country.name")]
gdp3[iso3 == "ARE" & year %in% 1991:1992, gdppc_mix := NA]
gdp3[iso3 == "CSK", Country:='Czechoslovakia']
gdp3[iso3 == "SUN", Country:='USSR']
gdp3[iso3 == "YUG", Country:='Yugoslavia']

gdp_mix <- gdp3
rm(gdp3)

gdp_mix[, gdppc_mix := as.numeric(gdppc_mix)]
gdp_mix = gdp_mix[!is.na(year)]
gdp_mix = gdp_mix[!is.na(gdppc_mix)]

df <- gdp_mix
df <- subset(df,df$year>=1500)

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$gdppc_mix))])){
      if (length(temp$gdppc_mix[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$gdppc_mix[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# check if there are iso3 without entries:
df$iso3year <- paste0(df$iso3,df$year)
df <- df[!duplicated(df$iso3year),]

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 18195
ddd <- df$gdppc_mix
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 18195

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','GDP mix'))
} else {
  print(paste0('OK! All data were transfered: ','GDP mix'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"GDP_Per_Capita_Mix.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total'))])

#### Life Expectancy OK ####
##### Level Male #####
lif = readxl::read_excel("Chapter6Data.xls", "Sheet1")
setDT(lif)
setnames(lif, "iso_code", "iso3")

lif[iso3 == "GER", iso3 := "DEU"] # the horror
lif = lif[iso3 != "ANT"]

if (any(duplicated(lif, by = c("iso3", "year", "sex")))){
  warning("duplicates")
}

lif <- lif[,c('iso3','year','sex',"LifeExpectancy_Level", "LifeExpectancy_Gini", "LifeExpectancy_RelativeGini")]
df <- lif
rm(lif)
Sex <- unique(df$sex)

df$sex[which(df$sex==Sex[1])] <- 'Male'
df$sex[which(df$sex==Sex[2])] <- 'Female'

#df <- df[which((df$sex=='Male'))]

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)
ttt <- c()
for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$LifeExpectancy_Level))])){
      if (length(temp$LifeExpectancy_Level[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Male')])>0){
        ClioLayout[i,as.character(j)] <- temp$LifeExpectancy_Level[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Male')]
        ttt <- c(ttt,which(df$iso3==ClioLayout$ISO3[i] & df$year==j & df$sex=='Male'))
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

unique(df$iso3[!c(1:nrow(df)) %in% ttt])

sort(unique(df$iso3[which(!df$iso3 %in% ClioLayout$ISO3 & !is.na(df$LifeExpectancy_Level))]))
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 7581
ddd <- df$LifeExpectancy_Level[which(df$sex=='Male')]
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 7581

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','LE Male'))
} else {
  print(paste0('OK! All data were transfered: ','LE Male'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Life_Expectancy_Level_Male.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','df','total'))])

##### Level Female #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$LifeExpectancy_Level))])){
      if (length(temp$LifeExpectancy_Level[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Female')])>0){
        ClioLayout[i,as.character(j)] <- temp$LifeExpectancy_Level[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Female')]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 7562
ddd <- df$LifeExpectancy_Level[which(df$sex=='Female')]
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 7562

# add the double entries (meaning that they appear twice in the clio template with end year==2012) of CAN, Morocco, Sudan
if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','LE Female'))
} else {
  print(paste0('OK! All data were transfered: ','LE Female'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Life_Expectancy_Level_Female.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','df','total'))])

##### Gini Male #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$LifeExpectancy_Gini))])){
      if (length(temp$LifeExpectancy_Gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Male')])>0){
        ClioLayout[i,as.character(j)] <- temp$LifeExpectancy_Gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Male')]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 7581
ddd <- df$LifeExpectancy_Gini[which(df$sex=='Male')]
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 7581

# add the double entries (meaning that they appear twice in the clio template with end year==2012) of CAN, Morocco, Sudan
if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','LE Gini Male'))
} else {
  print(paste0('OK! All data were transfered: ','LE Gini Male'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Life_Expectancy_Gini_Male.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','df','total'))])

##### Gini Female #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$LifeExpectancy_Gini))])){
      if (length(temp$LifeExpectancy_Gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Female')])>0){
        ClioLayout[i,as.character(j)] <- temp$LifeExpectancy_Gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Female')]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 7562
ddd <- df$LifeExpectancy_Gini[which(df$sex=='Female')]
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 7562

# add the double entries (meaning that they appear twice in the clio template with end year==2012) of CAN, Morocco, Sudan
if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','LE Gini Female'))
} else {
  print(paste0('OK! All data were transfered: ','LE Gini Female'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Life_Expectancy_Gini_Female.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','df','total'))])

##### Relative Gini Male #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$LifeExpectancy_RelativeGini))])){
      if (length(temp$LifeExpectancy_RelativeGini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Male')])>0){
        ClioLayout[i,as.character(j)] <- temp$LifeExpectancy_RelativeGini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Male')]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 6108
ddd <- df$LifeExpectancy_RelativeGini[which(df$sex=='Male')]
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 6108

# add the double entries (meaning that they appear twice in the clio template with end year==2012) of CAN, Morocco, Sudan
if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','LE Relative Gini Male'))
} else {
  print(paste0('OK! All data were transfered: ','LE Relative Gini Male'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Life_Expectancy_Relative_Gini_Male.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','df','total'))])

##### Relative Gini Female #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$LifeExpectancy_RelativeGini))])){
      if (length(temp$LifeExpectancy_RelativeGini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Female')])>0){
        ClioLayout[i,as.character(j)] <- temp$LifeExpectancy_RelativeGini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j & temp$sex=='Female')]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 6086
ddd <- df$LifeExpectancy_RelativeGini[which(df$sex=='Female')]
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 6109

# add the double entries (meaning that they appear twice in the clio template with end year==2012) of CAN, Morocco, Sudan
if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','LE Relative Gini Female'))
} else {
  print(paste0('OK! All data were transfered: ','LE Relative Gini Female'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Life_Expectancy_Relative_Gini_Female.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total'))])

#### Education OK ####
##### Pop 15+ #####

edu = read.xlsx("rei1.xlsx", sheetName = "all", check.names = T, stringsAsFactors = F)
setDT(edu)

edu[, iso3 := countrycode(country, "country.name", "iso3c")]
edu[country == "Yugoslavia", iso3 := "YUG"]
edu[country == "USSR", iso3 := "SUN"]

edu <- edu[,c('year', 'iso3','pop15..MM','av.years.MM',
              'av.years.gini.MM','st.dev','rei')]

names(edu) <- c('year', 'iso3',
               'pop15plus',
               'aveduLLMM',
               'eduginiLLMM',
               'edusd',
               'relative_edu_gini')
df <- edu

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$pop15plus))])){
      if (length(temp$pop15plus[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$pop15plus[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 12018
ddd <- df$pop15plus
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 12018

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Edu: Pop 15+'))
} else {
  print(paste0('OK! All data were transfered: ','Edu: Pop 15+'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Education_Population_Above_15.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### aveduLLMM #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$aveduLLMM))])){
      if (length(temp$aveduLLMM[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$aveduLLMM[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 10905
ddd <- df$aveduLLMM
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 10905

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Edu: aveduLLMM'))
} else {
  print(paste0('OK! All data were transfered: ','Edu: aveduLLMM'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Education_Average_LLMM.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### eduginiLLMM #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$eduginiLLMM))])){
      if (length(temp$eduginiLLMM[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$eduginiLLMM[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 10879
ddd <- df$eduginiLLMM
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 10879

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Edu: eduginiLLMM'))
} else {
  print(paste0('OK! All data were transfered: ','Edu: eduginiLLMM'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Education_Gini_LLMM.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### edusd #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$edusd))])){
      if (length(temp$edusd[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$edusd[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 10879
ddd <- df$edusd
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 10879

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Edu: edusd'))
} else {
  print(paste0('OK! All data were transfered: ','Edu: edusd'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Education_SD.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### relative_edu_gini #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$relative_edu_gini))])){
      if (length(temp$relative_edu_gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$relative_edu_gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 10879
ddd <- df$relative_edu_gini
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 10879

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Edu: relative_edu_gini'))
} else {
  print(paste0('OK! All data were transfered: ','Edu: relative_edu_gini'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Education_Relative_Gini.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total'))])

#### Wealth OK ####
##### Decadal Ginis #####
wlt = readxl::read_xlsx("Wealth Inequality-Alfani_Schifano_FINAL.xlsx",
                        sheet = "Data")
setDT(wlt)
setnames(wlt, "Year", "year")

wlt <- wlt[year >= 1820,
    list(year, iso3,
         wealthgini = Gini,
         wealth_gini_source = SG,
         wealthtop10 = Top10per,
         wealth_top10_source = SourceTop10,
         wealth = Privatenetwealth2017)]

df <- wlt
rm(wlt)

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$wealthgini))])){
      if (length(temp$wealthgini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$wealthgini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 240
ddd <- df$wealthgini
ddd <- ddd[!is.na(ddd)]
length(ddd)

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==nrow(df)){
  stop(paste0('Not all data were transfered: ','Wealth Decadal Ginis'))
} else {
  print(paste0('OK! All data were transfered: ','Wealth Decadal Ginis'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Wealth_Decadal_Ginis.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### wealthtop10 #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$wealthtop10))])){
      if (length(temp$wealthtop10[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$wealthtop10[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 240
ddd <- df$wealthtop10
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 240

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==nrow(df)){
  stop(paste0('Not all data were transfered: ','Wealth wealthtop10'))
} else {
  print(paste0('OK! All data were transfered: ','Wealth wealthtop10'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Wealth_Top10.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### Privatenetwealth2017 #####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$wealth))])){
      if (length(temp$wealth[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$wealth[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 115
ddd <- df$wealth
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 115

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Wealth wealth'))
} else {
  print(paste0('OK! All data were transfered: ','Wealth wealth'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Wealth_Total_2017.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total'))])

##### Yearly Ginis ####
annual_wealth_ginis = readxl::read_xls("Figure 5.2 -upd.xls",
                                       sheet = "Figure 5.2", skip = 8)
setDT(annual_wealth_ginis)
setnames(annual_wealth_ginis, "Country code", "iso3")
setnames(annual_wealth_ginis, "Wealth Inequality", "Gini")
setnames(annual_wealth_ginis, "Year", "year")
annual_wealth_ginis <- annual_wealth_ginis[,c('year','iso3','Gini')]

df <- annual_wealth_ginis
rm(annual_wealth_ginis)
ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$Gini))])){
      if (length(temp$Gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$Gini[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 759
ddd <- df$Gini
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 759

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Wealth Ginis'))
} else {
  print(paste0('OK! All data were transfered: ','Wealth Ginis'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Wealth_Yearly_Ginis.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total'))])

#### Working Hours OK ####

wrk = read.xlsx("workweek.xls", sheetName = "Sheet1", colIndex = c(1:4), stringsAsFactors = F)
setDT(wrk)
setnames(wrk, "Country.name", "iso3")

#wrk[is.na(iso3), iso3 := countrycode(`country`, "Country.name", "iso3c")]
wrk[`country` == "Netherlands Antilles", iso3 := "ANT"]
wrk[`country` == "Germany, Democratic Rep.", iso3 := "DDR"]
wrk[`country` == "Macau", iso3 := "MAC"]
wrk[`country` == "Germany (West Berlin)", iso3 := NA]
wrk[`country` == "Saar", iso3 := NA]
wrk = wrk[!is.na(Working.hours.per.week.in.Manufacturing)]

for (i in which(wrk$iso3=="")){
  if (length(ISO3s$iso3[which(ISO3s$Country==wrk$country[i])])>0){
    wrk$iso3[i] <- 
      ISO3s$iso3[which(ISO3s$Country==wrk$country[i])]
  }
}

wrk = wrk[!is.na(iso3)]
df <- wrk
#rm(wrk)

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)
ttt <- c()
for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$Working.hours.per.week.in.Manufacturing))])){
      if (length(temp$Working.hours.per.week.in.Manufacturing[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$Working.hours.per.week.in.Manufacturing[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
        ttt <- c(ttt,which(df$iso3==ClioLayout$ISO3[i] & df$year==j))
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
sort(unique(df$iso3[!c(1:nrow(df)) %in% ttt]))
sort(unique(df$iso3[which(!df$iso3 %in% ClioLayout$ISO3 & !is.na(df$Working.hours.per.week.in.Manufacturing))]))

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 4311
ddd <- df$Working.hours.per.week.in.Manufacturing
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 4311

# add the double entries (meaning that they appear twice in the clio template with end year==2012) of CAN????
if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Working hours'))
} else {
  print(paste0('OK! All data were transfered: ','Working hours'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Working_hours.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total'))])

#### Gender OK ####
##### overall index (hgi) ####
gen = readRDS("genderimputations_1870_2010_averaged.rds.gz")
setDT(gen)

# to include:
# the overall index (hgi)
# the average years of education ratio (edu)
# the labour force participation ratio (lab)

# 728 = SSD
# 729 = SDN
# 736 = SDN before split
# the Sudan stuff needs to be checked here
# this is using 729 and 736 which makes zero sense
# also the gdp merge seems off, as if year was not included
# but it's probably the imputation madness

gen = gen[year >= 1900][order(year)]
gen[, iso3 := countrycode(countryid, "iso3n", "iso3c")]
gen[countryid == 200, iso3 := "CSK"]
gen[countryid == 729, iso3 := "SDN"]
gen[countryid == 810, iso3 := "SUN"]
gen[countryid == 890, iso3 := "YUG"]
gen = gen[countryid != 736]

df <- gen

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)
ttt <- c()
for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$hgi))])){
      if (length(temp$hgi[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$hgi[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
        ttt <- c(ttt,which(df$iso3==ClioLayout$ISO3[i] & df$year==j))
      } else {
        print(df[which(df$iso3==ClioLayout$ISO3[i] & df$year==j),])
      }
    }
    rm(temp)
  } else {
    #print(ClioLayout$ISO3[i])
  }
}
close(pb)

# df$iso3year[!c(1:nrow(df)) %in% ttt]
#[1] "REU1900" "REU1910" "REU1920" "REU1930" "REU1940" "REU1950" "REU1960" "REU1970" "REU1980" "REU1990" "REU2000" "REU2010"
# its La Reunion! and perhaps the same is true for other datasets above.

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 1685
ddd <- df$hgi
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 1685

# maybe missing ISO3?
#df$iso3[which(!df$iso3 %in% ClioLayout$ISO3)]
#sort(unique(ClioLayout$ISO3[which(!ClioLayout$ISO3 %in% df$iso3)]))
# No!

# maybe duplicates in df?
#df$iso3year <- paste0(df$iso3,df$year)
#df <- df[!duplicated(df$iso3year),]
# No!

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Gender hgi'))
} else {
  print(paste0('OK! All data were transfered: ','Gender hgi'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Gender_HGI.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### average years of education ratio (edu) ####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$edu))])){
      if (length(temp$edu[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$edu[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 1685
ddd <- df$edu
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 1685

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Gender edu'))
} else {
  print(paste0('OK! All data were transfered: ','Gender edu'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Gender_edu.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total','df'))])

##### labour force participation ratio (lab) ####

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$lab))])){
      if (length(temp$lab[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$lab[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)

# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 1685
ddd <- df$hgi
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 1685

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Gender edu'))
} else {
  print(paste0('OK! All data were transfered: ','Gender edu'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Gender_lab.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase','total'))])

#### Social Spending OK ####
soc = readxl::read_xlsx("Socspen 1815-1945, Table x2, Fig's x1a, x1b ••.xlsx",
                        sheet = "Correl's with GDPpc",
                        skip = 5)
setDT(soc)
soc <- soc[1:14]
setnames(soc, 1, "iso3")
soc = melt(soc, id.vars = "iso3", variable.factor = FALSE)
soc[, socspend := as.numeric(value)]
soc[, year := as.numeric(variable)]
soc <- soc[!is.na(year)]

df <- soc
df <- subset(df,!is.na(df$socspend))
rm(soc)

ClioLayout <- ClioLayoutBase

pb <- tkProgressBar(title = "progress bar", min = 0, max = length(total), width = 300)

for (i in total){
  setTkProgressBar(pb, which(total==i), label=paste( round(which(total==i)/length(total)*100, 1),"% done"))
  if (ClioLayout$ISO3[i] %in% df$iso3){
    temp <- subset(df,df$iso3==ClioLayout$ISO3[i])
    for (j in c(temp$year[which(!is.na(temp$socspend))])){
      if (length(temp$socspend[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)])>0){
        ClioLayout[i,as.character(j)] <- temp$socspend[which(temp$iso3==ClioLayout$ISO3[i] & temp$year==j)]
      }
    }
    rm(temp)
  } else {
    print(ClioLayout$ISO3[i])
  }
}
close(pb)
# how many entries are transferred:
ttt <- apply(ClioLayout, 2, function(x) length(which(!is.na(x))))
sum(ttt[as.character(c(1:2050))], na.rm = T)
# 290
ddd <- df$socspend
ddd <- ddd[!is.na(ddd)]
length(ddd)
# 290

if (!sum(ttt[as.character(c(1:2050))], na.rm = T)==length(ddd)){
  stop(paste0('Not all data were transfered: ','Social Transfers'))
} else {
  print(paste0('OK! All data were transfered: ','Social Transfers'))
}

ClioLayout$ISO3 <- NULL
write.xlsx2(ClioLayout,"Social_Transfers.xlsx",row.names = F)

rm(list= ls()[!(ls() %in% c('ISO2plus','ISO3s','ClioLayoutBase'))])