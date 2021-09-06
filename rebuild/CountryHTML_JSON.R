# test string substitution in html pages:
# had to rename:
# "Exchange_Rates_UK-historicalV2.xlsx" to "Exchange_Rates_UK-historical.xlsx"
# gold_production.docx to Gold_production.docx
# latend_Democracy.docx and txt to Latent_Democracy.docx and txt NOTICE THE "T" at the end of LATENT it was D initially
# Universities.docx and txt to universities.docx and txt
# Urban_Population.txt and docx to Urban_population.txt and docx

options( java.parameters = "-Xmx10g")

library(RefManageR)
library(readxl)
library(xlsx)
library(ggplot2)
library(jsonlite)
library(tidyr)
library(beepr)

rm(list=ls())

# to get the data from the ReadData.R script
load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/ClioData.RData')) # this only contains ClioData dataframe that is too big to export on xslx and gives the Java heap space error

# select only rows that have not all elements of the specified columns NA
ClioOnlyWithData <- ClioData[rowSums(is.na(ClioData[,8:524]))<516,]

ClioMetaData <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/metaD.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
OECDregions <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/OECDregions.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
GlobalMetadata <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/GlobalMetadata.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
UNmembers <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNmembers.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
UNregions <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNregions.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)

# rebuild the countries even if they exist already?
from_scratch <- T
# export the individual indicator data for each country separately to xlsx files?
ExportData <- T
# export all indicator data for each country to xlsx files?
ExportCountryData <- T
# export the JSON files for the plots?
ExportJSON <- T
# set main path for all scripts
GenericPath <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(GenericPath)
source('f_IndicatorMenu.R')

URL_basis <- "https://www.clio-infra.eu"

Citations <- read_excel("CitationsStatic.xls")
Citations <- subset(Citations, !Citations$Indicator == "Geocoder")

GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Region == "South and South-East Asia")] <- "S. &amp; S.E.Asia"
GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Region == "East. Europe and form. SU")] <- "E.Europe &amp; f.SU"
GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Region == "MENA")] <- "M.East &amp; N.Africa"
GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Region == "East Asia")] <- "E.Asia"
GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Region == "Sub-Saharan Africa")] <- "Sub-Sah. Africa"
GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Region == "Latin America and Carib.")] <- "L.America &amp; Carib."

GlobalMetadata$subregion[which(GlobalMetadata$subregion == "Australia and New Zealand")] <- "Australia and N.Zealand"
GlobalMetadata$subregion[which(GlobalMetadata$subregion == "Latin America and the Caribbean")] <- "L.America &amp; Carib."

# PARAMETERS:
# PARAMETERS:
# PARAMETERS:
# PARAMETERS:

MinDataAvailable <- 10 # minimum number of datapoints for selecting the visualized indicators
MinDataDuration <- 80 # minimum number of years for selecting the visualized indicators
YearOffset <- 10 # number of years to allow missing on each side of year periods, for selecting 
# the indicators in the various periods for the descriptive statistics.
MinNumOfDescrIndicators <- 10 # minimum number of indicators to produce a meaningful set of variables
# to rank
IndicPriorityList <- read_excel('IndicPriorityList.xlsx')
ExclFromPerfMatrix <- c(IndicPriorityList$IndicatorsList[which(IndicPriorityList$Benign==2)]) # indicators that due to their nature are excluded from the Rankings and Performance table

WellB <- read_excel('/home/michalis/PhD/Clio Infra/New Data/Composite Wellbeing Index/compind_clioformat.xlsx', skip = 1)
WellB$Code[which(WellB$`Continent, Region, Country`=="Taiwan")] <- 158

#load the foundation dates from CIA factbook
CIA <- read_excel('CIA-Factbook-Countries with notes for their independence status.xls')
CIA$COUNTRY[which(CIA$COUNTRY=="Congo, Republic of the")] <- "Congo"
CIA$COUNTRY[which(CIA$COUNTRY=="Congo, Democratic Republic of the")] <- "Congo, DRC"
CIA$COUNTRY[which(CIA$COUNTRY=="Czechia")] <- "Czech Republic"
CIA$COUNTRY[which(CIA$COUNTRY=="Gambia, The")] <- "The Gambia"
CIA$COUNTRY[which(CIA$COUNTRY=="Korea, North")] <- "North Korea"
CIA$COUNTRY[which(CIA$COUNTRY=="Korea, South")] <- "South Korea"
CIA$COUNTRY[which(CIA$COUNTRY=="Micronesia, Federated States of")] <- "Micronesia"
CIA$COUNTRY[which(CIA$COUNTRY=="Marshall Islands")] <- "Marshall Is."
CIA$COUNTRY[which(CIA$COUNTRY=="Timor-Leste")] <- "Timor Leste"
CIA$COUNTRY[which(CIA$COUNTRY=="Saint Kitts and Nevis")] <- "St. Kitts and Nevis"
CIA$COUNTRY[which(CIA$COUNTRY=="Saint Lucia")] <- "St. Lucia"
CIA$COUNTRY[which(CIA$COUNTRY=="Saint Vincent and the Grenadines")] <- "St. Vincent and the Grenadines"
CIA$COUNTRY[which(CIA$COUNTRY=="Cabo Verde")] <- "Cape Verde"
CIA$COUNTRY[which(CIA$COUNTRY=="Solomon Islands")] <- "Solomon Is."
CIA$COUNTRY[which(CIA$COUNTRY=="Bahamas, The")] <- "Bahamas"

fileName <- '/home/michalis/PhD/Clio Infra/UPDATE 20210315/CountryTemplateJSON.html'
fileName2 <- '/home/michalis/PhD/Clio Infra/UPDATE 20210315/CountryTemplateNoMoreVisualsJSON.html'

trimalls <- function (x) gsub("\\s", "", x)

IndicatorsList <- unique(ClioData$Indicator)

CountryList <- unique(GlobalMetadata$ClioInfraCountryName)
CountryList <- CountryList[!is.na(CountryList)]
CountryList <- CountryList[!CountryList==""]

GlobalMetadata$`Alpha-3 code`[which(GlobalMetadata$ISO3OECD=="CSK")] <- "CSK"

for (j in 1:nrow(GlobalMetadata)){
  if (!is.na(GlobalMetadata$ISO3OECD[j])){
    if (GlobalMetadata$ISO3OECD[j]==""){
      GlobalMetadata$ISO3OECD[j] <- NA
    }
  }
}

k <- seq(1,length(CountryList),1)

for (i in k[c(1:length(k))]){
  # check if the page has been created and skip if yes
  XxZzYyCountryShortXxZzYy <- CountryList[i]
  
  countryHTMLfile <- paste0(GenericPath,"/Country Pages Exports R/",trimalls(gsub("[[:punct:]]", "", XxZzYyCountryShortXxZzYy)),".html")
  country_page_already_build <- file.exists(countryHTMLfile)
  if (!from_scratch & country_page_already_build){
    #print(paste(CountryList[i],i," skipping as it exists already",sep=";"))
  } else {
    # ignore rows/countries/territories without data
    if (!is.na(GlobalMetadata$DataPoints[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])])){
      if (GlobalMetadata$DataPoints[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]>0){
        test <- readChar(fileName, file.info(fileName)$size)
        print(CountryList[i])
        
        XxZzYyCountryShortNoSpaceXxZzYy <- trimalls(gsub("[[:punct:]]", "", CountryList[i]))
        
        if (!is.na(GlobalMetadata$UN_membership_name[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])])){
          UN_Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(GlobalMetadata$UN_membership_name[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]))
          XxZzYyCountryFullXxZzYy <- paste0(CountryList[i]," (",UN_Name,")")
          rm(UN_Name)
        } else {
          XxZzYyCountryFullXxZzYy <- CountryList[i]
        }
        
        if (!is.na(GlobalMetadata$`Alpha-3 code`[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])])) {
          XxZzYyISO3XxZzYy <- GlobalMetadata$`Alpha-3 code`[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]
          XxZzYyISO3XxZzYy <- XxZzYyISO3XxZzYy[!(XxZzYyISO3XxZzYy=="")]
        }
        
        if (!is.na(GlobalMetadata$numeric[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])])) {
          XxZzYyISO3numericXxZzYy <- GlobalMetadata$numeric[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]
        }
        
        StartYears <- GlobalMetadata$WebmapperStartYears[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]
        EndYears <- GlobalMetadata$WebmapperEndYears[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]
        if (nchar(StartYears)>0){
          StartYears <- strsplit(StartYears,";")[[1]]
          EndYears <- strsplit(EndYears,";")[[1]]
          
          XxZzYyBorderPeriodsXxZzYy <- paste(StartYears, EndYears,sep = "-",collapse = ", ")
        }
        rm(StartYears,EndYears)
        
        temp <- sub('([0-9]{4}).*', '\\1', CIA$INDEPENDENCE[which(CIA$COUNTRY==CountryList[i])])
        #https://www.r-bloggers.com/catching-errors-in-r-and-trying-something-else/
        
        ttt <- try(XxZzYyFoundationDateXxZzYy <- ifelse(substr(temp,1,4)=="none","none",temp))
        if (class(file) == "try-error"){
          print(paste(CountryList[i],i,"name not found in CIA",sep=";"))
          XxZzYyFoundationDateXxZzYy <- "..."
        }
        if (length(temp)==0){
          print(paste(CountryList[i],i,"zero length",sep=";"))
          XxZzYyFoundationDateXxZzYy <- "..."
        }
        if (CountryList[i]=="Ethiopia"){
          XxZzYyFoundationDateXxZzYy <- "B.C.E."
        }
        if (CountryList[i]=="Myanmar"){
          XxZzYyFoundationDateXxZzYy <- "4 January 1948"
        }
        if (length(grep("none",XxZzYyFoundationDateXxZzYy))>0){
          XxZzYyFoundationDateXxZzYy <- "special"
        }
          
        rm(temp)
        
        XxZzYyRegionUNXxZzYy <- GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]
        XxZzYyRegionOECDXxZzYy <- GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]
        
        wb <- subset(WellB,WellB$Code==GlobalMetadata$numeric[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])])
        temp <- names(wb)[!is.na(wb)]
        if (length(temp)>2){
          temp <- temp[3:length(temp)]
          temp <- as.numeric(temp)
          
          XxZzYyEarliestRankingDateXxZzYy <- as.character(min(temp))
          WellB <- WellB[ order(-WellB[,grep(as.character(min(temp)), names(WellB))]), ]
          XxZzYyEarliestRankXxZzYy <- which(WellB$Code==GlobalMetadata$numeric[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])])
          XxZzYyEarliestRankNumCountriesXxZzYy <- sum(!is.na(WellB[,grep(as.character(min(temp)),names(WellB))]))
          
          XxZzYyLatestRankingDateXxZzYy <- as.character(max(temp))
          WellB <- WellB[ order(-WellB[,grep(as.character(max(temp)), names(WellB))]), ]
          XxZzYyLatestRankXxZzYy <- which(WellB$Code==GlobalMetadata$numeric[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])])
          XxZzYyLatestRankNumCountriesXxZzYy <- sum(!is.na(WellB[,grep(as.character(max(temp)),names(WellB))]))
          
        } else {
          XxZzYyEarliestRankingDateXxZzYy <- "N/A"
          XxZzYyEarliestRankXxZzYy <- "N/A"
          XxZzYyEarliestRankNumCountriesXxZzYy <- "N/A"
          XxZzYyLatestRankingDateXxZzYy <- "N/A"
          XxZzYyLatestRankXxZzYy <- "N/A"
          XxZzYyLatestRankNumCountriesXxZzYy <- "N/A"
        }
        # temp for the dataholder
        temp <- subset(ClioData,ClioData$`country name`==CountryList[i])
        #keep only the rows with at least one data point
        temp <- temp[rowSums(is.na(temp[,9:524]))<515,]
        # and remove data refering to non-2012 borders
        temp <- temp[temp$`end year`=="2012",]
        # from the problem caused by Sudan: if multiple borders end in 2012 remove the one with the oldest start year
        if (length(unique(temp$`start year`))>1){
          StartYearToExclude <- min(as.numeric(unique(temp$`start year`)))
          temp <- temp[!temp$`start year`==StartYearToExclude,]
        }
        
        # when there are more than two rows for a variable with the same 2012 end year, e.g. Morocco
        # then I need to test for those variables with two lines, select the one with the same webmapper code as the others
        if (nrow(temp)>0){
          # however I will control entry on this part of the script to identified countries only:
          if (F){
          #if (CountryList[i] %in% c("Morocco")){
            RowsToRemove <- c()
            #find the dominant webmapper code:
            WMC <- unique(temp$`Webmapper code`)
            WMC_count <- c(1:length(WMC))/c(1:length(WMC))-1
            for (L_j in 1:length(WMC)){
              WMC_count[L_j] <- nrow(subset(temp,temp$`Webmapper code`==WMC[L_j]))
            }
            DomWMC <- WMC[which.max(WMC_count)]
            if (length(unique(temp$Indicator))<nrow(temp)){
              for (L_i in 1:nrow(temp)){
                Ttemp <- subset(temp,temp$Indicator==temp$Indicator[L_i])
                if (nrow(Ttemp)>1){
                  if (temp$`Webmapper code`[L_i] != DomWMC){
                    RowsToRemove <- c(RowsToRemove,L_i)
                  }
                }
                rm(Ttemp)
              }
            }
            temp <- temp[-as.numeric(RowsToRemove),]
          }
          
          SelectedIndicators <- c()
          HalfRejectedIndicators <- c() #only reaching half the thresholds of the selected ones
          RejectedIndicators <- c()
          RejIndicObs <- c()
          HalfRejIndicObs <- c()
          
          for (n in IndicPriorityList$IndicatorsList){
            #if (length(SelectedIndicators)<11){
              if (n %in% unique(temp$Indicator)){
                temp2 <- subset(temp,temp$Indicator==n)
                #keep only those with highest end year in its borders
                MaxEndYear <- max(temp2$`end year`)
                temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
                #for (m in nrow(temp2)){
                 # if (exists('StartYear')){
                  #  StartYear <- min(c(StartYear,as.numeric(names(temp2[which(!is.na(temp2[m,]))]))),na.rm = T)
                  #} else {
                    StartYear <- min(as.numeric(names(temp2[,which(
                      !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
                  #}
                  #if (exists('EndYear')){
                  #  EndYear <- max(c(EndYear,as.numeric(names(temp2[which(!is.na(temp2[m,]))]))),na.rm = T)
                  #} else {
                    EndYear <- max(as.numeric(names(temp2[,which(
                      !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
                  #}
                #}
                NumObs <- sum(as.numeric(rowSums(!is.na(temp2[,9:524]))))
                if (EndYear-StartYear > MinDataDuration & NumObs > MinDataAvailable){
                  SelectedIndicators <- c(SelectedIndicators,n)
                } else {
                  if (EndYear-StartYear > MinDataDuration/2 & NumObs > MinDataAvailable/2){
                    HalfRejectedIndicators <- c(HalfRejectedIndicators,n)
                    HalfRejIndicObs <- c(HalfRejIndicObs,NumObs)
                  } else {
                    RejectedIndicators <- c(RejectedIndicators,n)
                    RejIndicObs <- c(RejIndicObs,NumObs)
                  }
                }
                rm(NumObs,EndYear,StartYear)
              }
            #}
          }
          
          HalfRejected <- data.frame(Indicator=HalfRejectedIndicators,
                                 Obs=HalfRejIndicObs,
                                 stringsAsFactors = F)
          if (nrow(HalfRejected)>0){
            HalfRejected <- HalfRejected[order(-HalfRejected[,2]),]
          }
          
          Rejected <- data.frame(Indicator=RejectedIndicators,
                                 Obs=RejIndicObs,
                                 stringsAsFactors = F)
          if (nrow(Rejected)>0){
            Rejected <- Rejected[order(-Rejected[,2]),]
          }
          
          # impose variety if necessary/possible:
          
          if (length(SelectedIndicators)>11){
            # for indicators not in top ten remove any duplicates from their category one by one
            # until left with 11 Selected Indicators
            
            FilteredSelectedIndicators <- c()
            
              for (kkk in 1:length(SelectedIndicators)){
                if (IndicPriorityList$Priority[which(IndicPriorityList$IndicatorsList==SelectedIndicators[kkk])]<16 & length(FilteredSelectedIndicators)<11){
                  FilteredSelectedIndicators <- c(FilteredSelectedIndicators,SelectedIndicators[kkk])
                } else {
                  if (IndicPriorityList$Priority[which(IndicPriorityList$IndicatorsList==SelectedIndicators[kkk])]>15 & length(FilteredSelectedIndicators)<11){
                    if (length(grep("per Capita",FilteredSelectedIndicators))<3 & length(FilteredSelectedIndicators)<11){
                      FilteredSelectedIndicators <- c(FilteredSelectedIndicators,SelectedIndicators[kkk])
                    } else {
                      FiltSelCategories <- c()
                      for (jjj in 1:length(FilteredSelectedIndicators)){
                        FiltSelCategories <- c(FiltSelCategories,ClioMetaData$WebCategory[which(ClioMetaData$WebName==FilteredSelectedIndicators[jjj])])
                      }
                      if (length(grep(ClioMetaData$WebCategory[which(ClioMetaData$WebName==SelectedIndicators[kkk])],FiltSelCategories))<3 & 
                          length(FilteredSelectedIndicators)<11){
                        FilteredSelectedIndicators <- c(FilteredSelectedIndicators,SelectedIndicators[kkk])
                      }
                      rm(FiltSelCategories)
                    }
                  }
                }
              }
              #HERE XXX check again the correct selection of the max end year available per country/entity
          } else {
            FilteredSelectedIndicators <- SelectedIndicators
            if (length(FilteredSelectedIndicators)<11 & nrow(HalfRejected)>0){
              NumOfHalfRejIndic <- 1
              while (length(FilteredSelectedIndicators)<11 & nrow(HalfRejected)>0 & nrow(HalfRejected) >= NumOfHalfRejIndic){
                FilteredSelectedIndicators <- c(FilteredSelectedIndicators,HalfRejected$Indicator[NumOfHalfRejIndic])
                NumOfHalfRejIndic <- NumOfHalfRejIndic - 1
              }
            }
            
            if (length(FilteredSelectedIndicators)<11 & nrow(Rejected)>0){
              NumOfRejIndic <- 1
              while (length(FilteredSelectedIndicators)<11 & nrow(Rejected)>0 & nrow(Rejected)>=NumOfRejIndic){
                FilteredSelectedIndicators <- c(FilteredSelectedIndicators,Rejected$Indicator[NumOfRejIndic])
                NumOfRejIndic <- NumOfRejIndic + 1
              }
            }
            FilteredSelectedIndicators <- unique(FilteredSelectedIndicators)
          }
          
          # now in case there are less than 5 indicators selected
          # go through the complete set of indicators for this country
          # and get the max number of them preferably those with most datapoints [this preference is not implemented]
          if (length(FilteredSelectedIndicators) <= 5){
            IndicatorsAvail <- c(temp$Indicator)[!(c(temp$Indicator) %in% FilteredSelectedIndicators)]
            while(length(IndicatorsAvail) > 0 & length(FilteredSelectedIndicators) <= 5){
              #stop("investigate less than 5 indicators, with options available.")
              FilteredSelectedIndicators <- c(FilteredSelectedIndicators, IndicatorsAvail[1])
              IndicatorsAvail <- c(temp$Indicator)[!(c(temp$Indicator) %in% FilteredSelectedIndicators)]
            }
          }
          if (length(FilteredSelectedIndicators) > 0){
            XxZzYyIndic0XxZzYy <- FilteredSelectedIndicators[1]
            XxZzYyIndic0NoSpaceNoParXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[1])),sep="_")
            
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[1])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyIndic0StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyIndic0EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyIndic0StartXxZzYy,XxZzYyIndic0EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyIndic0NoSpaceNoParXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){    
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyIndic0NoSpaceNoParXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            test <- gsub("XxZzYyIndic0NoSpaceNoParXxZzYy", XxZzYyIndic0NoSpaceNoParXxZzYy, test)
            test <- gsub("XxZzYyIndic0XxZzYy", XxZzYyIndic0XxZzYy, test)
            test <- gsub("XxZzYyIndic0StartXxZzYy", XxZzYyIndic0StartXxZzYy, test)
            test <- gsub("XxZzYyIndic0EndXxZzYy", XxZzYyIndic0EndXxZzYy, test)
            
            
          }
          if (length(FilteredSelectedIndicators) > 1){
          XxZzYyIndic1XxZzYy <- FilteredSelectedIndicators[2]
          XxZzYyIndic1NoSpaceNoParXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[2])),sep="_")
          
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[2])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyIndic1StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyIndic1EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyIndic1StartXxZzYy,XxZzYyIndic1EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyIndic1NoSpaceNoParXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyIndic1NoSpaceNoParXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            test <- gsub("XxZzYyIndic1NoSpaceNoParXxZzYy", XxZzYyIndic1NoSpaceNoParXxZzYy, test)
            test <- gsub("XxZzYyIndic1XxZzYy", XxZzYyIndic1XxZzYy, test)
            test <- gsub("XxZzYyIndic1StartXxZzYy", XxZzYyIndic1StartXxZzYy, test)
            test <- gsub("XxZzYyIndic1EndXxZzYy", XxZzYyIndic1EndXxZzYy, test)
            
          }
          if (length(FilteredSelectedIndicators) > 2){
            XxZzYyIndic2XxZzYy <- FilteredSelectedIndicators[3]
            XxZzYyIndic2NoSpaceNoParXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[3])),sep="_")
            
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[3])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyIndic2StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyIndic2EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyIndic2StartXxZzYy,XxZzYyIndic2EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyIndic2NoSpaceNoParXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyIndic2NoSpaceNoParXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            test <- gsub("XxZzYyIndic2NoSpaceNoParXxZzYy", XxZzYyIndic2NoSpaceNoParXxZzYy, test)
            test <- gsub("XxZzYyIndic2XxZzYy", XxZzYyIndic2XxZzYy, test)
            test <- gsub("XxZzYyIndic2StartXxZzYy", XxZzYyIndic2StartXxZzYy, test)
            test <- gsub("XxZzYyIndic2EndXxZzYy", XxZzYyIndic2EndXxZzYy, test)
            
          }
          if (length(FilteredSelectedIndicators) > 3){
            XxZzYyIndic3XxZzYy <- FilteredSelectedIndicators[4]
            XxZzYyIndic3NoSpaceNoParXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[4])),sep="_")
            
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[4])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyIndic3StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyIndic3EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyIndic3StartXxZzYy,XxZzYyIndic3EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyIndic3NoSpaceNoParXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyIndic3NoSpaceNoParXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            test <- gsub("XxZzYyIndic3XxZzYy", XxZzYyIndic3XxZzYy, test)
            test <- gsub("XxZzYyIndic3NoSpaceNoParXxZzYy", XxZzYyIndic3NoSpaceNoParXxZzYy, test)
            test <- gsub("XxZzYyIndic3StartXxZzYy", XxZzYyIndic3StartXxZzYy, test)
            test <- gsub("XxZzYyIndic3EndXxZzYy", XxZzYyIndic3EndXxZzYy, test)
            
            
          }
          if (length(FilteredSelectedIndicators) > 4){
            XxZzYyIndic4XxZzYy <- FilteredSelectedIndicators[5]
            XxZzYyIndic4NoSpaceNoParXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[5])),sep="_")
            
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[5])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyIndic4StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyIndic4EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyIndic4StartXxZzYy,XxZzYyIndic4EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyIndic4NoSpaceNoParXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyIndic4NoSpaceNoParXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            test <- gsub("XxZzYyIndic4XxZzYy", XxZzYyIndic4XxZzYy, test)
            test <- gsub("XxZzYyIndic4NoSpaceNoParXxZzYy", XxZzYyIndic4NoSpaceNoParXxZzYy, test)
            test <- gsub("XxZzYyIndic4StartXxZzYy", XxZzYyIndic4StartXxZzYy, test)
            test <- gsub("XxZzYyIndic4EndXxZzYy", XxZzYyIndic4EndXxZzYy, test)
            
          }
          if (length(FilteredSelectedIndicators) > 5){
            XxZzYyIndic5XxZzYy <- FilteredSelectedIndicators[6]
            XxZzYyIndic5NoSpaceNoParXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[6])),sep="_")
            
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[6])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyIndic5StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyIndic5EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyIndic5StartXxZzYy,XxZzYyIndic5EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyIndic5NoSpaceNoParXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
          
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyIndic5NoSpaceNoParXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            test <- gsub("XxZzYyIndic5XxZzYy", XxZzYyIndic5XxZzYy, test)
            test <- gsub("XxZzYyIndic5NoSpaceNoParXxZzYy", XxZzYyIndic5NoSpaceNoParXxZzYy, test)
            test <- gsub("XxZzYyIndic5StartXxZzYy", XxZzYyIndic5StartXxZzYy, test)
            test <- gsub("XxZzYyIndic5EndXxZzYy", XxZzYyIndic5EndXxZzYy, test)
          }
          
          if (length(FilteredSelectedIndicators)>=11){
            XxZzYyMoreVisIndic1NoSpaceXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[7])),sep="_")
            XxZzYyMoreVisIndic1XxZzYy <- FilteredSelectedIndicators[7]
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[7])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyMoreVisIndic1StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyMoreVisIndic1EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyMoreVisIndic1StartXxZzYy,XxZzYyMoreVisIndic1EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyMoreVisIndic1NoSpaceXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyMoreVisIndic1NoSpaceXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            XxZzYyMoreVisIndic2NoSpaceXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[8])),sep="_")
            XxZzYyMoreVisIndic2XxZzYy <- FilteredSelectedIndicators[8]
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[8])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyMoreVisIndic2StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyMoreVisIndic2EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyMoreVisIndic2StartXxZzYy,XxZzYyMoreVisIndic2EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyMoreVisIndic2NoSpaceXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyMoreVisIndic2NoSpaceXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            
            XxZzYyMoreVisIndic3NoSpaceXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[9])),sep="_")
            XxZzYyMoreVisIndic3XxZzYy <- FilteredSelectedIndicators[9]
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[9])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyMoreVisIndic3StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyMoreVisIndic3EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyMoreVisIndic3StartXxZzYy,XxZzYyMoreVisIndic3EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyMoreVisIndic3NoSpaceXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyMoreVisIndic3NoSpaceXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            
            XxZzYyMoreVisIndic4NoSpaceXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[10])),sep="_")
            XxZzYyMoreVisIndic4XxZzYy <- FilteredSelectedIndicators[10]
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[10])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyMoreVisIndic4StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyMoreVisIndic4EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyMoreVisIndic4StartXxZzYy,XxZzYyMoreVisIndic4EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyMoreVisIndic4NoSpaceXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyMoreVisIndic4NoSpaceXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            
            XxZzYyMoreVisIndic5NoSpaceXxZzYy <- paste(XxZzYyISO3XxZzYy,trimalls(gsub("[[:punct:]]", "", FilteredSelectedIndicators[11])),sep="_")
            XxZzYyMoreVisIndic5XxZzYy <- FilteredSelectedIndicators[11]
            temp2 <- subset(temp,temp$Indicator==FilteredSelectedIndicators[11])
            #keep only those with highest end year in its borders
            MaxEndYear <- max(temp2$`end year`)
            temp2 <- subset(temp2, temp2$`end year` == MaxEndYear)
            XxZzYyMoreVisIndic5StartXxZzYy <- min(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            XxZzYyMoreVisIndic5EndXxZzYy <- max(as.numeric(names(temp2[,which(
              !is.na(temp2))])[9:length(temp2[,which(!is.na(temp2))])]),na.rm = T)
            temp2[which(!(names(temp2) %in% as.character(seq(XxZzYyMoreVisIndic5StartXxZzYy,XxZzYyMoreVisIndic5EndXxZzYy,1))))] <- NULL
            
            pdata <- as.data.frame(names(temp2),stringsAsFactors = F)
            names(pdata)[1]<- "year"
            pdata$year <- as.numeric(pdata$year)
            pdata$value <- as.numeric(temp2[1,])
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315/html/graphs")
            
            svg(filename=paste(XxZzYyMoreVisIndic5NoSpaceXxZzYy,".svg", sep = ""),
                width=12, 
                height=4, 
                pointsize=10)
            
            print(ggplot(pdata, aes(year, value)) + geom_line(color="#ffffff", size=3) + geom_point(shape=21, fill="#0088cc", color="#00aacc", size=2) + 
              xlab("") + ylab("") + theme_gray() +
              theme(axis.text = element_text(colour = "darkgray", size=13),
                    axis.title.x = element_text(colour = "darkgray"),
                    axis.title.y = element_text(colour = "darkgray", face = "bold", size=14)))
            
            dev.off()
            if (ExportJSON){
              jsondata <- toJSON(pdata)
              write(jsondata,paste0("/home/michalis/PhD/Clio Infra/UPDATE 20210315/JSON/",XxZzYyMoreVisIndic5NoSpaceXxZzYy,".json"))
              rm(jsondata)
            }
            rm(temp2)
            
            setwd("/home/michalis/PhD/Clio Infra/UPDATE 20210315")
            
            ### and substitute here the More section
            
            test <- gsub("XxZzYyMoreVisIndic1NoSpaceXxZzYy", XxZzYyMoreVisIndic1NoSpaceXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic1XxZzYy", XxZzYyMoreVisIndic1XxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic1StartXxZzYy", XxZzYyMoreVisIndic1StartXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic1EndXxZzYy", XxZzYyMoreVisIndic1EndXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic2NoSpaceXxZzYy", XxZzYyMoreVisIndic2NoSpaceXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic2XxZzYy", XxZzYyMoreVisIndic2XxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic2StartXxZzYy", XxZzYyMoreVisIndic2StartXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic2EndXxZzYy", XxZzYyMoreVisIndic2EndXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic3NoSpaceXxZzYy", XxZzYyMoreVisIndic3NoSpaceXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic3XxZzYy", XxZzYyMoreVisIndic3XxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic3StartXxZzYy", XxZzYyMoreVisIndic3StartXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic3EndXxZzYy", XxZzYyMoreVisIndic3EndXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic4NoSpaceXxZzYy", XxZzYyMoreVisIndic4NoSpaceXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic4XxZzYy", XxZzYyMoreVisIndic4XxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic4StartXxZzYy", XxZzYyMoreVisIndic4StartXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic4EndXxZzYy", XxZzYyMoreVisIndic4EndXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic5NoSpaceXxZzYy", XxZzYyMoreVisIndic5NoSpaceXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic5XxZzYy", XxZzYyMoreVisIndic5XxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic5StartXxZzYy", XxZzYyMoreVisIndic5StartXxZzYy, test)
            test <- gsub("XxZzYyMoreVisIndic5EndXxZzYy", XxZzYyMoreVisIndic5EndXxZzYy, test)
            
          } else {
            # the CountryHTML template without the More visuals section is loaded
            test <- readChar(fileName2, file.info(fileName2)$size)
            # the graph template with the appropriate number of graphs is loaded and substituted in the CountryHTML template
            FileIndex <- ifelse(length(FilteredSelectedIndicators)<6, length(FilteredSelectedIndicators)-1,5)
            XxZzYyGraphMenuItemsXxZzYy <- c()
            for (L_menu in 1:FileIndex){
              XxZzYyGraphMenuItemsXxZzYy <- c(XxZzYyGraphMenuItemsXxZzYy,paste0('<li><a href="#XxZzYyIndic',as.character(L_menu),
                                            'NoSpaceNoParXxZzYy" data-toggle="tab">XxZzYyIndic',as.character(L_menu),'XxZzYy</a></li>'))
            }
            XxZzYyGraphMenuItemsXxZzYy <- paste0(XxZzYyGraphMenuItemsXxZzYy,collapse = "")
            test <- gsub("XxZzYyGraphMenuItemsXxZzYy", XxZzYyGraphMenuItemsXxZzYy, test)
            
            XxZzYyGraphTemplateXxZzYy <- paste(GenericPath,"/GraphTemplate",FileIndex,".html", sep = "") 
            XxZzYyGraphTemplateXxZzYy <- readChar(XxZzYyGraphTemplateXxZzYy, file.info(XxZzYyGraphTemplateXxZzYy)$size)
            test <- gsub("XxZzYyGraphTemplateXxZzYy", XxZzYyGraphTemplateXxZzYy, test)
            
            # one graph will always be there even with one point
            
            test <- gsub("XxZzYyIndic0NoSpaceNoParXxZzYy", XxZzYyIndic0NoSpaceNoParXxZzYy, test)
            test <- gsub("XxZzYyIndic0XxZzYy", XxZzYyIndic0XxZzYy, test)
            test <- gsub("XxZzYyIndic0StartXxZzYy", XxZzYyIndic0StartXxZzYy, test)
            test <- gsub("XxZzYyIndic0EndXxZzYy", XxZzYyIndic0EndXxZzYy, test)
            
            # continue with substitution when needed
            if (length(FilteredSelectedIndicators) > 1 ){
              test <- gsub("XxZzYyIndic1NoSpaceNoParXxZzYy", XxZzYyIndic1NoSpaceNoParXxZzYy, test)
              test <- gsub("XxZzYyIndic1XxZzYy", XxZzYyIndic1XxZzYy, test)
              test <- gsub("XxZzYyIndic1StartXxZzYy", XxZzYyIndic1StartXxZzYy, test)
              test <- gsub("XxZzYyIndic1EndXxZzYy", XxZzYyIndic1EndXxZzYy, test)
            }
            
            if (length(FilteredSelectedIndicators) > 2 ){
              test <- gsub("XxZzYyIndic2NoSpaceNoParXxZzYy", XxZzYyIndic2NoSpaceNoParXxZzYy, test)
              test <- gsub("XxZzYyIndic2XxZzYy", XxZzYyIndic2XxZzYy, test)
              test <- gsub("XxZzYyIndic2StartXxZzYy", XxZzYyIndic2StartXxZzYy, test)
              test <- gsub("XxZzYyIndic2EndXxZzYy", XxZzYyIndic2EndXxZzYy, test)
            }
            
            if (length(FilteredSelectedIndicators) > 3 ){
              test <- gsub("XxZzYyIndic3XxZzYy", XxZzYyIndic3XxZzYy, test)
              test <- gsub("XxZzYyIndic3NoSpaceNoParXxZzYy", XxZzYyIndic3NoSpaceNoParXxZzYy, test)
              test <- gsub("XxZzYyIndic3StartXxZzYy", XxZzYyIndic3StartXxZzYy, test)
              test <- gsub("XxZzYyIndic3EndXxZzYy", XxZzYyIndic3EndXxZzYy, test)
            }
            
            if (length(FilteredSelectedIndicators) > 4 ){
              test <- gsub("XxZzYyIndic4XxZzYy", XxZzYyIndic4XxZzYy, test)
              test <- gsub("XxZzYyIndic4NoSpaceNoParXxZzYy", XxZzYyIndic4NoSpaceNoParXxZzYy, test)
              test <- gsub("XxZzYyIndic4StartXxZzYy", XxZzYyIndic4StartXxZzYy, test)
              test <- gsub("XxZzYyIndic4EndXxZzYy", XxZzYyIndic4EndXxZzYy, test)
            }
            
            if (length(FilteredSelectedIndicators) > 5 ){
              test <- gsub("XxZzYyIndic5XxZzYy", XxZzYyIndic5XxZzYy, test)
              test <- gsub("XxZzYyIndic5NoSpaceNoParXxZzYy", XxZzYyIndic5NoSpaceNoParXxZzYy, test)
              test <- gsub("XxZzYyIndic5StartXxZzYy", XxZzYyIndic5StartXxZzYy, test)
              test <- gsub("XxZzYyIndic5EndXxZzYy", XxZzYyIndic5EndXxZzYy, test)
            }
          }
          
          ########################################################################################################################
          ########################################################################################################################
          ########################################################################################################################
          ########################################################################################################################
          ########## FILLING IN THE Availability, Performance & Rankings TABLE
          ########################################################################################################################
          ########################################################################################################################
          ########################################################################################################################
          ########################################################################################################################
          
          #temp <- temp[rowSums(is.na(temp[,9:524]))<515,]
          Period1Start <- "1820"
          Period1End <- "1869"
          XxZzYyPeriod1CountXxZzYy <- nrow(temp[rowSums(is.na(temp[,match(Period1Start,names(temp)):match(Period1End,names(temp))]))<as.numeric(Period1End)-as.numeric(Period1Start),])
          Period2Start <- "1870"
          Period2End <- "1919"
          XxZzYyPeriod2CountXxZzYy <- nrow(temp[rowSums(is.na(temp[,match(Period2Start,names(temp)):match(Period2End,names(temp))]))<as.numeric(Period2End)-as.numeric(Period2Start),])
          Period3Start <- "1920"
          Period3End <- "1959"
          XxZzYyPeriod3CountXxZzYy <- nrow(temp[rowSums(is.na(temp[,match(Period3Start,names(temp)):match(Period3End,names(temp))]))<as.numeric(Period3End)-as.numeric(Period3Start),])
          Period4Start <- "1960"
          Period4End <- "2010"
          XxZzYyPeriod4CountXxZzYy <- nrow(temp[rowSums(is.na(temp[,match(Period4Start,names(temp)):match(Period4End,names(temp))]))<as.numeric(Period4End)-as.numeric(Period4Start),])
          Period5Start <- "1500"
          Period5End <- "2015"
          XxZzYyPeriod5CountXxZzYy <- nrow(temp[rowSums(is.na(temp[,match(Period5Start,names(temp)):match(Period5End,names(temp))]))<as.numeric(Period5End)-as.numeric(Period5Start),])
          
          #need the subset of indicators with year span more than half the period
          #WRONG: temp2 <- temp[rowSums(!is.na(temp[,match(Period1Start,names(temp)):match(Period1End,names(temp))]))<2,]
          temp2 <- temp
          
          # to avoid the many +infinities that this datasets contain 
          # I start by substituting with NA all the Production variables with value=0
          Production <- ClioMetaData$WebName[which(ClioMetaData$WebCategory=='Production')]
          tagDATA <- function(x) ifelse(x==0,NA,x)
          temp2[,match(Period1Start, names(temp2)):match(Period5End, names(temp2))] <- as.data.frame(
            lapply(temp2[,match(Period1Start, names(temp2)):match(Period5End, names(temp2))], FUN = function(x) {sapply(x,FUN=tagDATA)}))
          
          temp3 <- temp2
          ColsToKeep <- c("Indicator",seq(as.numeric(Period1Start),as.numeric(Period1End),1))
          temp3[,which(!(names(temp3) %in% ColsToKeep))] <- NULL
          
          temp3 <- temp3[rowSums(is.na(temp3[,2:ncol(temp3)]))<ncol(temp3)-2,]
          SelPeriodIndic <- subset(temp3,temp3$Indicator=="sadjkhksdf8228282827y^^@&^@")
          
          for (jjj in 1:nrow(temp3)){
            StartYear <- min(as.numeric(names(temp3[jjj,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[jjj,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            EndYear <- max(as.numeric(names(temp3[,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            if (EndYear >= as.numeric(Period1End)-YearOffset & StartYear <= as.numeric(Period1Start)+YearOffset){
              SelPeriodIndic <- rbind(SelPeriodIndic,temp3[jjj,])
            }
          }
          
          #remove all the non-period years, and non-indicator name columns
          #SelPeriodIndic[,which(!(names(SelPeriodIndic) %in% ColsToKeep))] <- NULL
          AvailIndicForPeriod <- nrow(SelPeriodIndic)
          if (nrow(SelPeriodIndic) > 0) {
            SelPeriodIndic$Growth <- as.numeric(NA)
            IndicatorsInSelPeriodIndic <- unique(SelPeriodIndic$Indicator)
            IndicatorsInSelPeriodIndic <- IndicatorsInSelPeriodIndic[!IndicatorsInSelPeriodIndic %in% ExclFromPerfMatrix]
            SelPeriodIndic <- subset(SelPeriodIndic,!(SelPeriodIndic$Indicator %in% ExclFromPerfMatrix))
            
            #for the rankings:
            RankingIndic <- ClioOnlyWithData
            RankingIndic <- RankingIndic[RankingIndic$`end year`=="2012",]
            
            ColsToKeep <- c(ColsToKeep,"country name")
            RankingIndic[,which(!(names(RankingIndic) %in% ColsToKeep))] <- NULL
            RankingIndic <- RankingIndic[which(RankingIndic$Indicator %in% IndicatorsInSelPeriodIndic),]
            RankingIndic$Rank <- as.numeric((NA))
            RankingIndic <- RankingIndic[rowSums(is.na(RankingIndic[,2:ncol(RankingIndic)]))<ncol(RankingIndic)-2,]
            
            # now I need to rank all the variables according to their period's average
            RankingIndic$Avg <- rowMeans(RankingIndic[3:(ncol(RankingIndic)-2)],na.rm = T)
            for (L_kkk in IndicatorsInSelPeriodIndic){
              RankingIndic$Rank[which(RankingIndic$Indicator==L_kkk)] <- match(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)], 
                                                                               sort(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)],
                                                                                    decreasing = ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList %in% L_kkk)]==1,T,F)))
            }
          }
          
          if ( nrow(SelPeriodIndic) >= MinNumOfDescrIndicators) {
            # first get the earliest and last year for each variable and estimate its growth
            for (L_jij in 1:nrow(SelPeriodIndic)){
              L_sds <- SelPeriodIndic[L_jij,2:(ncol(SelPeriodIndic)-1)]
              L_sds[,which(is.na(L_sds[1,]))] <- NULL
              SelPeriodIndic$Growth[L_jij] <- ifelse(L_sds[1,ncol(L_sds)]==L_sds[1,1],0,100*(L_sds[1,ncol(L_sds)]-L_sds[1,1])/L_sds[1,1])[[1]][1]
              SelPeriodIndic$Growth[L_jij] <- ifelse(IndicPriorityList$Benign[which(
                IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[L_jij])]==0,-SelPeriodIndic$Growth[L_jij],SelPeriodIndic$Growth[L_jij])
            }
            # there are some variables with infinity (coming from the polity circle), I would group those
            
            XxZzYyPeriod1BestFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Infinite",abs(round(max(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod1BestCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Progressed",ifelse(round(max(SelPeriodIndic$Growth))>0,"Progressed",ifelse(XxZzYyPeriod1BestFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod1BestXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,
                                              paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==Inf)])],
                                                    collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",
                                                                     XxZzYyPeriod1BestCharacterizationXxZzYy," ",XxZzYyPeriod1BestFigureXxZzYy,"%\">",sep="")), 
                                              IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.max(SelPeriodIndic$Growth)])]) # the name of the indicator
            
            XxZzYyPeriod1WorstFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Minus Infinity",abs(round(min(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod1WorstCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Collapsed",ifelse(round(min(SelPeriodIndic$Growth),2)>0,"Progressed",ifelse(XxZzYyPeriod1WorstFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod1WorstXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,
                                               paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==-Inf)])],
                                                     collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod1WorstCharacterizationXxZzYy," ",XxZzYyPeriod1WorstFigureXxZzYy,"%\">")), 
                                               IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.min(SelPeriodIndic$Growth)])]) # the name of the indicator
            
          } else {
            XxZzYyPeriod1BestCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                  " available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
            XxZzYyPeriod1BestFigureXxZzYy <- "N/A" # just get the number here (in its absolute)
            XxZzYyPeriod1BestXxZzYy <- "N/A" # the name of the indicator
            
            XxZzYyPeriod1WorstCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                  " available indicators with enough observations in this period",sep = "") 
            XxZzYyPeriod1WorstFigureXxZzYy <- "N/A"
            XxZzYyPeriod1WorstXxZzYy <- "N/A"
            if (AvailIndicForPeriod == 0){
              XxZzYyPeriod1BestCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
              XxZzYyPeriod1WorstCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") 
            }
          }
          
          if ( nrow(SelPeriodIndic) >= 2) {
            XxZzYyPeriod1BestRankFigureXxZzYy <- min(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod1BestRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod1BestRankFigureXxZzYy)])], collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod1TotalBestRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod1BestRankFigureXxZzYy)][1])],na.rm = T)
            
            XxZzYyPeriod1WorstRankFigureXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod1WorstRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod1WorstRankFigureXxZzYy)])], collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod1TotalWorstRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod1WorstRankFigureXxZzYy)][1])],na.rm = T)
          } else {
            XxZzYyPeriod1BestRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod1TotalBestRankedXxZzYy <- "N/A"
            XxZzYyPeriod1BestRankXxZzYy <- "N/A"
            
            XxZzYyPeriod1WorstRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod1TotalWorstRankedXxZzYy <- "N/A"
            XxZzYyPeriod1WorstRankXxZzYy <- "N/A"
          }
          
          temp3 <- temp2
          ColsToKeep <- c("Indicator",seq(as.numeric(Period2Start),as.numeric(Period2End),1))
          temp3[,which(!(names(temp3) %in% ColsToKeep))] <- NULL
          
          temp3 <- temp3[rowSums(is.na(temp3[,2:ncol(temp3)]))<ncol(temp3)-2,]
          SelPeriodIndic <- subset(temp3,temp3$Indicator=="sadjkhksdf8228282827y^^@&^@")
          
          for (jjj in 1:nrow(temp3)){
            StartYear <- min(as.numeric(names(temp3[jjj,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[jjj,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            EndYear <- max(as.numeric(names(temp3[,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            if (EndYear >= as.numeric(Period2End)-YearOffset & StartYear <= as.numeric(Period2Start)+YearOffset){
              SelPeriodIndic <- rbind(SelPeriodIndic,temp3[jjj,])
            }
          }
          
          #remove all the non-period years, and non-indicator name columns
          #SelPeriodIndic[,which(!(names(SelPeriodIndic) %in% ColsToKeep))] <- NULL
          AvailIndicForPeriod <- nrow(SelPeriodIndic)
          if (nrow(SelPeriodIndic) > 0) {
            SelPeriodIndic$Growth <- as.numeric(NA)
            IndicatorsInSelPeriodIndic <- unique(SelPeriodIndic$Indicator)
            IndicatorsInSelPeriodIndic <- IndicatorsInSelPeriodIndic[!IndicatorsInSelPeriodIndic %in% ExclFromPerfMatrix]
            SelPeriodIndic <- subset(SelPeriodIndic,!(SelPeriodIndic$Indicator %in% ExclFromPerfMatrix))
            
            #for the rankings:
            RankingIndic <- ClioOnlyWithData
            RankingIndic <- RankingIndic[RankingIndic$`end year`=="2012",]
            
            ColsToKeep <- c(ColsToKeep,"country name")
            RankingIndic[,which(!(names(RankingIndic) %in% ColsToKeep))] <- NULL
            RankingIndic <- RankingIndic[which(RankingIndic$Indicator %in% IndicatorsInSelPeriodIndic),]
            RankingIndic$Rank <- as.numeric((NA))
            RankingIndic <- RankingIndic[rowSums(is.na(RankingIndic[,2:ncol(RankingIndic)]))<ncol(RankingIndic)-2,]
            
            # now I need to rank all the variables according to their period's average
            RankingIndic$Avg <- rowMeans(RankingIndic[3:(ncol(RankingIndic)-2)],na.rm = T)
            for (L_kkk in IndicatorsInSelPeriodIndic){
              RankingIndic$Rank[which(RankingIndic$Indicator==L_kkk)] <- match(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)], 
                                                                               sort(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)],
                                                                                    decreasing = ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==L_kkk)]==1,T,F)))
            }
          }
          
          if ( nrow(SelPeriodIndic) >= MinNumOfDescrIndicators) {
            # first get the earliest and last year for each variable and estimate its growth
            for (L_jij in 1:nrow(SelPeriodIndic)){
              L_sds <- SelPeriodIndic[L_jij,2:(ncol(SelPeriodIndic)-1)]
              L_sds[,which(is.na(L_sds[1,]))] <- NULL
              SelPeriodIndic$Growth[L_jij] <- ifelse(L_sds[1,ncol(L_sds)]==L_sds[1,1],0,100*(L_sds[1,ncol(L_sds)]-L_sds[1,1])/L_sds[1,1])[[1]][1]
              SelPeriodIndic$Growth[L_jij] <- ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[L_jij])]==0,-SelPeriodIndic$Growth[L_jij],SelPeriodIndic$Growth[L_jij])
            }
            # there are some variables with infinity (coming from the polity circle), I would group those
            
            XxZzYyPeriod2BestFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Infinite",abs(round(max(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod2BestCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Progressed",ifelse(round(max(SelPeriodIndic$Growth))>0,"Progressed",ifelse(XxZzYyPeriod2BestFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod2BestXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,
                                              paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==Inf)])],
                                                    collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod2BestCharacterizationXxZzYy," ",XxZzYyPeriod2BestFigureXxZzYy,"%\">",sep="")), 
                                              IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.max(SelPeriodIndic$Growth)])]) # the name of the indicator
            
            XxZzYyPeriod2WorstFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Minus Infinity",abs(round(min(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod2WorstCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Collapsed",ifelse(round(min(SelPeriodIndic$Growth),2)>0,"Progressed",ifelse(XxZzYyPeriod2WorstFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod2WorstXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,
                                               paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==-Inf)])],
                                                     collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod2WorstCharacterizationXxZzYy," ",XxZzYyPeriod2WorstFigureXxZzYy,"%\">")), 
                                               IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.min(SelPeriodIndic$Growth)])]) # the name of the indicator
            
          } else {
            XxZzYyPeriod2BestCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                             " available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
            XxZzYyPeriod2BestFigureXxZzYy <- "N/A" # just get the number here (in its absolute)
            XxZzYyPeriod2BestXxZzYy <- "N/A" # the name of the indicator
            
            XxZzYyPeriod2WorstCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                              " available indicators with enough observations in this period",sep = "") 
            XxZzYyPeriod2WorstFigureXxZzYy <- "N/A"
            XxZzYyPeriod2WorstXxZzYy <- "N/A"
            if (AvailIndicForPeriod == 0){
              XxZzYyPeriod2BestCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
              XxZzYyPeriod2WorstCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") 
            }
            
          }
          
          if ( nrow(SelPeriodIndic) >= 2) {
            XxZzYyPeriod2BestRankFigureXxZzYy <- min(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod2BestRankXxZzYy <- paste(IndicPriorityList$ShortName[
              which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & 
              RankingIndic$Rank==XxZzYyPeriod2BestRankFigureXxZzYy)])], collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod2TotalBestRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod2BestRankFigureXxZzYy)][1])],na.rm = T)
            
            XxZzYyPeriod2WorstRankFigureXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod2WorstRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod2WorstRankFigureXxZzYy)])], collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod2TotalWorstRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod2WorstRankFigureXxZzYy)][1])],na.rm = T)
          } else {
            XxZzYyPeriod2BestRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod2TotalBestRankedXxZzYy <- "N/A"
            XxZzYyPeriod2BestRankXxZzYy <- "N/A"
            
            XxZzYyPeriod2WorstRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod2TotalWorstRankedXxZzYy <- "N/A"
            XxZzYyPeriod2WorstRankXxZzYy <- "N/A"
          }
          
          temp3 <- temp2
          ColsToKeep <- c("Indicator",seq(as.numeric(Period3Start),as.numeric(Period3End),1))
          temp3[,which(!(names(temp3) %in% ColsToKeep))] <- NULL
          
          temp3 <- temp3[rowSums(is.na(temp3[,2:ncol(temp3)]))<ncol(temp3)-2,]
          SelPeriodIndic <- subset(temp3,temp3$Indicator=="sadjkhksdf8228282827y^^@&^@")
          
          for (jjj in 1:nrow(temp3)){
            StartYear <- min(as.numeric(names(temp3[jjj,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[jjj,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            EndYear <- max(as.numeric(names(temp3[,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            if (EndYear >= as.numeric(Period3End)-YearOffset & StartYear <= as.numeric(Period3Start)+YearOffset){
              SelPeriodIndic <- rbind(SelPeriodIndic,temp3[jjj,])
            }
          }
          
          #remove all the non-period years, and non-indicator name columns
          #SelPeriodIndic[,which(!(names(SelPeriodIndic) %in% ColsToKeep))] <- NULL
          AvailIndicForPeriod <- nrow(SelPeriodIndic)
          if (nrow(SelPeriodIndic) > 0) {
            SelPeriodIndic$Growth <- as.numeric(NA)
            IndicatorsInSelPeriodIndic <- unique(SelPeriodIndic$Indicator)
            IndicatorsInSelPeriodIndic <- IndicatorsInSelPeriodIndic[!IndicatorsInSelPeriodIndic %in% ExclFromPerfMatrix]
            SelPeriodIndic <- subset(SelPeriodIndic,!(SelPeriodIndic$Indicator %in% ExclFromPerfMatrix))
            
            #for the rankings:
            RankingIndic <- ClioOnlyWithData
            RankingIndic <- RankingIndic[RankingIndic$`end year`=="2012",]
            
            ColsToKeep <- c(ColsToKeep,"country name")
            RankingIndic[,which(!(names(RankingIndic) %in% ColsToKeep))] <- NULL
            RankingIndic <- RankingIndic[which(RankingIndic$Indicator %in% IndicatorsInSelPeriodIndic),]
            RankingIndic$Rank <- as.numeric((NA))
            RankingIndic <- RankingIndic[rowSums(is.na(RankingIndic[,2:ncol(RankingIndic)]))<ncol(RankingIndic)-2,]
            
            # now I need to rank all the variables according to their period's average
            RankingIndic$Avg <- rowMeans(RankingIndic[3:(ncol(RankingIndic)-2)],na.rm = T)
            for (L_kkk in IndicatorsInSelPeriodIndic){
              RankingIndic$Rank[which(RankingIndic$Indicator==L_kkk)] <- match(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)], 
                                                                               sort(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)],
                                                                                    decreasing = ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==L_kkk)]==1,T,F)))
            }
          }
          
          if ( nrow(SelPeriodIndic) >= MinNumOfDescrIndicators) {
            # first get the earliest and last year for each variable and estimate its growth
            for (L_jij in 1:nrow(SelPeriodIndic)){
              L_sds <- SelPeriodIndic[L_jij,2:(ncol(SelPeriodIndic)-1)]
              L_sds[,which(is.na(L_sds[1,]))] <- NULL
              SelPeriodIndic$Growth[L_jij] <- ifelse(L_sds[1,ncol(L_sds)]==L_sds[1,1],0,100*(L_sds[1,ncol(L_sds)]-L_sds[1,1])/L_sds[1,1])[[1]][1]
              SelPeriodIndic$Growth[L_jij] <- ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[L_jij])]==0,-SelPeriodIndic$Growth[L_jij],SelPeriodIndic$Growth[L_jij])
            }
            # there are some variables with infinity (coming from the polity circle), I would group those
            
            XxZzYyPeriod3BestFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Infinite",abs(round(max(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod3BestCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Progressed",ifelse(round(max(SelPeriodIndic$Growth))>0,"Progressed",ifelse(XxZzYyPeriod3BestFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod3BestXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,
                                              paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==Inf)])],
                                                    collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod3BestCharacterizationXxZzYy," ",XxZzYyPeriod3BestFigureXxZzYy,"%\">",sep="")), 
                                              IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.max(SelPeriodIndic$Growth)])]) # the name of the indicator
            
            XxZzYyPeriod3WorstFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Minus Infinity",abs(round(min(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod3WorstCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Collapsed",ifelse(round(min(SelPeriodIndic$Growth),2)>0,"Progressed",ifelse(XxZzYyPeriod3WorstFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod3WorstXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,
                                               paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==-Inf)])],
                                                     collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod3WorstCharacterizationXxZzYy," ",XxZzYyPeriod3WorstFigureXxZzYy,"%\">")), 
                                               IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.min(SelPeriodIndic$Growth)])]) # the name of the indicator
            
          } else {
            XxZzYyPeriod3BestCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                             " available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
            XxZzYyPeriod3BestFigureXxZzYy <- "N/A" # just get the number here (in its absolute)
            XxZzYyPeriod3BestXxZzYy <- "N/A" # the name of the indicator
            
            XxZzYyPeriod3WorstCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                              " available indicators with enough observations in this period",sep = "") 
            XxZzYyPeriod3WorstFigureXxZzYy <- "N/A"
            XxZzYyPeriod3WorstXxZzYy <- "N/A"
            if (AvailIndicForPeriod == 0){
              XxZzYyPeriod3BestCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
              XxZzYyPeriod3WorstCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") 
            }
            
          }
          
          if ( nrow(SelPeriodIndic) >= 2) {
            XxZzYyPeriod3BestRankFigureXxZzYy <- min(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            # XXX Australia produced a Warning here:
            # "In IndicPriorityList$IndicatorsList == RankingIndic$Indicator[which(RankingIndic$`country name` ==  :
            # longer object length is not a multiple of shorter object length"
            # The solution was to use %in% instead of ==
            
            XxZzYyPeriod3BestRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in%
                RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod3BestRankFigureXxZzYy)])], 
                collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod3TotalBestRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod3BestRankFigureXxZzYy)][1])],na.rm = T)
            
            XxZzYyPeriod3WorstRankFigureXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod3WorstRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod3WorstRankFigureXxZzYy)])], collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod3TotalWorstRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod3WorstRankFigureXxZzYy)][1])],na.rm = T)
          } else {
            XxZzYyPeriod3BestRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod3TotalBestRankedXxZzYy <- "N/A"
            XxZzYyPeriod3BestRankXxZzYy <- "N/A"
            
            XxZzYyPeriod3WorstRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod3TotalWorstRankedXxZzYy <- "N/A"
            XxZzYyPeriod3WorstRankXxZzYy <- "N/A"
          }
          
          temp3 <- temp2
          ColsToKeep <- c("Indicator",seq(as.numeric(Period4Start),as.numeric(Period4End),1))
          temp3[,which(!(names(temp3) %in% ColsToKeep))] <- NULL
          
          temp3 <- temp3[rowSums(is.na(temp3[,2:ncol(temp3)]))<ncol(temp3)-2,]
          SelPeriodIndic <- subset(temp3,temp3$Indicator=="sadjkhksdf8228282827y^^@&^@")
          
          for (jjj in 1:nrow(temp3)){
            StartYear <- min(as.numeric(names(temp3[jjj,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[jjj,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            EndYear <- max(as.numeric(names(temp3[,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            if (EndYear >= as.numeric(Period4End)-YearOffset & StartYear <= as.numeric(Period4Start)+YearOffset){
              SelPeriodIndic <- rbind(SelPeriodIndic,temp3[jjj,])
            }
          }
          
          #remove all the non-period years, and non-indicator name columns
          #SelPeriodIndic[,which(!(names(SelPeriodIndic) %in% ColsToKeep))] <- NULL
          AvailIndicForPeriod <- nrow(SelPeriodIndic)
          if (nrow(SelPeriodIndic) > 0) {
            SelPeriodIndic$Growth <- as.numeric(NA)
            IndicatorsInSelPeriodIndic <- unique(SelPeriodIndic$Indicator)
            IndicatorsInSelPeriodIndic <- IndicatorsInSelPeriodIndic[!IndicatorsInSelPeriodIndic %in% ExclFromPerfMatrix]
            SelPeriodIndic <- subset(SelPeriodIndic,!(SelPeriodIndic$Indicator %in% ExclFromPerfMatrix))
            
            #for the rankings:
            RankingIndic <- ClioOnlyWithData
            RankingIndic <- RankingIndic[RankingIndic$`end year`=="2012",]
            
            ColsToKeep <- c(ColsToKeep,"country name")
            RankingIndic[,which(!(names(RankingIndic) %in% ColsToKeep))] <- NULL
            RankingIndic <- RankingIndic[which(RankingIndic$Indicator %in% IndicatorsInSelPeriodIndic),]
            RankingIndic$Rank <- as.numeric((NA))
            RankingIndic <- RankingIndic[rowSums(is.na(RankingIndic[,2:ncol(RankingIndic)]))<ncol(RankingIndic)-2,]
            
            # now I need to rank all the variables according to their period's average
            RankingIndic$Avg <- rowMeans(RankingIndic[3:(ncol(RankingIndic)-2)],na.rm = T)
            for (L_kkk in IndicatorsInSelPeriodIndic){
              RankingIndic$Rank[which(RankingIndic$Indicator==L_kkk)] <- match(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)], 
                                                                               sort(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)],
                                                                                    decreasing = ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==L_kkk)]==1,T,F)))
            }
          }
          
          if ( nrow(SelPeriodIndic) >= MinNumOfDescrIndicators) {
            # first get the earliest and last year for each variable and estimate its growth
            for (L_jij in 1:nrow(SelPeriodIndic)){
              L_sds <- SelPeriodIndic[L_jij,2:(ncol(SelPeriodIndic)-1)]
              L_sds[,which(is.na(L_sds[1,]))] <- NULL
              SelPeriodIndic$Growth[L_jij] <- ifelse(L_sds[1,ncol(L_sds)]==L_sds[1,1],0,100*(L_sds[1,ncol(L_sds)]-L_sds[1,1])/L_sds[1,1])[[1]][1]
              SelPeriodIndic$Growth[L_jij] <- ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[L_jij])]==0,-SelPeriodIndic$Growth[L_jij],SelPeriodIndic$Growth[L_jij])
            }
            # there are some variables with infinity (coming from the polity circle), I would group those
            
            XxZzYyPeriod4BestFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Infinite",abs(round(max(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod4BestCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Progressed",ifelse(round(max(SelPeriodIndic$Growth))>0,"Progressed",ifelse(XxZzYyPeriod4BestFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod4BestXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,
                                              paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==Inf)])],
                                                    collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod4BestCharacterizationXxZzYy," ",XxZzYyPeriod4BestFigureXxZzYy,"%\">",sep="")), 
                                              IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.max(SelPeriodIndic$Growth)])]) # the name of the indicator
            
            XxZzYyPeriod4WorstFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Minus Infinity",abs(round(min(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod4WorstCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Collapsed",ifelse(round(min(SelPeriodIndic$Growth),2)>0,"Progressed",ifelse(XxZzYyPeriod4WorstFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod4WorstXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,
                                               paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==-Inf)])],
                                                     collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",
                                                        XxZzYyPeriod4WorstCharacterizationXxZzYy," ",XxZzYyPeriod4WorstFigureXxZzYy,"%\">")), 
                                               IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.min(SelPeriodIndic$Growth)])]) # the name of the indicator
            
          } else {
            XxZzYyPeriod4BestCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                             " available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
            XxZzYyPeriod4BestFigureXxZzYy <- "N/A" # just get the number here (in its absolute)
            XxZzYyPeriod4BestXxZzYy <- "N/A" # the name of the indicator
            
            XxZzYyPeriod4WorstCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                              " available indicators with enough observations in this period",sep = "") 
            XxZzYyPeriod4WorstFigureXxZzYy <- "N/A"
            XxZzYyPeriod4WorstXxZzYy <- "N/A"
            if (AvailIndicForPeriod == 0){
              XxZzYyPeriod4BestCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
              XxZzYyPeriod4WorstCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") 
            }
            
          }
          
          if ( nrow(SelPeriodIndic) >= 2) {
            XxZzYyPeriod4BestRankFigureXxZzYy <- min(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod4BestRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod4BestRankFigureXxZzYy)])], 
                                                 collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod4TotalBestRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==
                RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod4BestRankFigureXxZzYy)][1])],na.rm = T)
            
            XxZzYyPeriod4WorstRankFigureXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod4WorstRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod4WorstRankFigureXxZzYy)])], 
                                                  collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod4TotalWorstRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==
                RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod4WorstRankFigureXxZzYy)][1])],na.rm = T)
          } else {
            XxZzYyPeriod4BestRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod4TotalBestRankedXxZzYy <- "N/A"
            XxZzYyPeriod4BestRankXxZzYy <- "N/A"
            
            XxZzYyPeriod4WorstRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod4TotalWorstRankedXxZzYy <- "N/A"
            XxZzYyPeriod4WorstRankXxZzYy <- "N/A"
          }
          
          temp3 <- temp2
          ColsToKeep <- c("Indicator",seq(as.numeric(Period5Start),as.numeric(Period5End),1))
          temp3[,which(!(names(temp3) %in% ColsToKeep))] <- NULL
          
          temp3 <- temp3[rowSums(is.na(temp3[,2:ncol(temp3)]))<ncol(temp3)-2,]
          SelPeriodIndic <- subset(temp3,temp3$Indicator=="sadjkhksdf8228282827y^^@&^@")
          
          for (jjj in 1:nrow(temp3)){
            StartYear <- min(as.numeric(names(temp3[jjj,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[jjj,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            EndYear <- max(as.numeric(names(temp3[,which(
              !is.na(temp3[jjj,]))])[2:length(temp3[,which(!is.na(temp3[jjj,]))])]),na.rm = T)
            if (EndYear >= as.numeric(Period5End)-YearOffset & StartYear <= as.numeric(Period5Start)+YearOffset){
              SelPeriodIndic <- rbind(SelPeriodIndic,temp3[jjj,])
            }
          }
          
          #remove all the non-period years, and non-indicator name columns
          #SelPeriodIndic[,which(!(names(SelPeriodIndic) %in% ColsToKeep))] <- NULL
          AvailIndicForPeriod <- nrow(SelPeriodIndic)
          if (nrow(SelPeriodIndic) > 0) {
            SelPeriodIndic$Growth <- as.numeric(NA)
            IndicatorsInSelPeriodIndic <- unique(SelPeriodIndic$Indicator)
            IndicatorsInSelPeriodIndic <- IndicatorsInSelPeriodIndic[!IndicatorsInSelPeriodIndic %in% ExclFromPerfMatrix]
            SelPeriodIndic <- subset(SelPeriodIndic,!(SelPeriodIndic$Indicator %in% ExclFromPerfMatrix))
            
            #for the rankings:
            RankingIndic <- ClioOnlyWithData
            RankingIndic <- RankingIndic[RankingIndic$`end year`=="2012",]
            
            ColsToKeep <- c(ColsToKeep,"country name")
            RankingIndic[,which(!(names(RankingIndic) %in% ColsToKeep))] <- NULL
            RankingIndic <- RankingIndic[which(RankingIndic$Indicator %in% IndicatorsInSelPeriodIndic),]
            RankingIndic$Rank <- as.numeric((NA))
            RankingIndic <- RankingIndic[rowSums(is.na(RankingIndic[,2:ncol(RankingIndic)]))<ncol(RankingIndic)-2,]
            
            # now I need to rank all the variables according to their period's average
            RankingIndic$Avg <- rowMeans(RankingIndic[3:(ncol(RankingIndic)-2)],na.rm = T)
            for (L_kkk in IndicatorsInSelPeriodIndic){
              RankingIndic$Rank[which(RankingIndic$Indicator==L_kkk)] <- match(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)], 
                                                                               sort(RankingIndic$Avg[which(RankingIndic$Indicator==L_kkk)],
                                                                                    decreasing = ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==L_kkk)]==1,T,F)))
            }
          }
          
          if ( nrow(SelPeriodIndic) >= MinNumOfDescrIndicators) {
            # first get the earliest and last year for each variable and estimate its growth
            for (L_jij in 1:nrow(SelPeriodIndic)){
              L_sds <- SelPeriodIndic[L_jij,2:(ncol(SelPeriodIndic)-1)]
              L_sds[,which(is.na(L_sds[1,]))] <- NULL
              SelPeriodIndic$Growth[L_jij] <- ifelse(L_sds[1,ncol(L_sds)]==L_sds[1,1],0,100*(L_sds[1,ncol(L_sds)]-L_sds[1,1])/L_sds[1,1])[[1]][1]
              SelPeriodIndic$Growth[L_jij] <- ifelse(IndicPriorityList$Benign[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[L_jij])]==0,-SelPeriodIndic$Growth[L_jij],SelPeriodIndic$Growth[L_jij])
            }
            # there are some variables with infinity (coming from the polity circle), I would group those
            
            XxZzYyPeriod5BestFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Infinite",abs(round(max(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod5BestCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,"Progressed",ifelse(round(max(SelPeriodIndic$Growth))>0,"Progressed",ifelse(XxZzYyPeriod5BestFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod5BestXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==Inf))>0,
                                              paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==Inf)])],
                                                    collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod5BestCharacterizationXxZzYy," ",XxZzYyPeriod5BestFigureXxZzYy,"%\">",sep="")), 
                                              IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.max(SelPeriodIndic$Growth)])]) # the name of the indicator
            
            XxZzYyPeriod5WorstFigureXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Minus Infinity",abs(round(min(SelPeriodIndic$Growth),2))) # just get the number here (in its absolute)
            XxZzYyPeriod5WorstCharacterizationXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,"Collapsed",ifelse(round(min(SelPeriodIndic$Growth),2)>0,"Progressed",ifelse(XxZzYyPeriod5WorstFigureXxZzYy==0,"Stagnated","Regressed"))) # Progressed/Stagnated/Regressed
            XxZzYyPeriod5WorstXxZzYy <- ifelse(length(which(SelPeriodIndic$Growth==-Inf))>0,
                                               paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% SelPeriodIndic$Indicator[which(SelPeriodIndic$Growth==-Inf)])],
                                                     collapse = paste("</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"",XxZzYyPeriod5WorstCharacterizationXxZzYy," ",XxZzYyPeriod5WorstFigureXxZzYy,"%\">")), 
                                               IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList==SelPeriodIndic$Indicator[which.min(SelPeriodIndic$Growth)])]) # the name of the indicator
            
          } else {
            XxZzYyPeriod5BestCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                             " available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
            XxZzYyPeriod5BestFigureXxZzYy <- "N/A" # just get the number here (in its absolute)
            XxZzYyPeriod5BestXxZzYy <- "N/A" # the name of the indicator
            
            XxZzYyPeriod5WorstCharacterizationXxZzYy <- paste("Only ", AvailIndicForPeriod,
                                                              " available indicators with enough observations in this period",sep = "") 
            XxZzYyPeriod5WorstFigureXxZzYy <- "N/A"
            XxZzYyPeriod5WorstXxZzYy <- "N/A"
            if (AvailIndicForPeriod == 0){
              XxZzYyPeriod5BestCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") # Progressed/Stagnated/Regressed
              XxZzYyPeriod5WorstCharacterizationXxZzYy <- paste("No available indicators with enough observations in this period",sep = "") 
            }
            
          }
          
          if ( nrow(SelPeriodIndic) >= 2) {
            XxZzYyPeriod5BestRankFigureXxZzYy <- min(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod5BestRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod5BestRankFigureXxZzYy)])], collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod5TotalBestRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod5BestRankFigureXxZzYy)][1])],na.rm = T)
            
            XxZzYyPeriod5WorstRankFigureXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$`country name`==CountryList[i])])
            XxZzYyPeriod5WorstRankXxZzYy <- paste(IndicPriorityList$ShortName[which(IndicPriorityList$IndicatorsList %in% RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod5WorstRankFigureXxZzYy)])], collapse = "</span><br><span class=\"badge\" data-toggle=\"tooltip\" title=\"Equally ranked to previous indicator\">")
            XxZzYyPeriod5TotalWorstRankedXxZzYy <- max(RankingIndic$Rank[which(RankingIndic$Indicator==RankingIndic$Indicator[which(RankingIndic$`country name`==CountryList[i] & RankingIndic$Rank==XxZzYyPeriod5WorstRankFigureXxZzYy)][1])],na.rm = T)
          } else {
            XxZzYyPeriod5BestRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod5TotalBestRankedXxZzYy <- "N/A"
            XxZzYyPeriod5BestRankXxZzYy <- "N/A"
            
            XxZzYyPeriod5WorstRankFigureXxZzYy <- "N/A"
            XxZzYyPeriod5TotalWorstRankedXxZzYy <- "N/A"
            XxZzYyPeriod5WorstRankXxZzYy <- "N/A"
          }
          
          #### a list of webnames of indicators and the Indicator's name as it appears in the data is necessary to continue ####
          
          # the list of web names and their corresponding Indicator Name 
          IndicDictionary <- read_excel('/home/michalis/PhD/Clio Infra/UPDATE 20210315/IndicPriorityList.xlsx', sheet = 3)
          
          #### I need a full list of indicators with start year, end year and number of observations ####
          Durations <- temp[c("Indicator")]
          Durations$StartYear <- as.numeric(NA)
          Durations$EndYear <- as.numeric(NA)
          Durations$Obs <- as.numeric(NA)
          
          for (L_jjj in 1:nrow(Durations)){
            L_sds <- temp[L_jjj,9:ncol(temp)]
            L_sds[,which(is.na(L_sds[1,]))] <- NULL
            Durations$StartYear[L_jjj] <- names(L_sds)[1]
            Durations$EndYear[L_jjj] <- names(L_sds)[ncol(L_sds)]
            Durations$Obs[L_jjj] <- ncol(L_sds)
            Durations$Indicator[L_jjj] <- IndicDictionary$WebName[which(IndicDictionary$Indicator==Durations$Indicator[L_jjj])]
          }
          
          # I need to convert all to web names otherwise there will be missing values:
          
          # add the missing indicators with "-" instead of real values
          # it would be really hard to remove the badge, perhaps I correct for that later
          MissingIndic <- ClioMetaData$WebName[which(!(c(ClioMetaData$WebName) %in% c(Durations$Indicator)))]
          if (length(MissingIndic)>0){
            for (L_pop in 1:length(MissingIndic)){
              TempLine <- data.frame(Indicator=MissingIndic[L_pop],
                                     StartYear="",
                                     EndYear="",
                                     Obs="No Data")
              Durations <- rbind(Durations,TempLine)
            }
          }
          
          ### Indicators list ###
          
          # file name structure: CountryName_IndicatorName_TerritorialRef_Year1_Year2_CCODE.xlsx
          CountryIndicatorFilename <- 
            paste0(trimalls(gsub("[[:punct:]]", "", temp$`country name`[1])),
                   "_XXXNameOfIndicatorXXX_TerritorialRef_",temp$`start year`[1],"_",temp$`end year`[1],
                   "_CCode_",temp$ccode[1],".xlsx")
          
          XxZzYyAgricultureXxZzYy <- f_IndicatorMenu("Agriculture",ClioMetaData,Durations,
                                                     XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyDemographyXxZzYy <- f_IndicatorMenu("Demography",ClioMetaData,Durations,
                                                    XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyEnvironmentXxZzYy <- f_IndicatorMenu("Environment",ClioMetaData,Durations,
                                                     XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyFinanceXxZzYy <- f_IndicatorMenu("Finance",ClioMetaData,Durations,
                                                 XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyGenderEqualityXxZzYy <- f_IndicatorMenu("Gender Equality",ClioMetaData,Durations,
                                                        XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyHumanCapitalXxZzYy <- f_IndicatorMenu("Human Capital",ClioMetaData,Durations,
                                                      XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyInstitutionsXxZzYy <- f_IndicatorMenu("Institutions",ClioMetaData,Durations,
                                                      XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyLabourRelationsXxZzYy <- f_IndicatorMenu("Labour Relations",ClioMetaData,Durations,
                                                         XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyNationalAccountsXxZzYy <- f_IndicatorMenu("National Accounts",ClioMetaData,Durations,
                                                          XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyPricesAndWagesXxZzYy <- f_IndicatorMenu("Prices and Wages",ClioMetaData,Durations,
                                                        XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          XxZzYyProductionXxZzYy <- f_IndicatorMenu("Production",ClioMetaData,Durations,
                                                    XxZzYyISO3XxZzYy,CountryIndicatorFilename)
          
          test <- gsub("XxZzYyCountryShortXxZzYy", XxZzYyCountryShortXxZzYy, test)
          test <- gsub("XxZzYyCountryFullXxZzYy", XxZzYyCountryFullXxZzYy, test)
          test <- gsub("XxZzYyCountryShortNoSpaceXxZzYy", XxZzYyCountryShortNoSpaceXxZzYy, test)
          test <- gsub("XxZzYyISO3XxZzYy", XxZzYyISO3XxZzYy, test)
          test <- gsub("XxZzYyBorderPeriodsXxZzYy", XxZzYyBorderPeriodsXxZzYy, test)
          test <- gsub("XxZzYyISO3numericXxZzYy", XxZzYyISO3numericXxZzYy, test)
          test <- gsub("XxZzYyFoundationDateXxZzYy", XxZzYyFoundationDateXxZzYy, test)
          test <- gsub("XxZzYyRegionUNXxZzYy", XxZzYyRegionUNXxZzYy, test)
          test <- gsub("XxZzYyRegionOECDXxZzYy", XxZzYyRegionOECDXxZzYy, test)
          test <- gsub("XxZzYyEarliestRankingDateXxZzYy", XxZzYyEarliestRankingDateXxZzYy, test)
          test <- gsub("XxZzYyEarliestRankNumCountriesXxZzYy", XxZzYyEarliestRankNumCountriesXxZzYy, test)
          test <- gsub("XxZzYyEarliestRankXxZzYy", XxZzYyEarliestRankXxZzYy, test)
          test <- gsub("XxZzYyLatestRankingDateXxZzYy", XxZzYyLatestRankingDateXxZzYy, test)
          test <- gsub("XxZzYyLatestRankNumCountriesXxZzYy", XxZzYyLatestRankNumCountriesXxZzYy, test)
          test <- gsub("XxZzYyLatestRankXxZzYy", XxZzYyLatestRankXxZzYy, test)
          
          test <- gsub("XxZzYyPeriod1CountXxZzYy", XxZzYyPeriod1CountXxZzYy, test)
          test <- gsub("XxZzYyPeriod2CountXxZzYy", XxZzYyPeriod2CountXxZzYy, test)
          test <- gsub("XxZzYyPeriod3CountXxZzYy", XxZzYyPeriod3CountXxZzYy, test)
          test <- gsub("XxZzYyPeriod4CountXxZzYy", XxZzYyPeriod4CountXxZzYy, test)
          test <- gsub("XxZzYyPeriod5CountXxZzYy", XxZzYyPeriod5CountXxZzYy, test)
          test <- gsub("XxZzYyPeriod1BestCharacterizationXxZzYy", XxZzYyPeriod1BestCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod1BestFigureXxZzYy", XxZzYyPeriod1BestFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod1BestXxZzYy", XxZzYyPeriod1BestXxZzYy, test)
          test <- gsub("XxZzYyPeriod1WorstCharacterizationXxZzYy", XxZzYyPeriod1WorstCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod1WorstFigureXxZzYy", XxZzYyPeriod1WorstFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod1WorstXxZzYy", XxZzYyPeriod1WorstXxZzYy, test)
          test <- gsub("XxZzYyPeriod1BestRankFigureXxZzYy", XxZzYyPeriod1BestRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod1TotalBestRankedXxZzYy", XxZzYyPeriod1TotalBestRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod1TotalWorstRankedXxZzYy", XxZzYyPeriod1TotalWorstRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod1BestRankXxZzYy", XxZzYyPeriod1BestRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod1WorstRankFigureXxZzYy", XxZzYyPeriod1WorstRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod1WorstRankXxZzYy", XxZzYyPeriod1WorstRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod2BestCharacterizationXxZzYy", XxZzYyPeriod2BestCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod2BestFigureXxZzYy", XxZzYyPeriod2BestFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod2BestXxZzYy", XxZzYyPeriod2BestXxZzYy, test)
          test <- gsub("XxZzYyPeriod2WorstCharacterizationXxZzYy", XxZzYyPeriod2WorstCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod2WorstFigureXxZzYy", XxZzYyPeriod2WorstFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod2WorstXxZzYy", XxZzYyPeriod2WorstXxZzYy, test)
          test <- gsub("XxZzYyPeriod2BestRankFigureXxZzYy", XxZzYyPeriod2BestRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod2TotalBestRankedXxZzYy", XxZzYyPeriod2TotalBestRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod2TotalWorstRankedXxZzYy", XxZzYyPeriod2TotalWorstRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod2BestRankXxZzYy", XxZzYyPeriod2BestRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod2WorstRankFigureXxZzYy", XxZzYyPeriod2WorstRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod2WorstRankXxZzYy", XxZzYyPeriod2WorstRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod3BestCharacterizationXxZzYy", XxZzYyPeriod3BestCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod3BestFigureXxZzYy", XxZzYyPeriod3BestFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod3BestXxZzYy", XxZzYyPeriod3BestXxZzYy, test)
          test <- gsub("XxZzYyPeriod3WorstCharacterizationXxZzYy", XxZzYyPeriod3WorstCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod3WorstFigureXxZzYy", XxZzYyPeriod3WorstFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod3WorstXxZzYy", XxZzYyPeriod3WorstXxZzYy, test)
          test <- gsub("XxZzYyPeriod3BestRankFigureXxZzYy", XxZzYyPeriod3BestRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod3TotalBestRankedXxZzYy", XxZzYyPeriod3TotalBestRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod3TotalWorstRankedXxZzYy", XxZzYyPeriod3TotalWorstRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod3BestRankXxZzYy", XxZzYyPeriod3BestRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod3WorstRankFigureXxZzYy", XxZzYyPeriod3WorstRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod3WorstRankXxZzYy", XxZzYyPeriod3WorstRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod4BestCharacterizationXxZzYy", XxZzYyPeriod4BestCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod4BestFigureXxZzYy", XxZzYyPeriod4BestFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod4BestXxZzYy", XxZzYyPeriod4BestXxZzYy, test)
          test <- gsub("XxZzYyPeriod4WorstCharacterizationXxZzYy", XxZzYyPeriod4WorstCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod4WorstFigureXxZzYy", XxZzYyPeriod4WorstFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod4WorstXxZzYy", XxZzYyPeriod4WorstXxZzYy, test)
          test <- gsub("XxZzYyPeriod4BestRankFigureXxZzYy", XxZzYyPeriod4BestRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod4TotalBestRankedXxZzYy", XxZzYyPeriod4TotalBestRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod4TotalWorstRankedXxZzYy", XxZzYyPeriod4TotalWorstRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod4BestRankXxZzYy", XxZzYyPeriod4BestRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod4WorstRankFigureXxZzYy", XxZzYyPeriod4WorstRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod4WorstRankXxZzYy", XxZzYyPeriod4WorstRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod5BestCharacterizationXxZzYy", XxZzYyPeriod5BestCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod5BestFigureXxZzYy", XxZzYyPeriod5BestFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod5BestXxZzYy", XxZzYyPeriod5BestXxZzYy, test)
          test <- gsub("XxZzYyPeriod5WorstCharacterizationXxZzYy", XxZzYyPeriod5WorstCharacterizationXxZzYy, test)
          test <- gsub("XxZzYyPeriod5WorstFigureXxZzYy", XxZzYyPeriod5WorstFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod5WorstXxZzYy", XxZzYyPeriod5WorstXxZzYy, test)
          test <- gsub("XxZzYyPeriod5BestRankFigureXxZzYy", XxZzYyPeriod5BestRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod5TotalBestRankedXxZzYy", XxZzYyPeriod5TotalBestRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod5TotalWorstRankedXxZzYy", XxZzYyPeriod5TotalWorstRankedXxZzYy, test)
          test <- gsub("XxZzYyPeriod5BestRankXxZzYy", XxZzYyPeriod5BestRankXxZzYy, test)
          test <- gsub("XxZzYyPeriod5WorstRankFigureXxZzYy", XxZzYyPeriod5WorstRankFigureXxZzYy, test)
          test <- gsub("XxZzYyPeriod5WorstRankXxZzYy", XxZzYyPeriod5WorstRankXxZzYy, test)
      
          #test <- gsub("XxZzYyAnyPeriodsBestCharacterizationXxZzYy", XxZzYyAnyPeriodsBestCharacterizationXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsBestFigureXxZzYy", XxZzYyAnyPeriodsBestFigureXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsBestYearXxZzYy", XxZzYyAnyPeriodsBestYearXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsBestXxZzYy", XxZzYyAnyPeriodsBestXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsWorstCharacterizationXxZzYy", XxZzYyAnyPeriodsWorstCharacterizationXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsWorstFigureXxZzYy", XxZzYyAnyPeriodsWorstFigureXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsWorstYearXxZzYy", XxZzYyAnyPeriodsWorstYearXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsWorstXxZzYy", XxZzYyAnyPeriodsWorstXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsBestRankFigureXxZzYy", XxZzYyAnyPeriodsBestRankFigureXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsTotalBestRankedXxZzYy", XxZzYyAnyPeriodsTotalBestRankedXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsTotalWorstRankedXxZzYy", XxZzYyAnyPeriodsTotalWorstRankedXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsBestRankYearXxZzYy", XxZzYyAnyPeriodsBestRankYearXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsBestRankXxZzYy", XxZzYyAnyPeriodsBestRankXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsWorstRankFigureXxZzYy", XxZzYyAnyPeriodsWorstRankFigureXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsWorstRankYearXxZzYy", XxZzYyAnyPeriodsWorstRankYearXxZzYy, test)
          #test <- gsub("XxZzYyAnyPeriodsWorstRankXxZzYy", XxZzYyAnyPeriodsWorstRankXxZzYy, test)
          
          # Indicators
          test <- gsub("XxZzYyAgricultureXxZzYy", XxZzYyAgricultureXxZzYy, test)
          test <- gsub("XxZzYyDemographyXxZzYy", XxZzYyDemographyXxZzYy, test)
          test <- gsub("XxZzYyEnvironmentXxZzYy", XxZzYyEnvironmentXxZzYy, test)
          test <- gsub("XxZzYyFinanceXxZzYy", XxZzYyFinanceXxZzYy, test)
          test <- gsub("XxZzYyGenderEqualityXxZzYy", XxZzYyGenderEqualityXxZzYy, test)
          test <- gsub("XxZzYyHumanCapitalXxZzYy", XxZzYyHumanCapitalXxZzYy, test)
          test <- gsub("XxZzYyInstitutionsXxZzYy", XxZzYyInstitutionsXxZzYy, test)
          test <- gsub("XxZzYyLabourRelationsXxZzYy", XxZzYyLabourRelationsXxZzYy, test)
          test <- gsub("XxZzYyNationalAccountsXxZzYy", XxZzYyNationalAccountsXxZzYy, test)
          test <- gsub("XxZzYyPricesAndWagesXxZzYy", XxZzYyPricesAndWagesXxZzYy, test)
          test <- gsub("XxZzYyProductionXxZzYy", XxZzYyProductionXxZzYy, test)
          
          # Header data
          #test <- gsub("XxZzYyTotalDatasetsXxZzYy", XxZzYyTotalDatasetsXxZzYy, test)
          #test <- gsub("XxZzYyTotalNumOfCountriesXxZzYy", XxZzYyTotalNumOfCountriesXxZzYy, test)
          
          # About Clio Infra
          #test <- gsub("XxZzYyAboutClioInfraXxZzYy", XxZzYyAboutClioInfraXxZzYy, test)
          
          
          ### Now exporting indicator and country files
          # Now I want to export separate xls files for each country and indicator combination available
          
          # get the list of indicators for the current country
          ForExport <- unique(temp$Indicator)
          # make a copy to later contain the Web Names
          ForExportNames <- ForExport
          # get the web names for the copy
          for (L_jjj in 1:length(ForExport)){
            ForExportNames[L_jjj] <- 
              IndicPriorityList$WebName[which(IndicPriorityList$IndicatorsList==ForExportNames[L_jjj])]
          }
          
          # loop through the data for this country and save each file
          if (ExportData){
            for (L_ij in 1:length(ForExport)){
              dataframe1 <- subset(temp,temp$Indicator==ForExport[L_ij])
              
              # file name structure: CountryName_IndicatorName_TerritorialRef_Year1_Year2_CCODE.xlsx
              CountryIndicatorFilename <- 
                paste0(trimalls(gsub("[[:punct:]]", "", dataframe1$`country name`)),
                       "_",trimalls(gsub("[[:punct:]]", "", ForExportNames[L_ij])),
                       "_TerritorialRef_",dataframe1$`start year`,"_",dataframe1$`end year`,
                       "_CCode_",dataframe1$ccode,".xlsx")
              
              CountryIndicatorFilenamePath <- 
                paste0(GenericPath,"/IndicatorsPerCountry/",CountryIndicatorFilename)
              
              dataframe1$`Webmapper code`<- NULL
              dataframe1$`Webmapper numeric code` <- NULL
              dataframe1$`start year` <- NULL
              dataframe1$`end year` <- NULL
              dataframe1$Filename <- NULL
              
              StartYears <- c(Durations$StartYear[which(Durations$Indicator==ForExportNames[L_ij])])
              EndYears <- c(Durations$EndYear[which(Durations$Indicator==ForExportNames[L_ij])])
              LocalYears <- seq(as.numeric(StartYears),as.numeric(EndYears),1)
              dataframe1b <- expand.grid(dataframe1$`ccode`[1],dataframe1$`country name`[1],
                                         dataframe1$Indicator[1],LocalYears, stringsAsFactors = F,KEEP.OUT.ATTRS = F)
              names(dataframe1b) <- c("Country Code", "Country Name", "Indicator", "Year")
              
              dataframe1b$Data <- 
                t(dataframe1[,which(names(dataframe1)==dataframe1b$Year[1]):
                               which(names(dataframe1)==dataframe1b$Year[length(dataframe1b$Year)])])
              
              write.xlsx2(dataframe1b, file=CountryIndicatorFilenamePath, sheetName="Data", row.names=F, showNA=F)
              
              Metadata <- t(c("Downloaded from",paste0(URL_basis,"/IndicatorsPerCountry/",CountryIndicatorFilename)))
              Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
              names(Metadata) <- c("Description","Value")
              
              FullMetadata <- Metadata
              
              CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==ForExport[L_ij])]
              CitationFileName <- as.character(CitationFileName)
              
              bib <- ReadBib(paste0(GenericPath,"/Citations/",CitationFileName,".bib"))[1]
              
              TheAuthorAndDate <- capture.output(print(bib, .opts = list(bib.style = "authoryear", first.inits = FALSE, no.print.fields = c("title","publisher","url"))))
              xxxTemp <- unlist(bib)
              
              Metadata <- t(c("Text Citation",paste(TheAuthorAndDate," ",xxxTemp$title,". ", xxxTemp$url,", accessed via the Clio Infra website.", sep = "")))
              Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
              names(Metadata) <- c("Description","Value")
              
              FullMetadata <- rbind(FullMetadata,Metadata)
              
              
              CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==ForExport[L_ij])]
              Metadata <- t(c("XML Citation",paste0(URL_basis,"/Citations/",CitationFileName,".xml")))
              Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
              names(Metadata) <- c("Description","Value")
              
              FullMetadata <- rbind(FullMetadata,Metadata)
              
              Metadata <- t(c("RIS Citation",paste0(URL_basis,"/Citations/",CitationFileName,".ris")))
              Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
              names(Metadata) <- c("Description","Value")
              
              FullMetadata <- rbind(FullMetadata,Metadata)
              
              Metadata <- t(c("BIB Citation",paste0(URL_basis,"/Citations/",CitationFileName,".bib")))
              Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
              names(Metadata) <- c("Description","Value")
              
              FullMetadata <- rbind(FullMetadata,Metadata)
              
              write.xlsx2(FullMetadata, file=CountryIndicatorFilenamePath, sheetName="Metadata", append=TRUE, row.names=F)
              
            } # export individual datasets loop
          
          # now export all available data for the country:
          # with four metadata: download location, ris citations, bib citations and xml citations
          
          } # only run if ExportData is true
          
          CountryIndicatorFilename <- 
            paste0(trimalls(gsub("[[:punct:]]", "", temp$`country name`[1])),"_AllIndicatorsAvailable",
                   "_TerritorialRef_",temp$`start year`[1],"_",temp$`end year`[1],
                   "_CCode_",temp$ccode[1],".xlsx")
          
          CountryIndicatorFilenamePath <- 
            paste0(GenericPath,"/CountryData/",CountryIndicatorFilename)
          
          tempDurations <- Durations
          tempDurations$CountryName <- temp$`country name`[1]
          tempDurations$CCode <- temp$ccode[1]
          tempDurations$ISO3 <- XxZzYyISO3XxZzYy
          tempDurations$CountryFileName <- CountryIndicatorFilename
          
          if (exists("GlobalDurations")){
            GlobalDurations <- rbind(GlobalDurations,tempDurations)
          } else {
            GlobalDurations <- tempDurations
          }
          rm(tempDurations)
          
          test <- gsub("XxZzYyCountryIndicatorFilenameXxZzYy", CountryIndicatorFilename, test)
          
          LocalTemp <- temp
          LocalTemp$`Webmapper code`<- NULL
          LocalTemp$`Webmapper numeric code` <- NULL
          LocalTemp$`start year` <- NULL
          LocalTemp$`end year` <- NULL
          LocalTemp$Filename <- NULL
          names(LocalTemp)[1:3] <- c("Country Code", "Country Name", "Indicator")
          
          # order them alphabetically
          LocalTemp <- LocalTemp[ order(LocalTemp$Indicator), ]
          
          fLocalTemp <- LocalTemp
          fLocalTemp$`Country Code` <- factor(fLocalTemp$`Country Code`)
          fLocalTemp$`Country Name` <- factor(fLocalTemp$`Country Name`)
          fLocalTemp$Indicator <- factor(fLocalTemp$Indicator)
          
          LocalTempLongFormat <- gather(fLocalTemp,year,value,which(names(fLocalTemp)=="1500"):which(names(fLocalTemp)=="2015"))
          LocalTempLongFormat <- LocalTempLongFormat[!is.na(LocalTempLongFormat$value),]
          
          if (ExportCountryData){
            
            ttt <- as.data.frame(as.matrix(LocalTemp),stringsAsFactors = F)
            ttt[,c(1,seq(4,ncol(ttt),1))] <-  lapply(ttt[,c(1,seq(4,ncol(ttt),1))], function (x) as.numeric(x))
            
            write.xlsx2(ttt, file=CountryIndicatorFilenamePath, sheetName="Data Clio Infra Format", row.names=F, showNA=F)
            rm(ttt)
            
            ttt <- as.data.frame(as.matrix(LocalTempLongFormat),stringsAsFactors = F)
            ttt[,c(1,seq(4,ncol(ttt),1))] <-  lapply(ttt[,c(1,seq(4,ncol(ttt),1))], function (x) as.numeric(x))
            
            write.xlsx2(ttt, file=CountryIndicatorFilenamePath, sheetName="Data Long Format", append=TRUE,
                        row.names=F, showNA=F)
            rm(ttt)
            
            # fetch the download location:
            Metadata <- t(c("Downloaded from",paste0(URL_basis,"/CountryData/",CountryIndicatorFilename)))
            Metadata <- as.data.frame(Metadata)
            names(Metadata) <- c("Description","Value")
            
            write.xlsx2(Metadata, file=CountryIndicatorFilenamePath, sheetName="URL", append=TRUE, row.names=F)
            
            # now get the citations for all the data:
            CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator %in% unique(LocalTemp$Indicator))]
            CitationIndicators <- Citations$Indicator[which(Citations$Indicator %in% unique(LocalTemp$Indicator))]
            
            # first place the text based:
            FullMetadata <- NULL
            for (i_bib in 1:length(CitationFileName)){
              bib <- ReadBib(paste0(GenericPath,"/Citations/",CitationFileName,".bib")[i_bib])[1]
              
              TheAuthorAndDate <- capture.output(print(bib, .opts = list(bib.style = "authoryear", first.inits = FALSE, no.print.fields = c("title","publisher","url"))))
              xxxTemp <- unlist(bib)
              
              Metadata <- t(c("Text Citation",paste(TheAuthorAndDate," ",xxxTemp$title,". ", xxxTemp$url,", accessed via the Clio Infra website.", sep = "")))
              Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
              names(Metadata) <- c("Description","Value")
              FullMetadata <- rbind(FullMetadata,Metadata)
            }
            
            write.xlsx(FullMetadata, file=CountryIndicatorFilenamePath, sheetName="Text Citations", append=TRUE, row.names=F)
            
            Metadata <- c(paste0(URL_basis,"/Citations/",CitationFileName,".xml"))
            Metadata <- as.data.frame(Metadata)
            names(Metadata) <- c("Value")
            Metadata$Description <- CitationIndicators
            Metadata <- Metadata[,c(2,1)]
            Metadata <- Metadata[ order(Metadata[,1]),]
            
            write.xlsx2(Metadata, file=CountryIndicatorFilenamePath, sheetName="XML Citations", append=TRUE, row.names=F)
            
            Metadata <- c(paste0(URL_basis,"/Citations/",CitationFileName,".ris"))
            Metadata <- as.data.frame(Metadata)
            names(Metadata) <- c("Value")
            Metadata$Description <- CitationIndicators
            Metadata <- Metadata[,c(2,1)]
            Metadata <- Metadata[ order(Metadata[,1]),]
            
            write.xlsx2(Metadata, file=CountryIndicatorFilenamePath, sheetName="RIS Citations", append=TRUE, row.names=F)
            
            Metadata <- c(paste0(URL_basis,"/Citations/",CitationFileName,".bib"))
            Metadata <- as.data.frame(Metadata)
            names(Metadata) <- c("Value")
            Metadata$Description <- CitationIndicators
            Metadata <- Metadata[,c(2,1)]
            Metadata <- Metadata[ order(Metadata[,1]),]
            
            write.xlsx2(Metadata, file=CountryIndicatorFilenamePath, sheetName="BIB Citations", append=TRUE, row.names=F)
            
          }# end of exporting the available data and metadata for a given country
          
          # export the HTML page:
          write(test, countryHTMLfile)
          print(paste("Exported page for: ",CountryList[i], sep=""))
          rm(test)
          rm(list=ls(pattern="^XxZzYy"))
          
        } else {
          #if (nrow(temp)>0)
          print(paste("Only : ",nrow(temp)," variables for ",CountryList[i], sep=""))
        }
      }# if (GlobalMetadata$DataPoints[which(GlobalMetadata$ClioInfraCountryName==CountryList[i])]>0)
    }
  } # if for the create the page or not in case it already exists
} # outer for loop

beep(6)

ttt<-as.data.frame(as.matrix(GlobalDurations),stringsAsFactors = F)
ttt[,c(2,3,4,6)] <-  lapply(ttt[,c(2,3,4,6)], function (x) as.numeric(x))
write.xlsx2(ttt, "/home/michalis/PhD/Clio Infra/UPDATE 20210315/GlobalDurations.xlsx",sheetName = "Data",row.names = F, append = F, showNA = F)
