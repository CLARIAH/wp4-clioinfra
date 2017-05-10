# substitute for the news and publication items on the footer for all pages created:

# this script should be updating also all main variables subject to frequent change:

XxZzYyTotalDatasetsXxZzYy <- nrow(ClioMetaData)
XxZzYyTotalNumOfCountriesXxZzYy <- nrow(subset(GlobalMetadata,GlobalMetadata$DataPoints>0))
XxZzYyAboutClioInfraXxZzYy <- readChar(paste(GenericPath,"AboutClioInfra.txt",sep="/"), file.info(paste(GenericPath,"AboutClioInfra.txt",sep="/"))$size)
XxZzYyCopyrightYearsXxZzYy <- "2010-2017"
# newly added:
XxZzYyFAXXxZzYy <- "fax + 31 20 6654181"
XxZzYyTelephoneXxZzYy <- "tel + 31 20 6685866"
XxZzYyEmailXxZzYy <- "e-mail: clioinfra@iisg.nl"

require(tcltk)
# The pages should be exported in a different folder for easy updating

# list of html pages that need substitution (all of them need, thus this is a full list of html pages for the website):
CountryPath <- '/home/michalis/PhD/Clio Infra/Website/CountryPagesWithMenus'
CountriesPages <- list.files(CountryPath)
CountriesPages <- paste(CountryPath,CountriesPages,sep="/")
IndicatorPath <- '/home/michalis/PhD/Clio Infra/Website/IndicatorPagesWithMenus'
IndicatorPages <- list.files(IndicatorPath)
IndicatorPages <- paste(IndicatorPath,IndicatorPages,sep="/")
# core pages at the generic path:
GenericPath <- "/home/michalis/PhD/Clio Infra/Website/"
CorePages <- c("News_Publications.html","index.html","Partners.html")
CorePages <- paste0(GenericPath,CorePages)

AllPages <- c(CountriesPages,IndicatorPages,CorePages)
SaveFilenames <- gsub('Website',"Website/ToUpload",AllPages)

# populating news items

XxZzYyNewsItem1XxZzYy <- "<a href=\"./News_Publications.html#item010\">How Was Life? Global Well-Being Since 1820</a>"
XxZzYyNewsItem1DateXxZzYy <- "Published on Thu, 12/04/2014 - 16:22"

XxZzYyNewsItem2XxZzYy <- "<a href=\"./News_Publications.html#item002\">Quality of Life Workshop at the OECD</a>"
XxZzYyNewsItem2DateXxZzYy <- "Published on Sun, 03/24/2013 - 14:25"

XxZzYyNewsItem3XxZzYy <- "<a href=\"./News_Publications.html#item001\">Workshop on Real Wages</a>"
XxZzYyNewsItem3DateXxZzYy <- "Published on Thu, 03/21/2013 - 15:07"

XxZzYyPublicationItem1XxZzYy <- "<a href=\"./News_Publications.html#item009\">How Was Life? Global Well-Being <br/>Since 1820</a>"
XxZzYyPublicationItem1DateXxZzYy <- "Published on Thu, 12/04/2014 - 16:20"

XxZzYyPublicationItem2XxZzYy <- "<a href=\"./News_Publications.html#item008\">Overview of datasets used in Global <br/>Well-Being Since 1820</a>"
XxZzYyPublicationItem2DateXxZzYy <- "Published on Thu, 12/04/2014 - 16:15"

XxZzYyPublicationItem3XxZzYy <- "<a href=\"./News_Publications.html#item009\">Working Papers Center for Global Economic History Online</a>"
XxZzYyPublicationItem3DateXxZzYy <- ""

# process with read, substitution and save:
total <- length(AllPages)

# create progress bar
pb <- tkProgressBar(title = "progress bar", min = 0,max = total, width = 300)

for (i in 1:total){
  setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),"% done"))
  
  test <- readChar(AllPages[i], file.info(AllPages[i])$size)
  
  test <- gsub("XxZzYyNewsItem1XxZzYy", XxZzYyNewsItem1XxZzYy, test)
  test <- gsub("XxZzYyNewsItem2XxZzYy", XxZzYyNewsItem2XxZzYy, test)
  test <- gsub("XxZzYyNewsItem3XxZzYy", XxZzYyNewsItem3XxZzYy, test)
  test <- gsub("XxZzYyNewsItem1DateXxZzYy", XxZzYyNewsItem1DateXxZzYy, test)
  test <- gsub("XxZzYyNewsItem2DateXxZzYy", XxZzYyNewsItem2DateXxZzYy, test)
  test <- gsub("XxZzYyNewsItem3DateXxZzYy", XxZzYyNewsItem3DateXxZzYy, test)
  test <- gsub("XxZzYyPublicationItem1XxZzYy", XxZzYyPublicationItem1XxZzYy, test)
  test <- gsub("XxZzYyPublicationItem2XxZzYy", XxZzYyPublicationItem2XxZzYy, test)
  test <- gsub("XxZzYyPublicationItem3XxZzYy", XxZzYyPublicationItem3XxZzYy, test)
  test <- gsub("XxZzYyPublicationItem1DateXxZzYy", XxZzYyPublicationItem1DateXxZzYy, test)
  test <- gsub("XxZzYyPublicationItem2DateXxZzYy", XxZzYyPublicationItem2DateXxZzYy, test)
  test <- gsub("XxZzYyPublicationItem3DateXxZzYy", XxZzYyPublicationItem3DateXxZzYy, test)
  
  
  test <- gsub("XxZzYyTotalDatasetsXxZzYy", XxZzYyTotalDatasetsXxZzYy, test)
  test <- gsub("XxZzYyTotalNumOfCountriesXxZzYy", XxZzYyTotalNumOfCountriesXxZzYy, test)
  test <- gsub("XxZzYyAboutClioInfraXxZzYy", XxZzYyAboutClioInfraXxZzYy, test)
  test <- gsub("XxZzYyCopyrightYearsXxZzYy", XxZzYyCopyrightYearsXxZzYy, test)
  test <- gsub("XxZzYyFAXXxZzYy", XxZzYyFAXXxZzYy, test)
  test <- gsub("XxZzYyTelephoneXxZzYy", XxZzYyTelephoneXxZzYy, test)
  test <- gsub("XxZzYyEmailXxZzYy", XxZzYyEmailXxZzYy, test)
  
  write(test, SaveFilenames[i])
}

close(pb)
