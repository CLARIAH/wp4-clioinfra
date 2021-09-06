# this is to be used to substitute the XYZ items on PartnersTemplate.html
require(xlsx)
ContributorsList <- read.xlsx("ContributorsList.xlsx", sheetIndex = 1)
ContributorsList <- ContributorsList[order(ContributorsList[,4]),]

GenericPath <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))

fileName <- 'PartnersTemplate.html'
test <- readChar(fileName, file.info(fileName)$size)

if (FALSE){
  # not to be executed since now the substitution takes place at the FooterSubstitution.R script
  XxZzYyTotalDatasetsXxZzYy <- nrow(ClioMetaData)
  XxZzYyTotalNumOfCountriesXxZzYy <- nrow(subset(GlobalMetadata,GlobalMetadata$DataPoints>0))
  XxZzYyAboutClioInfraXxZzYy <- readChar(paste(GenericPath,"/AboutClioInfra.txt",sep="/"), file.info(paste(GenericPath,"/AboutClioInfra.txt",sep="/"))$size)
  XxZzYyCopyrightYearsXxZzYy <- "2010-2017"
  
  test <- gsub("XxZzYyTotalDatasetsXxZzYy", XxZzYyTotalDatasetsXxZzYy, test)
  test <- gsub("XxZzYyTotalNumOfCountriesXxZzYy", XxZzYyTotalNumOfCountriesXxZzYy, test)
  test <- gsub("XxZzYyAboutClioInfraXxZzYy", XxZzYyAboutClioInfraXxZzYy, test)
  test <- gsub("XxZzYyCopyrightYearsXxZzYy", XxZzYyCopyrightYearsXxZzYy, test)
  
}

XxZzYyContactXxZzYy <- "Michail Moatsos, clioinfra [at ] iisg.nl, for mail address see footer."
test <- gsub("XxZzYyContactXxZzYy", XxZzYyContactXxZzYy, test)

TemplateLine222 <- '<a href="#" class="list-group-item active">XxZzYyContrNameXxZzYy</a>
<p class="list-group-item">XxZzYyContrAffiliationXxZzYy</p>
<p class="list-group-item">XxZzYyContrURLXxZzYy</p>'

TemplateLine <- '<div class="panel panel-XxZzYyPanelTypeXxZzYy">
            <div class="panel-heading">
              <h3 class="panel-title"><a href="XxZzYyContrURLXxZzYy" target="_blank">
      				<strong>XxZzYyContrNameXxZzYy</strong></a></h3>
            </div>
            <div class="panel-body">
              XxZzYyContrAffiliationXxZzYy
            </div>
          </div>'

NumOfContributors <- nrow(ContributorsList)
PanelTypes <- c("primary","info","default")
XxZzYyContributors1XxZzYy <- c()
for (i_1 in 1:floor(NumOfContributors/3)){
  tempaddition <- gsub("XxZzYyPanelTypeXxZzYy",PanelTypes[round(runif(1,1,length(PanelTypes)))],TemplateLine)
  tempaddition <- gsub("XxZzYyContrNameXxZzYy",ContributorsList$NameProper[i_1],tempaddition)
  tempaddition <- gsub("XxZzYyContrAffiliationXxZzYy",ContributorsList$CurrentAffiliation[i_1],tempaddition)
  tempaddition <- gsub("XxZzYyContrURLXxZzYy",ContributorsList$URL[i_1],tempaddition)
  XxZzYyContributors1XxZzYy <- c(XxZzYyContributors1XxZzYy, tempaddition)
  rm(tempaddition)
}

XxZzYyContributors2XxZzYy <- c()
for (i_2 in (i_1+1):(i_1+1+floor(NumOfContributors/3))){
  tempaddition <- gsub("XxZzYyPanelTypeXxZzYy",PanelTypes[round(runif(1,1,length(PanelTypes)))],TemplateLine)
  tempaddition <- gsub("XxZzYyContrNameXxZzYy",ContributorsList$NameProper[i_2],tempaddition)
  tempaddition <- gsub("XxZzYyContrAffiliationXxZzYy",ContributorsList$CurrentAffiliation[i_2],tempaddition)
  tempaddition <- gsub("XxZzYyContrURLXxZzYy",ContributorsList$URL[i_2],tempaddition)
  XxZzYyContributors2XxZzYy <- c(XxZzYyContributors2XxZzYy, tempaddition)
  rm(tempaddition)
}

XxZzYyContributors3XxZzYy <- c()
for (i_3 in (i_2+1):NumOfContributors){
  tempaddition <- gsub("XxZzYyPanelTypeXxZzYy",PanelTypes[round(runif(1,1,length(PanelTypes)))],TemplateLine)
  tempaddition <- gsub("XxZzYyContrNameXxZzYy",ContributorsList$NameProper[i_3],tempaddition)
  tempaddition <- gsub("XxZzYyContrAffiliationXxZzYy",ContributorsList$CurrentAffiliation[i_3],tempaddition)
  tempaddition <- gsub("XxZzYyContrURLXxZzYy",ContributorsList$URL[i_3],tempaddition)
  XxZzYyContributors3XxZzYy <- c(XxZzYyContributors3XxZzYy, tempaddition)
  rm(tempaddition)
}

XxZzYyContributors1XxZzYy <-paste0(XxZzYyContributors1XxZzYy,collapse = "")
XxZzYyContributors2XxZzYy <-paste0(XxZzYyContributors2XxZzYy,collapse = "")
XxZzYyContributors3XxZzYy <-paste0(XxZzYyContributors3XxZzYy,collapse = "")

test <- gsub("XxZzYyContributors1XxZzYy", XxZzYyContributors1XxZzYy, test)
test <- gsub("XxZzYyContributors2XxZzYy", XxZzYyContributors2XxZzYy, test)
test <- gsub("XxZzYyContributors3XxZzYy", XxZzYyContributors3XxZzYy, test)

# export the HTML page:
write(test, paste0(GenericPath,"/Partners.html"))
