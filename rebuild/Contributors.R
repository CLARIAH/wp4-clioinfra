library(xlsx)

setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))

IndicatorsList <- unique(ClioData$Indicator)

ContributorsList <- c()
for (i in 1:length(IndicatorsList)){
  ContributorsList <- c(ContributorsList,as.character(trimws(ClioMetaData$author[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])))
}

# remove duplicate entries:
ContributorsList <- ContributorsList[!duplicated(ContributorsList)]
ContributorsList <- ContributorsList[-c(12,20)]

# remove entries: 
x <- c("Marhsall, M.G., Gurr, T.R., Jaggers,K", "Marhsall, M.G., Gurr, T.R., Jaggers, K","Foldvari,P", "De Zwart, Pim, International Institute of Social History",
       "Kees Klein Goldewijk & Jonathan Fink-Jensen, Utrecht University")
idx = which(ContributorsList %in% x)
ContributorsList <- ContributorsList[-idx]

# remove from the elements: 
x <- c("Utrecht University", "\\(original data\\)", "\\(conversion to binary data\\)", ", International Institute of Social History, Amsterdam, The \n\tNetherlands",
       ", Amsterdam, The \tNetherlands", "; International Institute of Social History",
       ", Technical\nUniversity Munich \\(formerly University of Tuebingen\\)", "University of Tuebingen.", ", International Institute of Social History",
       "Version 1: ", "Version 2: ", " \\(University College London\\)", " \\(Texas Tech University\\)", " \\(North Dakota State University\\)",
       "Peter foldvari, ")
for (i in 1:length(x)){
  for (j in 1:length(ContributorsList)){
    ContributorsList[j] <- gsub(x[i],"",ContributorsList[j])
  }
}

ContributorsList[ContributorsList=="Baten, Joerg, ; Blum, Mathias"] <- "Mathias Blum"
ContributorsList <- gsub("\n-",",",ContributorsList)
ContributorsList <- trimws(ContributorsList)
# remove trailing commas:
# http://stackoverflow.com/questions/23274035/removing-multiple-commas-and-trailing-commas-using-gsub
ContributorsList <- gsub("^,*|(?<=,),|,*$", "", ContributorsList, perl=T)

# finally I need to split among "&", ",", "and "
# prepare for splitting
ContributorsList <- gsub(", and",",",ContributorsList)
ContributorsList <- gsub(" and",",",ContributorsList)
ContributorsList <- gsub(" &",",",ContributorsList)

FinalContributorsList <- c()
for (i in 1:length(ContributorsList)){
  FinalContributorsList <- c(FinalContributorsList,strsplit(ContributorsList[i],",")[[1]])
}

FinalContributorsList <- trimws(FinalContributorsList)
FinalContributorsList <- FinalContributorsList[!duplicated(FinalContributorsList)]

ContributorsList <- as.data.frame(FinalContributorsList, stringsAsFactors = F)
names(ContributorsList) <- "Name"
ContributorsList$CurrentAffiliation <- as.character(NA)
ContributorsList$URL <- as.character(NA)
ContributorsList$NameProper <- as.character(NA)

ContributorsList$URL[which(ContributorsList$Name=="Bas van Leeuwen")] <- "http://www.basvanleeuwen.net/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Bas van Leeuwen")] <- "International Institute of Social History, Amsterdam"
ContributorsList$NameProper[which(ContributorsList$Name=="Bas van Leeuwen")] <- "Leeuwen, Bas van"
ContributorsList$URL[which(ContributorsList$Name=="Jieli van Leeuwen-Li")] <- "https://socialhistory.org/en/staff/jieli-li"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Jieli van Leeuwen-Li")] <- "International Institute of Social History, Amsterdam"
ContributorsList$NameProper[which(ContributorsList$Name=="Jieli van Leeuwen-Li")] <- "Leeuwen-Li, Jieli van"
ContributorsList$URL[which(ContributorsList$Name=="Peter Foldvari")] <- "http://www.peterfoldvari.com/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Peter Foldvari")] <- "International Institute of Social History, Amsterdam"
ContributorsList$NameProper[which(ContributorsList$Name=="Peter Foldvari")] <- "Foldvari, Peter"
ContributorsList$URL[which(ContributorsList$Name=="Jonathan Fink-Jensen")] <- "https://www.uu.nl/staff/JFinkJensen"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Jonathan Fink-Jensen")] <- "Utrecht University, Department of History and Art History"
ContributorsList$NameProper[which(ContributorsList$Name=="Jonathan Fink-Jensen")] <- "Fink-Jensen, Jonathan"

ContributorsList$URL[which(ContributorsList$Name=="Kees Klein Goldewijk")] <- "https://www.uu.nl/staff/CGMKleinGoldewijk"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Kees Klein Goldewijk")] <- "Utrecht University, Copernicus Institute of Sustainable Development"
ContributorsList$NameProper[which(ContributorsList$Name=="Kees Klein Goldewijk")] <- "Goldewijk, Kees Klein"
ContributorsList$URL[which(ContributorsList$Name=="Tatu Vanhanen")] <- "https://en.wikipedia.org/wiki/Tatu_Vanhanen"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Tatu Vanhanen")] <- "University of Tampere"
ContributorsList$NameProper[which(ContributorsList$Name=="Tatu Vanhanen")] <- "Vanhanen, Tatu"
ContributorsList$URL[which(ContributorsList$Name=="Peter Brecke")] <- "http://www.iac.gatech.edu/people/faculty/brecke"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Peter Brecke")] <- "Georgia Tech, The Sam Nunn School of International Affairs"
ContributorsList$NameProper[which(ContributorsList$Name=="Peter Brecke")] <- "Brecke, Peter"
ContributorsList$URL[which(ContributorsList$Name=="Reinoud Bosch")] <- "http://www.reinoudbosch.nl/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Reinoud Bosch")] <- "Editor-in-Chief, KWALON"
ContributorsList$NameProper[which(ContributorsList$Name=="Reinoud Bosch")] <- "Bosch, Reinoud"

ContributorsList$URL[which(ContributorsList$Name=="Koen van den Bos")] <- "-"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Koen van den Bos")] <- "International Institute of Social History, Amsterdam"
ContributorsList$NameProper[which(ContributorsList$Name=="Koen van den Bos")] <- "Bos, Koen van den"
ContributorsList$URL[which(ContributorsList$Name=="Filipa Ribeiro da Silva")] <- "https://socialhistory.org/en/staff/filipa-ribeiro-da-silva"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Filipa Ribeiro da Silva")] <- "International Institute of Social History, Amsterdam"
ContributorsList$NameProper[which(ContributorsList$Name=="Filipa Ribeiro da Silva")] <- "Silva, Filipa Ribeiro da"
ContributorsList$URL[which(ContributorsList$Name=="Jutta Bolt")] <- "http://www.rug.nl/staff/j.bolt/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Jutta Bolt")] <- "University of Groningen, Faculty of Economics and Business"
ContributorsList$NameProper[which(ContributorsList$Name=="Jutta Bolt")] <- "Bolt, Jutta"
ContributorsList$URL[which(ContributorsList$Name=="Jan Luiten van Zanden")] <- "https://www.uu.nl/staff/JLvanZanden"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Jan Luiten van Zanden")] <- "Utrecht University, Department of History and Art History"
ContributorsList$NameProper[which(ContributorsList$Name=="Jan Luiten van Zanden")] <- "Zanden, Jan Luiten van"
ContributorsList$URL[which(ContributorsList$Name=="Sarah Carmichael")] <- "https://www.uu.nl/medewerkers/SGCarmichael"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Sarah Carmichael")] <- "Utrecht University, Department of History and Art History"
ContributorsList$NameProper[which(ContributorsList$Name=="Sarah Carmichael")] <- "Carmichael, Sarah"
ContributorsList$URL[which(ContributorsList$Name=="Selin Dilli")] <- "https://www.uu.nl/staff/SDDilli"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Selin Dilli")] <- "Utrecht University, Department of History and Art History"
ContributorsList$NameProper[which(ContributorsList$Name=="Selin Dilli")] <- "Dilli, Selin"
ContributorsList$URL[which(ContributorsList$Name=="Auke Rijpma")] <- "https://www.uu.nl/staff/ARijpma"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Auke Rijpma")] <- "Utrecht University, Department of History and Art History"
ContributorsList$NameProper[which(ContributorsList$Name=="Auke Rijpma")] <- "Rijpma, Auke"

ContributorsList$URL[which(ContributorsList$Name=="Carmen M. Reinhart")] <- "http://www.carmenreinhart.com/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Carmen M. Reinhart")] <- "Harvard University, John F. Kennedy School of Government"
ContributorsList$NameProper[which(ContributorsList$Name=="Carmen M. Reinhart")] <- "Reinhart, Carmen M."
ContributorsList$URL[which(ContributorsList$Name=="Kenneth S. Rogoff")] <- "http://www.kennethrogoff.com/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Kenneth S. Rogoff")] <- "Harvard University, Faculty of Arts and Sciences"
ContributorsList$NameProper[which(ContributorsList$Name=="Kenneth S. Rogoff")] <- "Rogoff, Kenneth S."
ContributorsList$URL[which(ContributorsList$Name=="Mathias Blum")] <- "http://pure.qub.ac.uk/portal/en/persons/matthias-blum(68d46afc-d31e-46cd-a283-3414650b20f0).html"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Mathias Blum")] <- "Queen's University Management School"
ContributorsList$NameProper[which(ContributorsList$Name=="Mathias Blum")] <- "Blum, Mathias"
ContributorsList$URL[which(ContributorsList$Name=="Michalis Moatsos")] <- "https://www.uu.nl/staff/MMoatsos"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Michalis Moatsos")] <- "Utrecht University, Department of History and Art History"
ContributorsList$NameProper[which(ContributorsList$Name=="Michalis Moatsos")] <- "Moatsos, Michail"
ContributorsList$URL[which(ContributorsList$Name=="Joerg Baten")] <- "http://www.uni-tuebingen.de/fakultaeten/wirtschafts-und-sozialwissenschaftliche-fakultaet/faecher/wirtschaftswissenschaft/lehrstuehle/volkswirtschaftslehre/wirtschaftsgeschichte/team/prof-dr-joerg-baten.html"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Joerg Baten")] <- "University of Tübingen, Faculty of Economics and Social Sciences"
ContributorsList$NameProper[which(ContributorsList$Name=="Joerg Baten")] <- "Baten, Joerg"

ContributorsList$URL[which(ContributorsList$Name=="Jon Verriet")] <- "http://www.ru.nl/geschiedenis/wie_wat_waar/medewerkers/medewerkers/verriet/virtual-folder/english/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Jon Verriet")] <- "Radboud University, Faculty of Arts"
ContributorsList$NameProper[which(ContributorsList$Name=="Jon Verriet")] <- "Verriet, Jon"
ContributorsList$URL[which(ContributorsList$Name=="Sjaak van der Velden")] <- "http://www.sjaakvandervelden.info/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Sjaak van der Velden")] <- "International Institute of Social History, Amsterdam"
ContributorsList$NameProper[which(ContributorsList$Name=="Sjaak van der Velden")] <- "Velden, Sjaak van der"
ContributorsList$URL[which(ContributorsList$Name=="Monty G. Marshall")] <- "http://www.systemicpeace.org/people.html"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Monty G. Marshall")] <- "Center for Systemic Peace (CSP)"
ContributorsList$NameProper[which(ContributorsList$Name=="Monty G. Marshall")] <- "Marshall, Monty G."

ContributorsList$URL[which(ContributorsList$Name=="Keith Jaggers")] <- "http://www.honors.colostate.edu/honors-faculty"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Keith Jaggers")] <- "University of Colorado at Boulder, Honors Program"
ContributorsList$NameProper[which(ContributorsList$Name=="Keith Jaggers")] <- "Jaggers, Keith"
ContributorsList$URL[which(ContributorsList$Name=="Ted Robert Gurr")] <- "https://gvpt.umd.edu/facultyprofile/Gurr/Ted%20Robert"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Ted Robert Gurr")] <- "University of Maryland, Department of Government & Politics"
ContributorsList$NameProper[which(ContributorsList$Name=="Ted Robert Gurr")] <- "Gurr, Ted Robert"
ContributorsList$URL[which(ContributorsList$Name=="Richard Zijdeman")] <- "https://socialhistory.org/en/staff/richard-zijdeman"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Richard Zijdeman")] <- "International Institute of Social History, Amsterdam"
ContributorsList$NameProper[which(ContributorsList$Name=="Richard Zijdeman")] <- "Zijdeman, Richard"

ContributorsList$URL[which(ContributorsList$Name=="James Melton")] <- "http://www.ucl.ac.uk/constitution-unit/people/james-melton"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="James Melton")] <- "University College London"
ContributorsList$NameProper[which(ContributorsList$Name=="James Melton")] <- "Melton, James"
ContributorsList$URL[which(ContributorsList$Name=="Stephen Meserve")] <- "https://www.depts.ttu.edu/politicalscience/Faculty/Meserve_Stephen.php"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Stephen Meserve")] <- "Texas Tech University, Department of Political Science"
ContributorsList$NameProper[which(ContributorsList$Name=="Stephen Meserve")] <- "Meserve, Stephen"
ContributorsList$URL[which(ContributorsList$Name=="Daniel Pemstein")] <- "http://www.danpemstein.com/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Daniel Pemstein")] <- "North Dakota State University, Political Science Faculty"
ContributorsList$NameProper[which(ContributorsList$Name=="Daniel Pemstein")] <- "Pemstein, Daniel"

ContributorsList$URL[which(ContributorsList$Name=="Klaus Armingeon")] <- "http://www.ipw.unibe.ch/about_us/people/prof_dr_armingeon_klaus/index_eng.html"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Klaus Armingeon")] <- "University of Bern, Institute of Political Science"
ContributorsList$NameProper[which(ContributorsList$Name=="Klaus Armingeon")] <- "Armingeon, Klaus"
ContributorsList$URL[which(ContributorsList$Name=="David Weisstanner")] <- "http://www.ipw.unibe.ch/about_us/people/weisstanner_david/index_eng.html"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="David Weisstanner")] <- "University of Bern, Institute of Political Science"
ContributorsList$NameProper[which(ContributorsList$Name=="David Weisstanner")] <- "Weisstanner, David"
ContributorsList$URL[which(ContributorsList$Name=="Sarah Engler")] <- "http://www.ipw.unibe.ch/about_us/people/engler_sarah/index_eng.html"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Sarah Engler")] <- "University of Bern, Institute of Political Science"
ContributorsList$NameProper[which(ContributorsList$Name=="Sarah Engler")] <- "Engler, Sarah"
ContributorsList$URL[which(ContributorsList$Name=="Panajotis Potolidis")] <- "-"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Panajotis Potolidis")] <- "University of Bern, Institute of Political Science"
ContributorsList$NameProper[which(ContributorsList$Name=="Panajotis Potolidis")] <- "Potolidis, Panajotis"
ContributorsList$URL[which(ContributorsList$Name=="Marlène Gerber")] <- "http://www.ipw.unibe.ch/about_us/people/dr_gerber_marlne/index_eng.html"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Marlène Gerber")] <- "University of Bern, Institute of Political Science"
ContributorsList$NameProper[which(ContributorsList$Name=="Marlène Gerber")] <- "Gerber, Marlène"
ContributorsList$URL[which(ContributorsList$Name=="Pim de Zwart")] <- "https://www.wur.nl/en/Persons/Pim-P-Pim-de-Zwart.htm"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Pim de Zwart")] <- "Wageningen University, Department of Social Sciences"
ContributorsList$NameProper[which(ContributorsList$Name=="Pim de Zwart")] <- "Zwart, Pim de"

ContributorsList$URL[which(ContributorsList$Name=="Guido Alfani")] <- "https://faculty.unibocconi.eu/guidoalfani/"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Guido Alfani")] <- "Bocconi University, Dondena Centre and Stone Center on Socio-Economic Inequality"
ContributorsList$NameProper[which(ContributorsList$Name=="Guido Alfani")] <- "Alfani, Guido"
ContributorsList$URL[which(ContributorsList$Name=="Sonia Schifano")] <- "https://wwwen.uni.lu/research/fhse/dbcs/people/sonia_schifano"
ContributorsList$CurrentAffiliation[which(ContributorsList$Name=="Sonia Schifano")] <- "University of Luxembourg"
ContributorsList$NameProper[which(ContributorsList$Name=="Sonia Schifano")] <- "Schifano, Sonia"

ContributorsList <- subset(ContributorsList,!is.na(ContributorsList$NameProper))
# only export if you really have to, be careful you may loose manual entries on the file: write.xlsx(ContributorsList,"ContributorsList.xlsx", row.names = F)
