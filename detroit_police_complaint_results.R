
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)

#Import data set from the City of Detroit's Open Data Portal 
#link: https://data.detroitmi.gov/datasets/39076d87eb6d49ce8424873e3acd0d4b/explore 
#Downloaded 5/27/2021
complaints <- read_csv("DPD_Citizen_Complaints.csv")
glimpse(complaints)

#Field Descriptions
#BPC or Board of Police Commissioners number: a unique identifier issued by the BPC to each case in numerical order after the case has been assigned to an Investigator, used for internal tracking purposes.
#CCR or Citizen Complaint Report number: the unique identifier automatically assigned to the case via the data management system.
#Report Date: date the complaint was filed.
#Entry: mode of entry for the complaint by the citizen.
#Age: age of the citizen filing the complaint.
#ctznRace: race of the citizen filing the complaint.
#ctznSex: sex of the citizen filing the complaint.
#Closed: date the investigation was completed by OCI.
#Unit: commanding unit of the officer against which the complaint was filed.
#Administrative Closure: administrative finding.
#Allegation: a claim as set forth by the citizen complaint.
#Finding: the disposition of a citizen complaint after investigation.
#ofcrRace: race of the officer against which the complaint was filed.
#ofcrSex: sex of the officer against which the complaint was filed.

#Allegation Type Descriptions
#Arrest: Complaint that the restraint of a person’s liberty was improper or unjustified.
#Demeanor: Complaint regarding a DPD member’s bearing, gesture, language, or other action, which is offensive or of doubtful social propriety, or gives the appearance of conflict of interest, misuse of influence, or lack of jurisdiction or authority.
#Entry: Complaint that entry into a building or onto property was improper and/or that excessive force was used against property to gain entry.
#Force: Complaint regarding the use or threatened use of force against a person.
#Harassment: Complaint that the method of police action was predicated upon factors irrelevant under the circumstances to good law enforcement decision-making (e.g., race, attire, sex, age) or complaint of improperly selective law enforcement on the basis of factors just listed.
#Procedure: Complaint regarding other actions in violation of DPD rules, regulations, procedures or policies, or the Law Enforcement Code of Ethics.
#Property: Complaint regarding property lost or damaged while in police custody or confiscated through police action.
#Search: Complaint that the search of a person or his/her property was improper, or in violation of established police procedure, or unjustified.
#Service: Complaint regarding the lack, tardiness or inadequacy of police service.

#Finding Type Descriptions
#Unfounded: The investigation revealed no facts to support that the incident complained of actually occurred.
#Sustained: A preponderance of the evidence shows that the alleged conduct did occur and the actions of the officer violated DPD policies, procedures, or training.
#Not Sustained: There are insufficient facts to decide whether the alleged misconduct occurred.
#Exonerated: A preponderance of the evidence shows that the alleged conduct did occur but did not violate DPD policies, procedures or training.

#Pre-processing
#dropping clear mis-keys, i.e., age > 150
complaints <- subset(complaints, complaints$Age <150)

complaints$Allegation[complaints$Allegation=="PROcedure"|complaints$Allegation=="procedure"] <- "Procedure"
complaints$Allegation[complaints$Allegation=="arrest"] <- "Arrest"
complaints$Allegation[complaints$Allegation=="force"] <- "Force"
complaints$Allegation[complaints$Allegation=="search"] <- "Search"

complaints$Entry[complaints$Entry=="Telephone(or TDD)"|complaints$Entry=="Telephone"] <- "Telephone (or TDD)"
complaints$Entry[complaints$Entry=="Outside agency"|complaints$Entry=="Outside agendy"] <- "Outside Agency"

complaints$ctznRace[complaints$ctznRace=="Bi-racial"] <- "Biracial"
complaints$ctznRace[complaints$ctznRace=="NA"|complaints$ctznRace=="Select"] <- "Unknown"

complaints$ctznSex[is.na(complaints$ctznSex)] <- "Unknown"

complaints$ofcrSex[is.na(complaints$ofcrSex)|complaints$ofcrSex=="Unknown"|complaints$ofcrSex=="UNKNOWN"] <- "UNKNOWN"
complaints$ofcrSex[complaints$ofcrSex=="F"|complaints$ofcrSex=="Female"] <- "FEMALE"
complaints$ofcrSex[complaints$ofcrSex=="M"|complaints$ofcrSex=="Male"] <- "MALE"

complaints$ofcrRace[complaints$ofcrRace=="A"|complaints$ofcrRace=="Asian"] <- "ASIAN"
complaints$ofcrRace[complaints$ofcrRace=="B"|complaints$ofcrRace=="Black"] <- "BLACK"
complaints$ofcrRace[complaints$ofcrRace=="H"|complaints$ofcrRace=="Hispanic"] <- "HISPANIC"
complaints$ofcrRace[complaints$ofcrRace=="Unknown"|is.na(complaints$ofcrRace)] <- "UNKNOWN"
complaints$ofcrRace[complaints$ofcrRace=="W"|complaints$ofcrRace=="White"] <- "WHITE"

complaints$d_UNIT <- ifelse(grepl("2nd", complaints$Unit, ignore.case = TRUE)==TRUE, "2ND", complaints$Unit)
complaints$d_UNIT <- ifelse(grepl("3rd", complaints$Unit, ignore.case = TRUE)==TRUE, "3RD", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("4th", complaints$Unit, ignore.case = TRUE)==TRUE, "4TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("5th", complaints$Unit, ignore.case = TRUE)==TRUE, "5TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("6th", complaints$Unit, ignore.case = TRUE)==TRUE, "6TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("7th", complaints$Unit, ignore.case = TRUE)==TRUE, "7TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("8th", complaints$Unit, ignore.case = TRUE)==TRUE, "8TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("9th", complaints$Unit, ignore.case = TRUE)==TRUE, "9TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("10th", complaints$Unit, ignore.case = TRUE)==TRUE, "10TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("11th", complaints$Unit, ignore.case = TRUE)==TRUE, "11TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("12th", complaints$Unit, ignore.case = TRUE)==TRUE, "12TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("unknown|Detroit police department", complaints$Unit, ignore.case = TRUE)==TRUE, "UNKNOWN", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("major", complaints$Unit, ignore.case = TRUE)==TRUE, "MAJOR", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("gang", complaints$Unit, ignore.case = TRUE)==TRUE, "GANG", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("9th", complaints$Unit, ignore.case = TRUE)==TRUE, "9TH", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("DOWNTOWN", complaints$Unit, ignore.case = TRUE)==TRUE, "DOWNTOWN", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("Traffic", complaints$Unit, ignore.case = TRUE)==TRUE, "TRAFFIC", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("homicide", complaints$Unit, ignore.case = TRUE)==TRUE, "HOMICIDE", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("telephone crime", complaints$Unit, ignore.case = TRUE)==TRUE, "TELEPHONE CRIME", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("gaming", complaints$Unit, ignore.case = TRUE)==TRUE, "GAMING", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("k-9|canine", complaints$Unit, ignore.case = TRUE)==TRUE, "CANINE", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("special victims", complaints$Unit, ignore.case = TRUE)==TRUE, "SVU", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("vehicle task", complaints$Unit, ignore.case = TRUE)==TRUE, "ABANDON VEHICLE", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("chief", complaints$Unit, ignore.case = TRUE)==TRUE, "CHIEF", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("domestic", complaints$Unit, ignore.case = TRUE)==TRUE, "DOMESTIC VIOLENCE", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("FIREARMS ", complaints$Unit, ignore.case = TRUE)==TRUE, "FIREARMS", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("Headquarters", complaints$Unit, ignore.case = TRUE)==TRUE, "HEADQUARTERS", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("Internal affairs", complaints$Unit, ignore.case = TRUE)==TRUE, "INTERNAL AFFAIRS", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("Sex Crime", complaints$Unit, ignore.case = TRUE)==TRUE, "SEX CRIMES", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("tactical", complaints$Unit, ignore.case = TRUE)==TRUE, "TACTICAL", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("CRIMINAL INVESTIGATIONS", complaints$Unit, ignore.case = TRUE)==TRUE, "CRIMINAL INVESTIGATIONS", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("EXEC", complaints$Unit, ignore.case = TRUE)==TRUE, "EXECUTIVE PROTECTION", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("FUGITIVE", complaints$Unit, ignore.case = TRUE)==TRUE, "FUGITIVE", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("narcotic", complaints$Unit, ignore.case = TRUE)==TRUE, "NARCOTICS", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("JR.|JUNIOR", complaints$Unit, ignore.case = TRUE)==TRUE, "JUNIOR POLICE CADETS", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("records", complaints$Unit, ignore.case = TRUE)==TRUE, "RECORDS AND IDENTIFICATION", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("SPECIAL RESPONSE", complaints$Unit, ignore.case = TRUE)==TRUE, "SPECIAL RESPONSE TEAM", complaints$d_UNIT)
complaints$d_UNIT <- ifelse(grepl("INVESTIGATIVE OPERATIONS", complaints$Unit, ignore.case = TRUE)==TRUE, "INVESTIGATIVE OPERATIONS", complaints$d_UNIT)

#Exploratory Data Analysis
tab1 <- complaints %>% count(Finding) %>% arrange(desc(n))
kable(tab1, caption = "Finding Frequency")

tab2 <- complaints %>% count(Allegation) %>% arrange(desc(n))
kable(tab2, caption = "Allegation Frequency")

tab3 <- complaints %>% count(Entry) %>% arrange(desc(n))
kable(tab3, caption = "Entry Frequency")

tab4 <- complaints %>% count(ctznRace) %>% arrange(desc(n))
kable(tab4, caption = "Complaintant Race Frequency")

tab5 <- complaints %>% count(ctznSex) %>% arrange(desc(n))
kable(tab5, caption = "Complaintant Sex Frequency")

tab6 <- complaints %>% count(Unit) %>% arrange(desc(n))
kable(tab6, caption = "Unit Frequency")

tab7 <- complaints %>% count(ofcrSex) %>% arrange(desc(n))
kable(tab7, caption = "Officer Sex Frequency")

tab7 <- complaints %>% count(ofcrRace) %>% arrange(desc(n))
kable(tab7, caption = "Officer Race Frequency")



#Exploratory Data Analysis
ggplot(data = complaints, aes(x=as.factor(Finding), y=Age, color=as.factor(Finding))) +
  geom_boxplot() +
  xlab('Finding') +
  ylab('Age') +
  ggtitle('Finding vs Age')  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")




