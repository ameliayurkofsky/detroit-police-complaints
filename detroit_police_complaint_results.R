
library(readr)
library(dplyr)


#Import data set from the City of Detroit's Open Data Portal 
#link: https://data.detroitmi.gov/datasets/39076d87eb6d49ce8424873e3acd0d4b/explore 
#Downloaded 5/27/2021
complaints <- read_csv("DPD_Citizen_Complaints.csv")
glimpse(complaints)

