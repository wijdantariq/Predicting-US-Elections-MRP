#### Preamble ####
# Purpose: Prepare and clean the post-stratification ACS data downloaded from IPUMS
# Author: Annie Collins, Jennifer Do, Andrea Javellana, and Wijdan Tariq
# Data: 2 November 2020
# Contact: annie.collins@mail.utoronto.com, jenni.do@mail.utoronto.com, 
# andrea.javellana@mail.utoronto.com, wijdan.tariq@mail.utoronto.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2018 ACS data set from IPUMS 
#  and save the folder that you're interested in to inputs/data 




#### Workspace setup ####
library(haven)
library(tidyverse)
library(labelled)

# Read in the raw data. 
raw_data <- read_dta("inputs/data/usa_00002.dta")
names(raw_data)

table(raw_data$citizen)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
#names(raw_data)
reduced_data <- 
  raw_data %>% 
  select(
    region,# 
    stateicp,# 
    sex,  #
    age, #
    race, #
    hispan,#
    bpl,#
    educd,#
    empstat, #
    ftotinc) #


# Only keeping respondents over 17 years of age
ACS <- reduced_data
ACS <- subset(ACS, age > 17) #keep responses of age over 17  
ACS <- subset(ACS, age < 94) # keep responses of age under 94


# BIRTHPLACE

#categorizing whether born in USA (0-56) or not
ACS$bpl = cut(ACS$bpl,c(0,56,999))
levels(ACS$bpl) = c('USA', 'another country')


# GENDER
ACS$sex <- cut(ACS$sex, c(0,1,2))
levels(ACS$sex) <- c("male", "female")


# EDUCATION

ACS$educd = cut(ACS$educd,c(0,26,61,64,71,101,116), labels=c(1:6))
ACS$educd <- as.numeric(ACS$educd)
#levels(ACS$educd) = c('less than high school', 'some high school',
#                      'completed high school', 'some post-secondary',
#                      'post-secondary degree', 'post-graduate degree'
#)



# AGE
# Assign people with age 90 the numeric value 90 instead of "90 (90+ born in...)"
# then create bins for ages
table(ACS$age)
ACS$age <- as.numeric(as.character(ACS$age))
ACS$age[is.na(ACS$age)] <- 90
ACS$age = cut(ACS$age,c(17, 29, 44, 59, 74, 93))
levels(ACS$age) = c('18 to 29', '30 to 44',
                     '45 to 59', '60 to 74',
                     '74 and above')

# forsome reason 90 y/o are the only ones labelled as '90(90+ born in ....)'
table(ACS$age)
nrow(ACS) == nrow(ACS) # TRUE :), didnt lose anyone


# EMPLOYMENT
# Assign the appropriate labels to employment levels and save as character vector
ACS$empstat <- labelled::to_factor(ACS$empstat)
ACS$empstat <- as.character(ACS$empstat)


# INCOME

ACS$ftotinc = cut(ACS$ftotinc,c(-16400,14999,19999,24999,29999,34999,
                                39999,44999, 49999,54999,59999,64999,
                                69999,74999,79999,84999,89999,94999,
                                99999,124999,149999,174999, 199999,
                                249999, 9999998, 9999999), labels=c(1:25))
ACS$ftotinc <- as.numeric(ACS$ftotinc)
ACS$ftotinc[ACS$ftotinc == 25] <- NA

levels(ACS$ftotinc) = c('Less than $14,999', '$15,000 to $19,999', '$20,000 to $24,999',
                       '$25,000 to $29,999', '$30,000 to $34,999', '35,000 to $39,999',
                       ' $40,000 to $44,999','$45,000 to $49,999','$50,000 to $54,999',
                       '$55,000 to $59,999', '$60,000 to $64,999', '$65,000 to $69,999',
                       '$70,000 to $74,999',   '$75,000 to $79,999',   '$80,000 to $84,999',
                       '$85,000 to $89,999', '$90,000 to $94,999', '$95,000 to $99,999',
                      '$100,000 to $124,999', '$125,000 to $149,999', '$150,000 to $174,999',
                       '$175,000 to $199,999', '$200,000 to $249,999',   '$250,000 and above',
                       NA
                       )
table(ACS$ftotinc)


# RACE

# Add race group labels to race and hispan
ACS$race <- labelled::to_factor(ACS$race)
ACS$hispan <- labelled::to_factor(ACS$hispan)

# Simplifying/grouping ACS races
ACS$race <- gsub("american indian or alaska native", "native american", ACS$race)
ACS$race <- gsub("japanese", "other asian/pacific islander", ACS$race)
ACS$race <- gsub("other asian or pacific islander", "other asian/pacific islander", ACS$race)
ACS$race <- gsub("other race, nec", "other", ACS$race)
ACS$race <- gsub("two major races", "other", ACS$race)
ACS$race <- gsub("three or more major races", "other", ACS$race)
ACS$race <- gsub("black/african american/negro", "black", ACS$race)
table(ACS$race)

#Making HISPAN a binary variable (hispanic vs not hispanic)
ACS$hispan <- gsub("mexican", "hispanic", ACS$hispan)
ACS$hispan <- gsub("puerto rican", "hispanic", ACS$hispan)
ACS$hispan <- gsub("cuban", "hispanic", ACS$hispan)
ACS$hispan <- gsub("other", "hispanic", ACS$hispan)
table(ACS$hispan)

#RACE including hispanics as a race
ACS$race <- ACS$race
ACS$race[ACS$hispan == 'hispanic'] <- "hispanic"
ACS$race <- as.character(ACS$race_ethnicity)
table(ACS$race)

# Discard HISPAN column
ACS <- 
  ACS %>% 
  select(
    region, 
    stateicp,
    sex,
    age,
    race, 
    bpl,
    educd,
    empstat, 
    ftotinc)


# STATE
ACS$stateicp <- labelled::to_factor(ACS$stateicp)
# Switch to character vector and then back to factor to remove unused levels 
# and sort new levels in alphabetical order
ACS$stateicp <- as.character(ACS$stateicp)
ACS$stateicp <- as.factor(ACS$stateicp)
# Assign levels a number from 1 to 51 corresponding to state and make a numeric variable
levels(ACS$stateicp) <- c(1:51)
ACS$stateicp <- as.numeric(ACS$stateicp)


# make variable names match the corresponding variables in the survey data
colnames(ACS) <- c("census_region", "state", "gender", "age", "race_ethnicity", 
                   "foreign_born", "education", "employment", 
                   "household_income")

summary(ACS)

# create clean output file
write_csv(ACS, "outputs/data/ACS.csv")  

###########################################################
         