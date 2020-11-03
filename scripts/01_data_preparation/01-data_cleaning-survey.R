#### Preamble ####
# Purpose: Prepare and clean the survey data (nationscape) downloaded from voterstudygroup.org
# Author: Annie Collins, Jennifer Do, Andrea Javellana, and Wijdan Tariq
# Data: 2 November 2020
# Contact: annie.collins@mail.utoronto.com, jenni.do@mail.utoronto.com, 
# andrea.javellana@mail.utoronto.com, wijdan.tariq@mail.utoronto.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the nationscape data set  from voterstudygroup.org 
#  and save the folder that you're interested in to inputs/data 



#### Workspace setup ####
library(haven)
library(tidyverse)
library(labelled)
# Read in the raw data. 
raw_UCLA <- read_dta("inputs/data/ns20200625.dta")


# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_UCLA)

reduced_UCLA <- 
  raw_UCLA %>% 
  select(vote_2020, #
         employment, #
         foreign_born,#
         gender,#
         census_region, # 
         hispanic,#
         race_ethnicity, #
         household_income,#
         education, #
         state, #
         age #
         )

UCLA <- reduced_UCLA

#deleting responses if not Trump(1)/Biden(2)
UCLA <- subset(UCLA, vote_2020 < 3 ) 
# Assign a vote for Joe Biden a value of 0
UCLA$vote_2020[UCLA$vote_2020 == 2] <- 0
UCLA$vote_2020 <- as.numeric(UCLA$vote_2020)
state.abb#deleting responses who picked "other" as employment
UCLA <- subset(UCLA, employment <= 8 )


# EDUCATION

UCLA$education = cut(UCLA$education,c(0,2,3,4,6,9,11), labels=c(1:6))

# levels(UCLA$education) = c('less than high school', 'some high school',
#                      'completed high school', 'some post-secondary',
#                      'post-secondary degree', 'post-graduate degree'
# )

UCLA$education <- as.numeric(UCLA$education)


# GENDER
UCLA$gender = cut(UCLA$gender,c(0,1,2))
levels(UCLA$gender) = c('female', 'male')
table(UCLA$gender)


# AGE
# put age into bins
UCLA$age = cut(UCLA$age,c(17, 29, 44, 59, 74, 93))
levels(UCLA$age) = c('18 to 29', '30 to 44',
                            '45 to 59', '60 to 74',
                            '74 and above')


# BIRTHPLACE

UCLA$foreign_born = cut(UCLA$foreign_born,c(0,1,2))
levels(UCLA$foreign_born) = c('USA', 'another country')
table(UCLA$foreign_born)


# RACE

#hispanic (make binary)
UCLA$hispanic = cut(UCLA$hispanic,c(0,1,15))
levels(UCLA$hispanic) = c('not hispanic', 'hispanic')
table(UCLA$hispanic)

#Simplifying/grouping UCLA races
UCLA$race_ethnicity = cut(UCLA$race_ethnicity,c(0,1,2,3,4,5,14,15))
levels(UCLA$race_ethnicity) = c('white', 'black',
                           'native american', 'other asian/pacific islander',
                           'chinese', 'other asian/pacific islander 1', 'other'
)
UCLA$race_ethnicity <- gsub('other asian/pacific islander 1', 'other asian/pacific islander', UCLA$race_ethnicity)
table(UCLA$race_ethnicity)

#RACE including hispanics as a race
UCLA$race_ethnicity <- UCLA$race_ethnicity
UCLA$race_ethnicity[UCLA$hispanic == 'hispanic'] <- "hispanic"
UCLA$race_ethnicity <- as.character(UCLA$race_ethnicity)

#discard hispanic column
UCLA <- 
  UCLA %>% 
  select(vote_2020, #
         employment, #
         foreign_born,#
         gender,#
         census_region, # UNFINISHED
         race_ethnicity, #
         household_income,
         education, #
         state, 
         age #
  )
table(UCLA$race_ethnicity)


# EMPLOYMENT
UCLA$employment = cut(UCLA$employment,c(0,1,3,4,5,7,8))
levels(UCLA$employment) = c('employed', 'not in labor force',
                            'unemployed', 'employed1',
                            'not in labor force1', 'employed2')
table(UCLA$employment)
UCLA$employment <- gsub('employed1', 'employed', UCLA$employment)
UCLA$employment <- gsub('employed2', 'employed', UCLA$employment)
UCLA$employment <- gsub('not in labor force1', 'not in labor force', UCLA$employment)
table(UCLA$employment)


# STATE
# Replace state abbreviations with state names, adding "DC" to the 
# state.abb vector and "district of columbia" to the state.name vector
UCLA$state <- append(state.name, values=c("district of columbia"))[match(
  UCLA$state, append(state.abb, values=c("DC")))]
# Make all state names lowercase
UCLA$state <- tolower(UCLA$state)
# Assign state names a numeric value between 1 and 51 in alphabetical order
UCLA$state <- as.factor(UCLA$state)
levels(UCLA$state) <- c(1:51)
UCLA$state <- as.numeric(UCLA$state)


###################################################################################
# Add the labels
UCLA <- labelled::to_factor(UCLA)



# INCOME
sum(table(UCLA$household_income))


table(UCLA$state)
nrow(table(UCLA$state))
sum(table(UCLA$state))

# create clean output file
write_csv(UCLA, "outputs/data/UCLA.csv")  