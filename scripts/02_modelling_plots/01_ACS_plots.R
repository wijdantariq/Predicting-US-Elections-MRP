# Code for creating the ACS plots.

library(tidyverse)
library(ggthemes)

# Load the clean ACS dataset.
ACS <- read_csv(("outputs/data/ACS.csv"), col_types = cols(census_region = col_character(), 
                                                           race_ethnicity = col_character(),
                                                           employment = col_character(),
                                                           gender = col_factor(),
                                                           age = col_factor(),
                                                           foreign_born = col_factor(),
                                                           state = col_number(),
                                                           education = col_number(),
                                                           household_income = col_number()
                                                           )
                )

# AGE
AGE <- ggplot(ACS, aes(x =age)) + 
  geom_bar(fill = 'steelblue', aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")+
  theme_minimal() +
  ggtitle("Age distribution in ACS")
AGE
ggsave("outputs/figures/ACS_age_distribution.pdf")

# GENDER 
GENDER <- ACS  %>% group_by(gender) %>% summarise(n = n()) %>%
  mutate(prop = 100 * (n / sum(n))) 
GENDER <- GENDER %>%
  ggplot(aes(x="", y=prop, fill=gender)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer(palette="Paired") +
  theme_void() + 
  ylab("") + xlab("") + 
  labs(title = "", fill="Gender ") + 
  geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.5), size = 4.7) +
  theme(text = element_text(size=15)) + ggtitle("Gender distribution in ACS")
GENDER
ggsave("outputs/figures/ACS_gender_distribution.pdf")

# EMPLOYMENT
EMP <- ACS  %>% group_by(employment) %>% summarise(n = n()) %>%
  mutate(prop = 100 * (n / sum(n))) 
EMP <- EMP %>%
  ggplot(aes(x="", y=prop, fill=employment)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer(palette="Paired") +
  theme_void() + 
  ylab("") + xlab("") + 
  labs(title = "", fill="employment ") + 
  geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.5), size = 4.7) +
  theme(text = element_text(size=15)) + ggtitle("Employment status distribution in ACS")
EMP
ggsave("outputs/figures/ACS_employment_distribution.pdf")

# RACE
RACE <- ACS  %>% group_by(race_ethnicity) %>% summarise(n = n()) %>%
  mutate(prop = 100 * (n / sum(n))) 
RACE <- RACE %>%
  ggplot(aes(x="", y=prop, fill=race_ethnicity)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer(palette="Paired") +
  theme_void() + 
  ylab("") + xlab("") + 
  labs(title = "", fill="race ") + 
  geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.5), size = 4.7) +
  theme(text = element_text(size=15)) + ggtitle("Race distribution in ACS")
RACE
ggsave("outputs/figures/ACS_race_distribution.pdf")