library(tidyverse)
library(ggthemes)




survey <- read.csv('survey.csv')


# AGE
AGE <- ggplot(survey, aes(x =age)) + 
  geom_bar(fill = 'steelblue', aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")+
  theme_minimal() +
  ggtitle("Respondents' Age Distribution")
AGE


# GENDER 
GENDER <- survey  %>% group_by(gender) %>% summarise(n = n()) %>%
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
  theme(text = element_text(size=15)) + ggtitle("Respondents' Gender Distribution")
GENDER



# EMPLOYMENT
EMP <- survey  %>% group_by(employment) %>% summarise(n = n()) %>%
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
  theme(text = element_text(size=15)) + ggtitle("Respondents' Employment status Distribution")
EMP


# RACE
RACE <- survey  %>% group_by(race_ethnicity) %>% summarise(n = n()) %>%
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
  theme(text = element_text(size=15)) + ggtitle("Respondents' Distribution of Race")
RACE


### Vote 2020
vote_2020_prop <- UCLA %>% group_by(vote_2020) %>% summarise(n = n()) %>%
  mutate(prop = 100 * (n / sum(n)))
p4 <- vote_2020_prop %>% ggplot(aes(x = "", y = prop, fill = )) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Greens") +
  theme_void() +
  ylab("") + xlab("") + 
  labs(title = "", fill="Trump") +
  geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.5), angle=45, size = 4.7) +
  theme(text = element_text(size=15))

# Pie Chart   
slices <- c(2447,2680)
lbls <- c("Trump", "Biden")
pct <-round(slices/sum(slices)*100)
lbls <-paste(lbls, pct)
lbls <-paste(lbls, "%", sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Distribution of Voter choice")
