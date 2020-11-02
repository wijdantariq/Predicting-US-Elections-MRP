install.packages("lme4")
library(lme4)
library(tidyverse)

# Read in the clean survey data and clean post-strat data.
UCLA <- read_csv(("outputs/data/UCLA.csv"), col_types = cols(employment = col_character(), 
                                                              race_ethnicity = col_character(),
                                                              foreign_born = col_factor(),
                                                              gender = col_factor(),
                                                              census_region = col_factor(),
                                                              household_income = col_factor(),
                                                              age = col_factor(),
                                                              vote_2020 = col_number(),
                                                              education = col_number(),
                                                              state = col_number()
                                                             )
                 )

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


model1 <- glmer(vote_2020 ~ (1|state) + education + factor(age) + factor(gender) + 
                 factor(race_ethnicity), data=UCLA, family=binomial(link="logit"))
#
model2 <- glmer(vote_2020 ~ (1|state) + education + factor(age) + factor(gender) + 
                  factor(race_ethnicity) + factor(employment), data=UCLA, family=binomial(link="logit"))
#
model3 <- glmer(vote_2020 ~ (1|state) + education + factor(age) + factor(gender) + 
                  factor(race_ethnicity) + household_income, data=UCLA, family=binomial(link="logit"))
model4 <- glmer(vote_2020 ~ (1|state) + education + age + factor(gender) + 
                  factor(race_ethnicity), data=UCLA, family=binomial(link="logit"))
summary(model1)

# Note that as a factor education is insignificant, but as a numeric variable it is significant
ACS$estimate <- model2 %>% predict(newdata=ACS, type="response") %>% round(digits=3)
mean(ACS$estimate)

# Construct table of cells and add estimates for each level
cells <- plyr::count(ACS[complete.cases(ACS),] %>% select(state, education, age, gender, race_ethnicity, employment))
cells$estimate <- model2 %>% predict(newdata = cells, type="response")

# Change state variable back into state names
ACS$state <- as.factor(ACS$state)
levels(ACS$state) <- append(state.name, values="District of Columbia", after=8)

# Create summary tables for post-stratification outcome grouped by each variable
# for use in constructing plots
states <- ACS %>% group_by(state) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                      sd=round(sd(estimate), digits=3), 
                                      lower=round(quantile(estimate, 0.025), 
                                                  digits=3), 
                                      upper=round(quantile(estimate, 0.975), digits=3))
age <- ACS %>% group_by(age) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                    sd=round(sd(estimate), digits=3), 
                                                    lower=round(quantile(estimate, 0.025), 
                                                                digits=3), 
                                                    upper=round(quantile(estimate, 0.975), digits=3))
race_ethnicity <- ACS %>% group_by(race_ethnicity) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                     sd=round(sd(estimate), digits=3), 
                                                     lower=round(quantile(estimate, 0.025), 
                                                                 digits=3), 
                                                     upper=round(quantile(estimate, 0.975), digits=3))
education <- ACS %>% group_by(education) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                 sd=round(sd(estimate), digits=3), 
                                                 lower=round(quantile(estimate, 0.025), 
                                                             digits=3), 
                                                 upper=round(quantile(estimate, 0.975), digits=3))
foreign_born <- ACS %>% group_by(foreign_born) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                    sd=round(sd(estimate), digits=3), 
                                                    lower=round(quantile(estimate, 0.025), 
                                                                digits=3), 
                                                    upper=round(quantile(estimate, 0.975), digits=3))
employment <- ACS %>% group_by(employment) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                             sd=round(sd(estimate), digits=3), 
                                                             lower=round(quantile(estimate, 0.025), 
                                                                         digits=3), 
                                                             upper=round(quantile(estimate, 0.975), digits=3))

# Survey data summaries
UCLA$state <- as.factor(UCLA$state)
levels(UCLA$state) <- append(state.name, values="District of Columbia", after=8)
UCLA_states <- UCLA %>% group_by(state) %>% summarise(mean=round(mean(vote_2020), digits=4))

mean(UCLA$vote_2020)
mean(ACS$estimate)

overall <- cbind(states, raw_data_mean=UCLA_states$mean)

# TABLES
summarise(ACS, Vote=mean(ACS$estimate), 'Standard Deviation' = sd(ACS$estimate)) %>% rbind(summarise(UCLA, Vote = mean(UCLA$vote_2020), "Standard Deviation" = sd(UCLA$vote_2020)))

# PLOTS
install.packages("usmap")
library(usmap)

# Republican proportion of vote by state on map
plot_usmap(data=states, values="post_strat_mean") + 
  scale_fill_continuous(name="Republican Proportion of Popular Vote", 
                        low="white", high="red") + theme(legend.position="top") +
  ggtitle("Republican Favourability by State (Post-Stratification Data)")
ggsave("outputs/figures/map.pdf")


# State post-stratification estimates versus survey estimates
ggplot(states, aes(x=reorder(state, post_strat_mean), y=post_strat_mean)) + geom_point() + geom_point(data=UCLA_states, aes(x=state, y=mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("State") + 
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="Survey versus Post-Stratified Estimates by State")
ggsave("outputs/figures/survey_versus_poststrat_state.pdf")

# Age post-stratification estimates
ggplot(age, aes(x=age, y=post_strat_mean)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Age Group") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="Survey versus Post-Stratified Estimates")
ggsave("outputs/figures/survey_versus_poststrat_age.pdf")

# Race post-stratification estimates
race_ethnicity$race_ethnicity <- str_to_title(race_ethnicity$race_ethnicity)
ggplot(race_ethnicity, aes(x=reorder(race_ethnicity, post_strat_mean), y=post_strat_mean)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Race") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Racial or Ethnic Identity")
ggsave("outputs/figures/predicted_republican_race.pdf")

# Education post-stratification estimates
education$education <- as.factor(education$education)
levels(education$education) <- c('Less than high school', 'Some high school',
                                'Completed high school', 'Some post-secondary',
                                'Post-secondary degree', 'Post-graduate degree')
ggplot(education, aes(x=education, y=post_strat_mean)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Education") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Level of Education")
ggsave("outputs/figures/predicted_republican_education.pdf")


# Employment post-stratification estimates
employment$employment <- str_to_sentence(employment$employment)
ggplot(employment, aes(x=reorder(employment, post_strat_mean), y=post_strat_mean)) + geom_bar(stat="identity") + 
  xlab("Employment") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Employment Status")
ggsave("outputs/figures/predicted_republican_employment.pdf")