install.packages("lme4")
install.packages("stargazer")
library(lme4)
library(tidyverse)
library(stargazer)

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

# SELECTED MODEL
model2 <- glmer(vote_2020 ~ (1|state) + education + factor(age) + factor(gender) + 
                  factor(race_ethnicity) + factor(employment), data=UCLA, family=binomial(link="logit"))

model3 <- glmer(vote_2020 ~ (1|state) + education + factor(age) + factor(gender) + 
                  factor(race_ethnicity) + household_income, data=UCLA, family=binomial(link="logit"))
model4 <- glmer(vote_2020 ~ (1|state) + education + age + factor(gender) + 
                  factor(race_ethnicity), data=UCLA, family=binomial(link="logit"))
summary(model2)

# Estimate likelihood to vote Republican of each ACS respondent 
ACS$estimate <- model2 %>% predict(newdata=ACS, type="response") %>% round(digits=3)

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

gender <- ACS %>% group_by(gender) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                 sd=round(sd(estimate), digits=3), 
                                                 lower=round(quantile(estimate, 0.025), 
                                                             digits=3), 
                                                 upper=round(quantile(estimate, 0.975), digits=3))
gender <- cbind(gender, UCLA %>% group_by(gender) %>% summarise(survey_mean=round(mean(vote_2020), digits=4)) %>% select(survey_mean))


age <- ACS %>% group_by(age) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                           sd=round(sd(estimate), digits=3), 
                                           lower=round(quantile(estimate, 0.025), 
                                                       digits=3), 
                                           upper=round(quantile(estimate, 0.975), digits=3))
age <- cbind(age, UCLA %>% group_by(age) %>% summarise(survey_mean=round(mean(vote_2020), digits=4)) %>% select(survey_mean))


race_ethnicity <- ACS %>% group_by(race_ethnicity) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                                 sd=round(sd(estimate), digits=3), 
                                                                 lower=round(quantile(estimate, 0.025), 
                                                                             digits=3), 
                                                                 upper=round(quantile(estimate, 0.975), digits=3))
race_ethnicity <- cbind(race_ethnicity, UCLA %>% group_by(race_ethnicity) %>% summarise(survey_mean=round(mean(vote_2020), digits=4)) %>% select(survey_mean))

education <- ACS %>% group_by(education) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                       sd=round(sd(estimate), digits=3), 
                                                       lower=round(quantile(estimate, 0.025), 
                                                                   digits=3), 
                                                       upper=round(quantile(estimate, 0.975), digits=3))
education <- cbind(education, UCLA %>% group_by(education) %>% summarise(survey_mean=round(mean(vote_2020), digits=4)) %>% select(survey_mean))

foreign_born <- ACS %>% group_by(foreign_born) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                             sd=round(sd(estimate), digits=3), 
                                                             lower=round(quantile(estimate, 0.025), 
                                                                         digits=3), 
                                                             upper=round(quantile(estimate, 0.975), digits=3))
foreign_born <- cbind(foreign_born , UCLA %>% group_by(foreign_born) %>% summarise(survey_mean=round(mean(vote_2020), digits=4)) %>% select(survey_mean))

employment <- ACS %>% group_by(employment) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                         sd=round(sd(estimate), digits=3), 
                                                         lower=round(quantile(estimate, 0.025), 
                                                                     digits=3), 
                                                         upper=round(quantile(estimate, 0.975), digits=3))
employment <- cbind(employment , UCLA %>% group_by(employment) %>% summarise(survey_mean=round(mean(vote_2020), digits=4)) %>% select(survey_mean))


# Survey data summaries
UCLA$state <- as.factor(UCLA$state)
levels(UCLA$state) <- append(state.name, values="District of Columbia", after=8)
UCLA_states <- UCLA %>% group_by(state) %>% summarise(mean=round(mean(vote_2020), digits=4))

UCLA_employment <- UCLA %>% group_by(employment) %>% summarise(mean=round(mean(vote_2020), digits=4))

mean(UCLA$vote_2020)
mean(ACS$estimate)

overall <- cbind(states, raw_data_mean=UCLA_states$mean)

# TABLES
ACS %>% summarise(Vote=mean(ACS$estimate), 
                  'Standard Deviation' = sd(ACS$estimate)) %>% 
  rbind(summarise(UCLA, Vote = mean(UCLA$vote_2020), 
                  "Standard Deviation" = sd(UCLA$vote_2020)))

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

par(mfrow=c(2,2))
# Age post-stratification estimates
age %>% ggplot(aes(x=age, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=age, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=age, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Age Group") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Age Group, Survey versus Post-Stratified Estimates")
ggsave("outputs/figures/survey_versus_poststrat_age.pdf")

# Race post-stratification estimates
race_ethnicity$race_ethnicity <- str_to_title(race_ethnicity$race_ethnicity)
race_ethnicity %>% ggplot(aes(x=race_ethnicity, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=race_ethnicity, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=race_ethnicity, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Racial or Ethnic Identity") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Race or Ethnicity, Survey versus Post-Stratified Estimates")
ggsave("outputs/figures/predicted_republican_race.pdf")

# Education post-stratification estimates
education$education <- as.factor(education$education)
levels(education$education) <- c('Less than high school', 'Some high school',
                                 'Completed high school', 'Some post-secondary',
                                 'Post-secondary degree', 'Post-graduate degree')
education %>% ggplot(aes(x=education, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=education, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=education, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + 
  xlab("Highest Level of Education Completed") + 
  ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Educational Attainment, Survey versus Post-Stratified Estimates")
ggsave("outputs/figures/predicted_republican_education.pdf")


# Employment post-stratification estimates
employment$employment <- str_to_sentence(employment$employment)
employment %>% ggplot(aes(x=employment, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=employment, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=employment, y=survey_mean), color="dark grey") +
  xlab("Employment Status") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Employment Status, Survey versus Post-Stratified Estimates")
ggsave("outputs/figures/predicted_republican_employment.pdf")


# Gender post-stratification estimates
gender$gender <- str_to_sentence(gender$gender)
gender %>% ggplot(aes(x=gender, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=gender, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=gender, y=survey_mean), color="dark grey") +
  xlab("Gender") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Republican Proportion of Popular Vote") +
  labs(subtitle="By Gender, Survey versus Post-Stratified Estimates")
ggsave("outputs/figures/predicted_republican_gender.pdf")

# Regression ouput
install.packages("latexpdf")
library(latexpdf)

model2_table <- stargazer(model2, type="latex",
          title="2020 US Election Popular Vote Outcome based on state, education, age, gender, race, and employment status",
          dep.var.labels=c("Selected Model"))

as.pdf(model2_table, "/outputs/tables/regression.pdf")

# Rejected model summaries
rejected_models <- stargazer(model1, model4, type="latex", 
          dep.var.labels=c("Model 1: excluding employment and household income",
                           "Model 2: excluding employment and household income, age not as factor"))

as.pdf(rejected_models, "/rejected_models.pdf")

#means
means <- ACS %>% summarise("Republican Vote Proportion"=mean(ACS$estimate), 
                           "Standard Deviation" = sd(ACS$estimate)) %>% 
  rbind(summarise(UCLA, "Republican Vote Proportion" = mean(UCLA$vote_2020), 
                  "Standard Deviation" = sd(UCLA$vote_2020)))
cbind(c("Post_stratification Estimate", "Survey Estimate"), means) %>% write.csv("votesummary.csv")