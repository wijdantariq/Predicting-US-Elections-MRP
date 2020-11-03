# Chosen model summary
stargazer(model2, type="latex",
          title="2020 US Election Popular Vote Outcome based on state, education, age, gender, race, and employment status"
          dep.var.labels=c("Selected Model"))

# Rejected model summaries
stargazer(model1, model4, type="latex", 
          dep.var.labels=c("Model 1: excluding employment and household income",
                           "Model 2: excluding employment and household income, age not as factor"))


# 
means <- ACS %>% summarise("Republican Vote Proportion"=mean(ACS$estimate), 
                  "Standard Deviation" = sd(ACS$estimate)) %>% 
  rbind(summarise(UCLA, "Republican Vote Proportion" = mean(UCLA$vote_2020), 
                  "Standard Deviation" = sd(UCLA$vote_2020)))
cbind(c("Post_stratification Estimate", "Survey Estimate"), means) %>% write.csv("votesummary.csv")