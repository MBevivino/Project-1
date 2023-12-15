### POL 501 Final Project Script 2 ### 
### Exploratory Data Analysis ###
# Load the libraries
library(ggplot2)
library(dplyr)

# Viewing the first few rows of the dataset
head(merged_df)
# Summary of the dataset
summary(merged_df)
# Structure of the dataset
str(merged_df)
# Descriptive statistics for key numerical columns
merged_df %>%
  select(Median_Household_Income, Median_Age, Number_of_Unemployed) %>%
  summary()

# Histogram for Median Household Income
ggplot(merged_df, aes(x = Median_Household_Income)) + 
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Distribution of Median Household Income", x = "Median Household Income", y = "Count")

# Boxplot for Median Age
ggplot(merged_df, aes(y = Median_Age)) + 
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Median Age", y = "Median Age")

# Inspect the structure of the dataframe
str(merged_df)
## CREATE DEMOCRATIC VOTE SHARE VARIABLE AND REPUBLICAN VOTE SHARE VARIABLE 
# Assuming 'JOSEPH R BIDEN JR' is the Democratic candidate
merged_df <- merged_df %>%
  group_by(County_Name, State_Name) %>%
  mutate(Democratic_Vote_Share = ifelse(candidate == "JOSEPH R BIDEN JR", candidatevotes / totalvotes * 100, NA_real_))
# Assuming 'DONALD J TRUMP' is the Republican candidate for 2020
merged_df <- merged_df %>%
  mutate(Republican_Vote_Share = ifelse(candidate == "DONALD J TRUMP", candidatevotes / totalvotes * 100, NA_real_))

cor.test(merged_df$Median_Household_Income, merged_df$Democratic_Vote_Share, use = "complete.obs")
cor.test(merged_df$Median_Household_Income, merged_df$Republican_Vote_Share, use = "complete.obs")

# ... and so on for other variables


dem_vote_model <- lm(Democratic_Vote_Share ~ Median_Household_Income + Median_Age + People_with_Bachelors, data = merged_df)
summary(dem_vote_model)

rep_vote_model <- lm(Republican_Vote_Share ~ Median_Household_Income + Median_Age + People_with_Bachelors, data = merged_df)
summary(rep_vote_model)

merged_df %>% 
  group_by(State_Name) %>%
  summarise(Avg_Dem_Share = mean(Democratic_Vote_Share, na.rm = TRUE),
            Avg_Rep_Share = mean(Republican_Vote_Share, na.rm = TRUE))

plot(dem_vote_model)
plot(rep_vote_model)

library(ggplot2)

# Scatter plot for each predictor against the dependent variable
ggplot(merged_df, aes(x = Median_Household_Income, y = Democratic_Vote_Share)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Median Household Income vs. Democratic Vote Share")

###### VISUALIZATIONS #######
library(ggplot2)

# Histogram for Median Household Income
ggplot(merged_df, aes(x = Median_Household_Income)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Distribution of Median Household Income", x = "Median Household Income", y = "Frequency")

# Histogram for Democratic Vote Share
ggplot(merged_df, aes(x = Democratic_Vote_Share)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Democratic Vote Share", x = "Democratic Vote Share (%)", y = "Frequency")

# Scatter Plot for Median Household Income vs. Democratic Vote Share
ggplot(merged_df, aes(x = Median_Household_Income, y = Democratic_Vote_Share)) +
  geom_point(color = "red") +
  labs(title = "Median Household Income vs. Democratic Vote Share", x = "Median Household Income", y = "Democratic Vote Share (%)")

# Boxplot for Democratic Vote Share by State
ggplot(merged_df, aes(x = State_Name, y = Democratic_Vote_Share)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Democratic Vote Share by State", x = "State", y = "Democratic Vote Share (%)")

# Correlation between Median Household Income and Democratic Vote Share
cor.test(merged_df$Median_Household_Income, merged_df$Democratic_Vote_Share, use = "complete.obs")

# Correlation between Median Age and Democratic Vote Share
cor.test(merged_df$Median_Age, merged_df$Democratic_Vote_Share, use = "complete.obs")

# Correlation between People with Bachelors and Democratic Vote Share
cor.test(merged_df$People_with_Bachelors, merged_df$Democratic_Vote_Share, use = "complete.obs")

##### REGRESSION ######
lm_income_vs_dem_vote <- lm(Democratic_Vote_Share ~ Median_Household_Income, data = merged_df)
summary(lm_income_vs_dem_vote)

lm_age_vs_dem_vote <- lm(Democratic_Vote_Share ~ Median_Age, data = merged_df)
summary(lm_age_vs_dem_vote)

lm_bachelors_vs_dem_vote <- lm(Democratic_Vote_Share ~ People_with_Bachelors, data = merged_df)
summary(lm_bachelors_vs_dem_vote)

lm_combined <- lm(Democratic_Vote_Share ~ Median_Household_Income + Median_Age + People_with_Bachelors, data = merged_df)
summary(lm_combined)

lm_race_vs_dem_vote <- lm(Democratic_Vote_Share ~ People_Identifying_as_White, data = merged_df)
summary(lm_race_vs_dem_vote)

lm_all_factors <- lm(Democratic_Vote_Share ~ Median_Household_Income + Median_Age + People_with_Bachelors + People_Identifying_as_White, data = merged_df)
summary(lm_all_factors)


