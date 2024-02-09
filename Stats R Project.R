library(tidyverse)
library(psych)
library(olsrr)

championship <- read_csv("C:/Users/garre/Downloads/england-championship-2018-to-2019 (3).csv")

# Regression Analysis:

# Correlation Analysis Between Two Quantitative Variables

# Correlation Between Home Shots on Target and Full-Time Home Goals

corr.test(championship$FTHG, championship$HST)

# Indicates a moderately strong positive relationship between the two variables.
# Based on the correlation, shots on target for home teams is a decent indicator of home teams scoring goals.

# Correlation Between Home Shots on Target and Full-Time Away Goals

corr.test(championship$FTAG, championship$AST)

# Indicates a moderately strong positive relationship between the two variables.
# Similar to home teams, shots on target for away teams is a decent indicator of away teams scoring goals.

# Due to both possessing similar levels of correlation, we can conclude that home-field does not increase a team's ability to successfully convert shots on target into goals.
# Also shows a lack of difference in goalie/keeper performance.

# Simple Linear Regression:

# Regression Analysis Between Home Team Shots and Home Shots on Target

ols_regress(lm(championship$HST ~ championship$HS, data = championship))

# Regression line is 0.019 + 0.325x.
# While small, indicates that as more shots are attempted, more shots on target will occur.
# p-value is 0.0000, which indicates the relationship between the two variables is statistically significant.
# Could be attributed to large sample size however, as larger sample sizes make tend to make things more significant.
# Correlation between the two variables is 0.646, indicating a moderately strong positive relationship.
# R-squared is 0.418, meaning 41.8% of the variability in home shots on target can be explained by home shot attempts.

# Regression analysis between Away Team Shots and Away Shots on Target

ols_regress(lm(championship$AST ~ championship$AS, data = championship))

# Regression line is 0.365 + 0.300x.
# Similar to home teams, the line indicates that as more shots are attempted, more shots on target will occur. 
# Due to the similarity of the slope, it shows that the rate of shots on target between home and away teams are about the same, showing almost no difference in home vs away performance.
# p-value is 0.0000, which indicates the relationship between the two variables is statistically significant.
# Could be attributed to large sample size however, as larger sample sizes make tend to make things more significant.
# Correlation between the two variables is 0.625, indicating a moderately strong positive relationship.
# R-squared is 0.390, meaning 39% of the variability in away shots on target can be explained by away shot attempts.

# Residual Analysis?

# Home Shots vs Home Shots on Target
regression_home <- lm(championship$HST ~ championship$HS, data = championship)
hresid <- resid(lm(championship$HST ~ championship$HS, data = championship))
plot(fitted(regression_home), hresid)
abline(0,0)
qqnorm(hresid)
qqline(hresid)

# Away Shots vs Away Shots on Target
regression_away <- lm(championship$AST ~ championship$AS, data = championship)
aresid <- resid(lm(championship$AST ~ championship$AS, data = championship))
plot(fitted(regression_away), aresid)
abline(0,0)
qqnorm(aresid)
qqline(aresid)


# Bivariate correlations for home teams

corr.test(x = select(championship, HF, HC, HY, HR))

# Bivariate correlation between home fouls and home corners is -0.13, indicating a weak negative relationship.
# Bivariate correlation between home fouls and home yellow cards is 0.37, indicating a fairly weak positive relationship.
# Bivariate correlation between home fouls and home red cards is -0.02, indicating practically no relationship.
# p-values for first two bivariate correlations are both 0.00, indicating statistical significance. This is likely due to large sample size however.
# p-value for home fouls and home red cards is 0.64, indicating no statistical significance.

# Bivariate correlations for away teams

corr.test(x = select(championship, AF, AC, AY, AR))

# Bivariate correlation between home fouls and home corners is -0.10, indicating a weak negative relationship.
# Bivariate correlation between home fouls and home yellow cards is 0.34, indicating a fairly weak positive relationship.
# Bivariate correlation between home fouls and home red cards is 0.01, indicating practically no relationship.
# p-values for first two bivariate correlations are 0.01 and 0.00 respectively, indicating statistical significance. This is likely due to large sample size however.
# p-value for home fouls and home red cards is 0.87, indicating no statistical significance.

# Hypothesis Testing
set.seed(1)
sample_home <- sample(x = championship$HF, size = 100)
sample_away <- sample(x = championship$AF, size = 100)
average_foul <- mean(((sample_home + sample_away)/2), na.rm = TRUE)
average_foul
t.test(HF1 ~ 1, alternative = "less", mu = 12, conf.level = 0.95, data = championship)

# Random sample of 100 results for both home and away to calculate the mean.
# Mean ended up equaling 12.29293.
# Wanted to see if there was a significant difference between the mean number of fouls the home team would be called for in a game compared to the average number of fouls called in a game.
# After taking numerous samples, the average fouls called in a game was closest to 12, so that's the number we decided to choose for the mu value. 
# 0.05 significance level.
# Null hypothesis: the number of fouls the home team is called per game is equal to or greater than 12.
# Alternative hypothesis: the number of fouls the home team is called per game is less than 12.
# p-value is 0.2495, keep the null.
# No statistical significance.
# p-value equals 0.2495, far higher than 0.05, meaning there is no significance between the number
