# ---
# title: "MSCA 31007 Statistical Analysis - Group Assignment 2"
# author: "Aashish Singh, Alexander Saucedo, Prinu Mathew, Nyckeisha' Sam, Qingwei Zhang"
# start_date: "10/25/2022"
# last_revision_date: "11/14/2022"
# ---

# Reference links
# https://walker-data.com/census-r/mapping-census-data-with-r.html
# https://api.census.gov/data/2019/acs/acs5/profile/variables.html
# https://walker-data.com/isds-webinar/#21


# ---
# Install all required packages and install census api key
# ---

install.packages(c("tidycensus", "tidyverse", "gridExtra"))
install.packages("lmtest")
install.packages("nptest")
install.packages("MASS")
install.packages("olsrr")
install.packages("fitdistrplus")
install.packages("ggpubr")
install.packages("scatterplot3d")
install.packages("rgl")
install.packages("ggiraph")
install.packages("ggiraphExtra")
install.packages("plyr")
install.packages("Metrics")
install.packages('leaps')
library(olsrr)
library(tidycensus)
library(tidyverse)
library(sf)            # Objects and functions for geospatial data
library(rgdal)         # Functions for spatial data input/output
library(ggplot2)       # Graphing functions
library(dplyr)         # Functions for processing tabular data
library(tidyr)         # Functions for processing tabular data
library(scales)        # Additional graphics functions
library(RColorBrewer)  # Color ramps for graphs and maps
library(gridExtra)     # Functions for arranging multiple plots on a page
library(readr)         # Functions for reading data
library(lmtest)
library(nptest)
library(MASS)
library(fitdistrplus)
library(ggpubr)        # Functions for plotting the regression and a wide range of measures
library(Metrics)
library(leaps)

# Enter Census API Key
census_api_key("0c4a2a2815a8d526966f2490024ef157e19478db", overwrite = TRUE, install = TRUE)


# ---
# Perform Step 1
#Restore the dataset on Cook County census tracts and the model you used in the previous group assignment 
#which linked household income with college degree attainment.


# 1.a Define specific ACS variables we need to pull
# And, bring in tract-level data from the 2015-2019 American Community Survey (ACS) 5-year estimates for Cook County, IL
# 5-year ACS with the argument survey = "acs5", starting from 2015 till 2019
acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE')
census_tidy_2015_2019 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2019, geometry = TRUE, survey = "acs5"
)

# 1.b Drop the columns which report margin of error
census_tidy_dropcols_2015_2019 <- census_tidy_2015_2019[,!(names(census_tidy_2015_2019) %in% "moe")]
# Format your output as a ‘wide’ table, not a ‘tidy’ table
census_wide_2015_2019 <- census_tidy_dropcols_2015_2019 %>% 
  pivot_wider(names_from = 'variable', values_from = c('estimate'))

# 1.c Rename the remaining columns
# DP02_0065P -> propbac  (Bachelor's degree)
# DP03_0062  -> medhhinc (Median household income)
# DP03_0096P -> propcov  (Health insurance coverage)
# DP03_0128P -> proppov  (PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL)
# DP04_0047P -> proprent (Renter-occupied)
# DP05_0001  -> totpop   (Total population)
# DP05_0018  -> medage   (Median age)
census_final_2015_2019 <- census_wide_2015_2019 %>%  
  rename('geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065P', 
         'medhhinc' = 'DP03_0062', 'propcov' = 'DP03_0096P', 'proppov' = 'DP03_0128P', 
         'proprent' = 'DP04_0047P', 'totpop' = 'DP05_0001', 'medage' = 'DP05_0018')

ggplot(data = census_final_2015_2019, aes(fill = propbac)) +
  geom_sf() + 
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1, 
                       breaks = pretty_breaks()) +
  labs(title="Tract-level baccalaureate attainment rates",
       subtitle = "Cook County, Illinois",
       caption = "Data: 2015-2019 5-year ACS, US Census Bureau",
       fill = "Percentage") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey", color = NA))

rm(census_tidy_dropcols_2015_2019)
rm(census_tidy_2015_2019)
rm(census_wide_2015_2019)

# ---
# Perform Step 2
#Create a new regression which uses income as well as all the remaining variables (excluding geography) 
#to predict college degree attainment levels.
# ---

# Remove the rows from dataframe that contains at least one NA
census_final_2015_2019 <- na.omit(census_final_2015_2019) 


# 2.a What is the change in R^2 between the model with a single predictor and the model with
#   all predictors? Consider a real-world interpretation for this difference.

#Regression line with single predictor
census_final_2015_2019.lm <- lm(propbac ~ medhhinc, data = census_final_2015_2019)
# Regression line with all predictors
census_final_2015_2019.lm.all <- lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019)

# Change in R^2 between the two models
census_final_2015_2019.lm.summary <- summary(census_final_2015_2019.lm)
summary(census_final_2015_2019.lm)
census_final_2015_2019.lm.all.summary <- summary(census_final_2015_2019.lm.all)
summary(census_final_2015_2019.lm.all)

# Though median household income explain 53.5% variance in college degree attainment. 
# Factors like health insurance coverage, renter occupied, and percentage of families whose income is 
# below poverty level help improve model's overall R^2 (71.5% of variance in college degree attainment
# is now explained due to addition of these variables). It is interesting to note that total 
# population of a tract and median age of the tract do not have  significant contribution towards the model.


# 2.b Perform an ANOVA-based F test to determine whether the difference in explanatory
#   power between these two models is significant.

#Let’s use the anova() function to compare these models and see which one provides the best parsimonious fit of the data.
#This ANVOA will test whether or not including multiple predictor variables propcov,proppov,proprent,totpop,medage 
#(both models use medhhinc) leads to a significant improvement over using just single predictor variable 'medhhinc'

#H0: Single predictor is enough to predict the baccalaureate attainment rate (propbac)
#HA: Single predictor is not enough to predict the baccalaureate attainment rate (propbac)

# H0: the difference in explanatory power between these two models is not significant
# H1: the difference in explanatory power between these two models is significant

#Compare model with single predictor to model with all predictor
census_final_2015_2019_anova <- anova(census_final_2015_2019.lm, census_final_2015_2019.lm.all)
census_final_2015_2019_anova

#The larger the F-statistic, the greater the variation between sample means relative to the variation within the samples
#Thus, the larger the F-statistic, the greater the evidence that there is a difference between the group means

#As you can see, the result shows a Df of 5 (indicating that the complex model has more additional parameter), 
#and a very small p-value (< .0000001) which is smaller than 0.05. This means that adding the multiple predicator 
#variables (propcov,proppov,proprent,totpop,medage) to the model did lead to a significantly improved 
#fit over the model with single predictor. Hence rejecting the NULL hypothesis of the ANOVA


#- 2.c Plot the empirical densities of the residuals from these two models, with both
#   distributions appearing on the same graph. Make the graph as close to publication-ready
#   as you can. Be prepared to discuss whether the full model has not just added explanatory
#   power, but improved the fit with OLS (Ordinary Least Squares) model assumptions.

census_final_2015_2019.lm.residuals <- data.frame(residuals = census_final_2015_2019.lm[['residuals']])
census_final_2015_2019.lm.residuals$model <- 'Single'
census_final_2015_2019.lm.all.residuals <- data.frame(residuals = census_final_2015_2019.lm.all[['residuals']])
census_final_2015_2019.lm.all.residuals$model <- 'All'

ggplot() +
  stat_ecdf(data = census_final_2015_2019.lm.residuals, col="red", aes(x = residuals, linetype = model), size = 1.2) +
  stat_ecdf(data = census_final_2015_2019.lm.all.residuals, col="blue", aes(x = residuals, linetype = model), size = 1.2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_linetype_manual(values=c("solid", "dotted")) +
  scale_color_manual(values = c("Single" = "red", "All" = "blue"),
                     name="Model",
                     breaks=c("Single", "All"),
                     labels=c("Single Predictor", "All Predictors")) +
  scale_size_manual(values=c(1, 1.5))+
  labs(x='Residuals Distribution', 
       y='Cumulative Probability Distribution',
       title="Empirical Densities of the Residuals (Single vs All Predictors Model)",
       caption = "Data: 2015-2019 5-year ACS, US Census Bureau, Cook County, IL",
       subtitle = sprintf("Adjusted R-squared (Single): %s, Actual Adjusted R-squared (Multiple): %s", 
                          round(census_final_2015_2019.lm.summary$adj.r.squared, 4), 
                          round(census_final_2015_2019.lm.all.summary$adj.r.squared, 4))) +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'), 
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = c(0.9, 0.5),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid", colour ="darkblue"))



#Diagnostic plots with single predictor
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019.lm)

# diagnostic plots with all predictor
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019.lm.all)


##############################   ADD KS TEST, DW TEST, BP TEST ###################################
############################## OLS model assumption need to be checked ###########################
# There is slight improvement in normality, heteroskedascity, and serial correlation but not significant
# But still model R^2 improves without affecting OLS assumptions
# And also Adj R^2


# i)Normality
# Check the normality assumption by creating a Q-Q plot:
ols_plot_resid_qq(census_final_2015_2019.lm)
ols_plot_resid_qq(census_final_2015_2019.lm.all)
#This qq plot shows the distribution of residuals .If the Q-Q plot forms a diagonal line, you can assume that the residuals follow a normal distribution.
#Here from the plot we can see model with single predictor doesnt follow normal distribution but model with all predictors
# normality assumption improves since it  does follow a diagonal line mostly,except some deviations in the extremities

# Let's plot a histogram of the residuals to observe this further:
hist(census_final_2015_2019.lm$residuals, main = "Residual Histogram of Baccalaureate Attainment Rate Model with single predictor")
hist(census_final_2015_2019.lm.all$residuals, main = "Residual Histogram of Baccalaureate Attainment Rate Model with all predictors")

# The histogram for single predictor model is right skewed whereas all predictor model does follow a normal distribution.

# Also, using hypothesis testing to evaluate normality assumptions:
# H0 -> The residuals follow a normal distribution
# H1 -> The residual does not follow a normal distribution
ols_test_normality(census_final_2015_2019.lm)
ols_test_normality(census_final_2015_2019.lm.all)
# As the data has more than 50 observation, We should use the Kolmogorov-Smirnov test to examine the normality of the residuals.
#P value for single predictor model is below 0.05 , we reject null hypothesis and conclude residuals doesnt follow normal distribution
# Whereas for all predicctor model the p-value is above 0.05, we failed to reject the null hypothesis and conclude that the residuals do follow a normal distribution.

# ii)Serial correlation
# Let's check for serial correlation by plotting acf:
plot(acf(census_final_2015_2019.lm$residuals, type = "correlation"))
plot(acf(census_final_2015_2019.lm.all$residuals, type = "correlation"))
# Using hypothesis testing to evalaute serial correlation assumptions:
# H0 -> There is no first order serial correlation among the residuals
# H1 -> There is first order serial correlation in residuals
dwtest(formula = census_final_2015_2019.lm, alternative = "two.sided")
dwtest(formula = census_final_2015_2019.lm.all, alternative = "two.sided")
#Pvalue=0.00559 for single predictor model which means p-value < 0.05, thus we reject the NULL hypothesis,
#hence there is serial correlation among residuals
# Whereas for all predictor model Since p-value = 0.1803, which means p-value > 0.05, thus we failed to reject the NULL hypothesis
# We conclude that there is no serial correlation present among the residuals.

# iii) Heteroskedasticity
# Heteroscedasticity is the situation in which the variance of the residuals of a regression model
# is not the same across all values of the predicted variable.
# Check the heteroskedasticity assumption by creating a plot:
plot(census_final_2015_2019.lm, which = 1)
plot(census_final_2015_2019.lm.all, which = 1)
# Both model the plots shows a deviation from horizontal line meaning there is heteroscedasticity.
# but for all predictor model there is some imporvement.

# Also, using hypothesis testing to evaluate heteroskedasticity assumptions:
# H0 -> Residuals are distributed with equal variance (i.e homoscedasticity)
# H1 -> Residuals are distributed with unequal variance (i.e heteroskedasticity)
lmtest::bptest(census_final_2015_2019.lm)
lmtest::bptest(census_final_2015_2019.lm.all)
# p-value is less the significance threshold (alpha) of 0.05, in both  models , we can reject NULL hypothesis
#and  residuals are distributed with unequal variance thus the residual distribution is heteroskedasticity.
#but for all predictor model p value is slightly larger hence we some improvemnet in Heteroskedasticity.

# OLS model assumptions imporved for all prdictor model


# ---
# Perform Step 3
# In one to two paragraphs, summarize the difference in performance between the two models and make a recommendation 
# of which to use when deciding future policy actions
# ---

# Residual spread is closer in  model 2 (ecdf plot)
# Then OLS assumption are not worse
# Adj R2 is higher for model 2 and through anova we see it's significant different from model 1

#Mean Square Error(MSE)/Root Mean Square Error(RMSE)
sqrt(mean(census_final_2015_2019.lm.summary$residuals^2))
sqrt(mean(census_final_2015_2019.lm.all.summary$residuals^2))

#Mean Absolute Error(MAE)
mean(abs(census_final_2015_2019.lm.summary$residuals))
mean(abs(census_final_2015_2019.lm.all.summary$residuals))

# So RMSE is reduced by 1.91pp in model 2 vs model 1
# And MAE is reduced by 1.53pp in model 2 vs model 1

# We recommend model 2 given all above factors.


#Three main metrics for model evaluation in regression. R-Square/Adjusted-R Square is better used to explain the model 
#to other people because you can explain the number as a percentage of the output variability. MSE, RMSE, or MAE 
#are better be used to compare performance between different regression models


#For model 1 with one predictor 'median household income', the R-squared value is 0.5351 which means only 54% of the
#dependent variable (baccalaureate attainment rate) can be explained by the model. 
#For model 2 with all predictors, the R-squared value is 0.7149 which means 71% of the baccalaureate attainment rate
#can be explained by the model. Also to prevent the overfitting problem in model 2, we have considered the Adjusted R-Square
#which will penalize additional independent variables added to the model. Both R-squared and Adjusted R-Square are pretty closer
#which tells model 2 out performs model 1 as it indicates a better fit between prediction and actual value.

#Now comparing Root Mean Square Error(RMSE) between model 1 and model 2, model 2 value 6.8824 is slightly lower than
#model 1 value 8.7887 which indicates the concentration of the data points for model 2 is closer to the line of
#best fit than model 1.

#Now comparing Mean Absolute Error(MAE) between model 1 and model 2, model 2 value 5.3849 is slightly lower than
#model 1 value 6.9197 which indicates the sum of error is smaller and concentration of the data points for model 2 
#is closer to the line of best fit than model 1


# ---
# Perform Step 4
#Discuss amongst your group whether each of the predictors fit into one of these three categories
# ---

## now that we know model 2 is performing better and should be the preffered model for making policy decisions
## we look at the variables

summary(census_final_2015_2019.lm.all)


# 4.a Predictors with no significant explanatory power

# Total population and median age do not have statistically greater contribution to the model
# And thus have no explanatory power.


# 4.b Predictors with explanatory power, useful as control variables, but without a policy “lever” that 
# decision makers could use to increase college degree attainment

# Confused about this one as there is none factor that cannot be affected by policy lever



# 4.c Predictors with both explanatory power and a corresponding policy “lever”

# health coverage can be improved
# renter occupied can be subsidized 
# median income and poverty line can be reduced by lower taxes / giving better unemployment benefits / basic income

#proprent (Renter-occupied) has P-value way lesser than 0.05 and second biggest contributor to R-squared after medhhinc (Median household income). 
#County executives has the power to increase the baccalaureate attainment rates by introducing new ordinances about what 
#proportion of housing has to be available for rent or put like big penalties on high rise. 
#For Condos you can do all sorts of things to change the proportion of renters and owners like giving subsidies to renters or 
#subsidies to homeowners. So that's something where I think a decision maker, like an executive has a lever that they can pull

#propcov (Health insurance coverage) can have some impact on increasing baccalaureate attainment rate if the government provides
#free or lesser expensive medical coverage to household with lower income brackets



# ---
# Perform Step 5
#Make a proposal for increasing overall college degree attainment in Cook County by roughly 5
#percentage points. The solution does not need to be politically feasible and you may assume
#causal links without proving they exist. However, your proposal should still be data-driven
#and you should back up your argument with output from your regression.
# ---

#Baseline values for dependent variable
propbac_mean <- mean(census_final_2015_2019$propbac)

#Baseline values for predictor variables
medhhinc_mean <- mean(census_final_2015_2019$medhhinc)
propcov_mean <- mean(census_final_2015_2019$propcov)
proppov_mean <- mean(census_final_2015_2019$proppov)
proprent_mean <- mean(census_final_2015_2019$proprent)

#Do regression of all predictors
census_final_2015_2019.lm.all <- lm(propbac ~ medhhinc+propcov+proppov+proprent, data = census_final_2015_2019)
census_final_2015_2019.lm.all.summary <- summary(lm(propbac ~ medhhinc+propcov+proppov+proprent, data = census_final_2015_2019))
census_final_2015_2019.lm.all.summary

#Predicting the existing average college degree attainment rate for Cook County
attainment_rate_actual <- predict(census_final_2015_2019.lm.all, newdata= list(medhhinc=medhhinc_mean, propcov=propcov_mean, proppov=proppov_mean, proprent=proprent_mean))
attainment_rate_actual

#Adjusting the predictor variable values to desired amount to get the 5% increase in 
#college degree attainment rate for Cook County

medhhinc_target <- medhhinc_mean #+  (medhhinc_mean*5.5)/100
propcov_target <- propcov_mean #+  (propcov_mean*7)/100
proppov_target <- 0 #proppov_mean #- (proppov_mean*0.1)/100
proprent_target <- proprent_mean #+  (proprent_mean*5)/100

attainment_rate_target <- predict(census_final_2015_2019.lm.all, newdata= list(medhhinc=medhhinc_target, propcov=propcov_target, proppov=proppov_target, proprent=proprent_target))
attainment_rate_target
attainment_rate_actual

# Take the tracts with proppov that is in the lowest 25th percentile and reduce proppov to 0% and increase propcov to 100% and proprent to 100%
modify_census_for_best_scenario <- census_final_2015_2019
# view(modify_census_for_best_scenario[modify_census_for_best_scenario$proppov >= 25,])
modify_census_for_best_scenario$propcov[modify_census_for_best_scenario$medhhinc <= 40000] <- 100
modify_census_for_best_scenario$proprent[modify_census_for_best_scenario$medhhinc <= 40000] <- 100
modify_census_for_best_scenario$proppov[modify_census_for_best_scenario$medhhinc <= 40000] <- 0

# view(modify_census_for_best_scenario)

medhhinc_target <- mean(modify_census_for_best_scenario$medhhinc)
propcov_target <- mean(modify_census_for_best_scenario$propcov)
proppov_target <- mean(modify_census_for_best_scenario$proppov)
proprent_target <- mean(modify_census_for_best_scenario$proprent)

attainment_rate_target <- predict(census_final_2015_2019.lm.all, newdata= list(medhhinc=medhhinc_target, propcov=propcov_target, proppov=proppov_target, proprent=proprent_target))
attainment_rate_target
attainment_rate_actual

#By increasing Median household income by 5.5%, Health insurance coverage by 7%, Renter-occupied by 5% and decreasing 
#poverty level by 0.1%, we have roughly achieved the targeted college degree attainment rate for Cook County



# ---
# Perform Step 6
#Using the Census Bureau API, pull the total population and college degree achievement levels
#('DP05_0001E','DP02_0065PE') for every tract in the United States (using the 2015-19 5-year ACS estimates). 
#Flag which tracts belong to Cook County, IL.
# ---

acs_var <- c('DP05_0001E','DP02_0065PE')
us_states <- c("AK","AL","AR","AZ","CA","CO","CT","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY",
               "LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY",
               "OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")
all_census_tidy_2015_2019 <- get_acs(
  geography = "tract", variables = acs_var, state = us_states, year = 2019, geometry = TRUE, survey = "acs5",
  output = "wide"
)

#Flag which tracts belong to Cook County, IL
all_census_tidy_2015_2019 <- all_census_tidy_2015_2019 %>%
  mutate(iscookcounty = case_when(
    str_detect(all_census_tidy_2015_2019$NAME, "Cook County, Illinois") ~ 1,
    TRUE ~ 0
  ))

#Drop the columns that are not required
all_census_2015_2019 <- all_census_tidy_2015_2019[,!(names(all_census_tidy_2015_2019) %in% c("DP05_0001M", "DP02_0065PM"))]

colnames(all_census_2015_2019)
head(all_census_2015_2019)


#Rename the remaining columns
all_census_2015_2019 <- all_census_2015_2019 %>%  
  rename('geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065PE', 'totpop' = 'DP05_0001E')


# 6.a Filter to tracts with non-missing population, non-missing college degree data, and
# population of at least 100.
all_census_final_2015_2019<- all_census_2015_2019[
  all_census_2015_2019$totpop >= 100 
  & !is.na(all_census_2015_2019$propbac)
  & !is.na(all_census_2015_2019$totpop),
]


# 6.b Calculate the national average for tract-level college degree attainment, using both an
#equal-weight average as well as weighting by population. For these calculations, exclude
#Cook County, IL.

all_census_exclude_cook_county_2015_2019 <- filter(all_census_final_2015_2019, iscookcounty == 0)

all_census_exclude_cook_county_2015_2019["totpop_percentage"] = all_census_exclude_cook_county_2015_2019$totpop/(sum(all_census_exclude_cook_county_2015_2019$totpop))
all_propbac_exclude_cook_county_mean <- mean(all_census_exclude_cook_county_2015_2019$propbac)
all_propbac_exclude_cook_county_weighted_mean <- sum(all_census_exclude_cook_county_2015_2019$totpop_percentage*all_census_exclude_cook_county_2015_2019$propbac)

all_propbac_exclude_cook_county_mean
all_propbac_exclude_cook_county_weighted_mean


# 6.c Perform a hypothesis test of whether the tracts from Cook County could share the same
#equal-weighted average college degree attainment as the national average excluding Cook
#County. Treat that national average as a known constant, not a random variable.

#H0 -> Cook County could share the same equal-weighted average college degree attainment as the 
#      national average excluding Cook County

#H1 -> Cook County doesn't share the same equal-weighted average college degree attainment as the 
#      national average excluding Cook County


all_census_only_cook_county_2015_2019 <- filter(all_census_final_2015_2019, iscookcounty == 1)

all_census_only_cook_county_2015_2019["totpop_percentage"] = all_census_only_cook_county_2015_2019$totpop/(sum(all_census_only_cook_county_2015_2019$totpop))
all_propbac_only_cook_county_mean <- mean(all_census_only_cook_county_2015_2019$propbac)
all_propbac_only_cook_county_weighted_mean <- sum(all_census_only_cook_county_2015_2019$totpop_percentage*all_census_only_cook_county_2015_2019$propbac)

all_propbac_only_cook_county_mean
all_propbac_only_cook_county_weighted_mean

# Performing t-test for hypothesis testing
mean(all_census_final_2015_2019$propbac[all_census_final_2015_2019$iscookcounty==1])
sd(all_census_final_2015_2019$propbac[all_census_final_2015_2019$iscookcounty==1])

#one sample t test comparing CookCounty propbac to national mean of propbac
t.test(x=all_census_final_2015_2019$propbac[all_census_final_2015_2019$iscookcounty==1], mu = all_propbac_exclude_cook_county_mean, alternative = "two.sided", paired = FALSE)


#Cook County equal-weighted average college degree attainment rate is '22.2978' greater than the national average
#of '19.4961'. Hence we are rejecting the NULL hypothesis (H0) meaning distrubutions are from different



# ---
# Perform Step 7
#Identify the tract containing the Gleacher Center and NBC Tower. Note that the 2015-2019 ACS 5-year 
#estimates use the 2010 Census tract boundaries. Restore your regression from the previous week that 
#predicted college degree attainment as a function of all the predictors downloaded in the first group assignment.
# ---

#Identify the tract containing the Gleacher Center and NBC Tower

#Step1 : Goto https://censusreporter.org/profiles
#Step2 : To get the tract containing the Gleacher Center and NBC Tower, search for term 'Census Tract 814.03, Cook, IL'

# 0814.03 as of 2022, GEO ID 17031081403
# uctract <- df[df$geoid == "17031081403",]
# uctract <- uctract[c("totpop","medage","medhhinc","propcov","proppov","proprent")]

# gleacher center and nbc tower lon and lat coords
# locations <- data.frame(longitude = c(-87.622240,-87.621150), latitude = c(41.889640,41.890100))
# Trying to plot
# ggplot(data = uctract) + 
#  geom_sf() +
#  geom_point(data = locations, aes(x = longitude, y = latitude), color="red") +
#  labs(title = "Tract: 813.03, Geo ID: 17031081403",
#      subtitle = "Gleacher Center and NBC Tower",
#      caption = "Data: 2015-2019 5-year ACS, US Census Bureau, Cook County, IL",
#      fill = "Percentage") + 
#  theme_minimal()

census_tract_814_03 <- filter(census_final_2015_2019, str_detect(census_final_2015_2019$name, "814.03"))

#Liner model with all predictors
census_final_2015_2019.lm.all <- lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019)
census_final_2015_2019.lm.all.summary <- summary(census_final_2015_2019.lm.all)
census_final_2015_2019.lm.all.summary


# 7.a What is the point estimate and 90% confidence interval for the predicted college degree
#attainment in this tract? Is the true college degree attainment for this tract contained in
#that interval?


census_tract_814_03_drop_columns <- c("geoid", "name", "geometry", "propbac")
census_tract_814_03_new <- census_tract_814_03[,!(names(census_tract_814_03) %in% census_tract_814_03_drop_columns)]
st_geometry(census_tract_814_03_new) <- NULL


# Predicts the future values
#A prediction interval captures the uncertainty around a single value. 
#A confidence interval captures the uncertainty around the mean predicted values. 
#Thus, a prediction interval will always be wider than a confidence interval for the same value.
census_tract_814_03_prediction_result <- predict(census_final_2015_2019.lm.all, 
                                                 newdata = census_tract_814_03_new,
                                                 interval = c("confidence"),
                                                 level = 0.90)

sprintf("The point estimate for the predicted college degree attainment in this tract is %s", 
        round(census_tract_814_03_prediction_result[,1], digits = 4))
sprintf("90 percentage confidence interval for the predicted college degree attainment in this tract is %s and %s", 
        round(census_tract_814_03_prediction_result[,2], digits = 4),
        round(census_tract_814_03_prediction_result[,3], digits = 4))
sprintf("The true college degree attainment for this tract is %s and its not contained in the 90 percentage confidence interval",
        round(census_tract_814_03$propbac, digits = 4))


# 7.b How does this point estimate and interval differ if you re-calculate the regression,
#weighting by population?


totpop_weights <- census_final_2015_2019$totpop/sum(census_final_2015_2019$totpop)

census_final_2015_2019.lm.all.weighted <- lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019, weights = totpop_weights)
census_final_2015_2019.lm.all.weighted.summary <- summary(census_final_2015_2019.lm.all.weighted)
census_final_2015_2019.lm.all.weighted.summary


census_tract_814_03_prediction_result_weighted <- predict(census_final_2015_2019.lm.all.weighted, 
                                                          newdata = census_tract_814_03_new,
                                                          interval = c("confidence"),
                                                          level = 0.90)

sprintf("The point estimate for the predicted college degree attainment after weighting by population for this tract 
 is %s and marginally lower than the previous value of %s", 
        round(census_tract_814_03_prediction_result_weighted[,1], digits = 4),
        round(census_tract_814_03_prediction_result[,1], digits = 4))

sprintf("90 percentage confidence interval for the predicted college degree attainment after weighting by population for this tract 
 is %s and %s and marginally smaller than the previous value of %s and %s",
        round(census_tract_814_03_prediction_result_weighted[,2], digits = 4),
        round(census_tract_814_03_prediction_result_weighted[,3], digits = 4),
        round(census_tract_814_03_prediction_result[,2], digits = 4),
        round(census_tract_814_03_prediction_result[,3], digits = 4))


# 7.c Using all of the betas and their standard errors estimated by your (unweighted)
#regression, simulate 10,000 sets of possible betas. Use those 10,000 sets of betas to
#calculate 10,000 predictions for the Gleacher/NBC tract. Compare a 90% interval formed
#from these simulations to the 90% CI produced in (a) above.

census_final_2015_2019.lm.all


# betas
int <- census_final_2015_2019.lm.all$coefficient[1]
b1 <- census_final_2015_2019.lm.all$coefficient[2]
b2 <- census_final_2015_2019.lm.all$coefficient[3]
b3 <- census_final_2015_2019.lm.all$coefficient[4]
b4 <- census_final_2015_2019.lm.all$coefficient[5]
b5 <- census_final_2015_2019.lm.all$coefficient[6]
b6 <- census_final_2015_2019.lm.all$coefficient[7]

# std errors
se.int <- census_final_2015_2019.lm.all.summary$coefficients[1,2]
se.b1 <- census_final_2015_2019.lm.all.summary$coefficients[2,2]
se.b2 <- census_final_2015_2019.lm.all.summary$coefficient[3,2]
se.b3 <- census_final_2015_2019.lm.all.summary$coefficient[4,2]
se.b4 <- census_final_2015_2019.lm.all.summary$coefficient[5,2]
se.b5 <- census_final_2015_2019.lm.all.summary$coefficient[6,2]
se.b6 <- census_final_2015_2019.lm.all.summary$coefficient[7,2]

# variables for GleacherNBCTract
var1.GleacherNBCTract <- census_tract_814_03$medhhinc
var2.GleacherNBCTract <- census_tract_814_03$propcov
var3.GleacherNBCTract <- census_tract_814_03$proppov
var4.GleacherNBCTract <- census_tract_814_03$proprent
var5.GleacherNBCTract <- census_tract_814_03$totpop
var6.GleacherNBCTract <- census_tract_814_03$medage

#set seed for samples
set.seed(20221111)
#make sample betas using beta +/- se
sample.df <- data.frame(matrix(ncol = 6, nrow = 10000))

sample.df[,1] <- runif(10000,(b1-se.b1),(b1+se.b1))
sample.df[,2] <- runif(10000,(b2-se.b2),(b2+se.b2))
sample.df[,3] <- runif(10000,(b3-se.b3),(b3+se.b3))
sample.df[,4] <- runif(10000,(b4-se.b4),(b4+se.b4))
sample.df[,5] <- runif(10000,(b5-se.b5),(b5+se.b5))
sample.df[,6] <- runif(10000,(b6-se.b6),(b6+se.b6))
#use samples to generate 10k predictions
sample.predictions <- int + (sample.df[,1]*var1.GleacherNBCTract) + (sample.df[,2]*var2.GleacherNBCTract) + (sample.df[,3]*var3.GleacherNBCTract) + (sample.df[,4]*var4.GleacherNBCTract) + (sample.df[,5]*var5.GleacherNBCTract) + (sample.df[,6]*var6.GleacherNBCTract)

#find 90 percent confidence
quantile(sample.predictions, probs = c(.05,.95))

# t.test(x=sample.predictions, mu = census_tract_814_03$propbac)

# ---
# Perform Step 8
#Prepare a brief (at most 1-page) memo to someone who has asked you to model college degree
#attainment using a linear model. Assume they have already seen the raw model output and
#that they know how to read it. Discuss your confidence in a new public school program
#encouraging college attendance, which will target the tracts with residuals in the lowest
#quartile, with reference to your findings in 6) and 7) above and anything else that seems
#relevant.
# ---

# Restore data and model results
head(census_final_2015_2019)
summary(census_final_2015_2019.lm.all)

# Check the actual, residuals and fitted values
head(census_final_2015_2019$propbac)
head(census_final_2015_2019.lm.all$residuals)
head(census_final_2015_2019.lm.all$fitted.values)

# Store residual in df
df_public_school_program <- census_final_2015_2019
df_public_school_program$model_residuals <- census_final_2015_2019.lm.all$residuals
df_public_school_program$propbac_pred <- census_final_2015_2019.lm.all$fitted.values

# Store residuals's lower quartile data in df
lower_quartile <- quantile(df_public_school_program$model_residuals, probs = 0.25)
df_public_school_program_residual_lower_quartile <- df_public_school_program[df_public_school_program$model_residuals <= lower_quartile,]

# Explore residuals's lower quartile data across variables vs actual data
summary(df_public_school_program_residual_lower_quartile)
summary(census_final_2015_2019)

# Store fitted value's lower quartile data in df
lower_quartile <- quantile(df_public_school_program$propbac_pred, probs = 0.25)
df_public_school_program_predicted_lower_quartile <- df_public_school_program[df_public_school_program$propbac_pred <= lower_quartile,]

# Explore fitted value's lower quartile data across variables vs actual data
summary(df_public_school_program_residual_lower_quartile)
summary(census_final_2015_2019)
#Liner model with all predictors
census_final_2015_2019.lm.all <- lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019)
census_final_2015_2019.lm.all.summary <- summary(census_final_2015_2019.lm.all)
census_final_2015_2019.lm.all.summary

#R^2 for each predictor variable
medhhincR2<-lm(propbac ~ medhhinc, data = census_final_2015_2019)
summary(medhhincR2)

propcovR2<-lm(propbac ~ propcov, data = census_final_2015_2019)
summary(propcovR2)

proppovR2<-lm(propbac ~ proppov, data = census_final_2015_2019)
summary(proppovR2)

proprentR2<-lm(propbac ~ proprent, data = census_final_2015_2019)
summary(proprentR2)

totpopR2<-lm(propbac ~ totpop, data = census_final_2015_2019)
summary(totpopR2)

medageR2<-lm(propbac ~ medage, data = census_final_2015_2019)
summary(medageR2)

#Finding which of the impactful predictors are correlated to each other
cor(census_final_2015_2019$medhhinc, census_final_2015_2019$propcov, method="pearson")
cor(census_final_2015_2019$propcov, census_final_2015_2019$proppov, method="pearson")
cor(census_final_2015_2019$medhhinc, census_final_2015_2019$proppov, method="pearson")


# ---
# Perform Step 9
#As a change from previous weeks, consider the tract-level proportion of households living
#below the poverty line ('DP03_0128PE'). Select ten numeric ACS variables which you think
#may be explanatory variables for tract-level poverty rates – note: do not select any variables
#which directly measure income.
# ---


acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE',
             'DP03_0096PE','DP03_0128PE','DP04_0047PE', "DP03_0136PE",
             "DP03_0074PE", "DP03_0075PE", "DP04_0045PE", "DP04_0063PE",
             "DP04_0065PE", "DP04_0093PE", "DP04_0126PE", "DP04_0057PE",
             "DP02_0153PE", "DP03_0024PE", "DP03_0021PE")
census_wide_2015_2019 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2019, geometry = TRUE, survey = "acs5", 
  output = "wide"
)

#Poverty Line Illinois
#Instead of the traditional 138 percent above the federal poverty line calculation, Illinois uses 
#200 percent above the federal poverty line. So a family of four that earns $4,367 a month or less 
#would qualify for benefits.
#https://www.irp.wisc.edu/resources/how-is-poverty-measured/

#Filter tract-level proportion of households living below the poverty line ('DP03_0128PE')
#census_wide_2015_2019_below_proverty <- filter(census_wide_2015_2019, DP03_0062E <= (2208*12))

#Mean poverty line w.r.t to poverty level income number (2208*12) and family size of 4 is 42%
census_wide_2015_2019_below_proverty <- filter(census_wide_2015_2019, DP03_0128PE >= 42)

# Rename the remaining columns
# DP02_0065PE  -> propbac               (Bachelor's degree)
# DP03_0062E   -> medhhinc              (Median household income)
# DP03_0096PE  -> propcov               (Health insurance coverage)
# DP03_0128PE  -> proppov               (POVERTY LEVEL)
# DP05_0001E   -> totpop                (Total population)
# DP05_0018E   -> medage                (Median age)
# DP03_0136PE  -> familysize            (People in families)
# DP03_0074PE  -> foodstamp             (Food Stamp/SNAP benefits)
# DP03_0075PE  -> addedbenefits         (INCOME AND BENEFITS)
# DP04_0045PE  -> housingtenure         (HOUSING TENURE!!Occupied housing units)
# DP04_0047PE  -> proprent              (Renter-occupied)
# DP04_0063PE  -> proputilgas           (Occupied housing units!!Utility gas)
# DP04_0065PE  -> proputilelectric      (Occupied housing units!!Electricity)
# DP04_0093PE  -> propmortgage          (Housing units with a mortgage)
# DP04_0126PE  -> proppayrent           (Occupied units paying rent)
# DP04_0057PE  -> propvehicles          (Occupied housing units!!VEHICLES AVAILABLE)
# DP02_0153PE  -> propinternet          (Total households!!With a broadband Internet subscription)
# DP03_0024PE  -> propwrkfromhome       (COMMUTING TO WORK!!Worked from home)
# DP03_0021PE  -> proppublictranstowrk  (COMMUTING TO WORK!!Public transportation)

census_final_2015_2019_below_proverty <- census_wide_2015_2019_below_proverty %>%  
  rename('geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065PE', 
         'medhhinc' = 'DP03_0062E', 'propcov' = 'DP03_0096PE', 'proppov' = 'DP03_0128PE', 
         'proprent' = 'DP04_0047PE', 'totpop' = 'DP05_0001E', 'medage' = 'DP05_0018E', 
         'familysize' = 'DP03_0136PE', 'foodstamp' = 'DP03_0074PE',
         'addedbenefits' = 'DP03_0075PE', 'housingtenure' = 'DP04_0045PE',
         'proputilgas' = 'DP04_0063PE', 'proputilelectric' = 'DP04_0065PE',
         'propmortgage' = 'DP04_0093PE', 'proppayrent' = 'DP04_0126PE',
         'propvehicles' = 'DP04_0057PE', 'propinternet' = 'DP02_0153PE',
         'propwrkfromhome' = 'DP03_0024PE', 'proppublictranstowrk' = 'DP03_0021PE')

# Drop the columns which report margin of error plus others
census_final_2015_2019_below_proverty_drop_columns <- c("DP02_0065PM", "DP03_0062M", "DP05_0001M", 
                                                        "DP05_0018M", "DP03_0096PM", "DP03_0128PM",
                                                        "DP04_0047PM", "DP03_0136PM", "DP03_0074PM",
                                                        "DP03_0075PM", "DP04_0045PM", "DP04_0063PM",
                                                        "DP04_0065PM", "DP04_0093PM", "DP04_0126PM",
                                                        "DP04_0057PM", "DP02_0153PM", "DP03_0024PM", 
                                                        "DP03_0021PM")
census_final_2015_2019_below_proverty <- census_final_2015_2019_below_proverty[,!(names(census_final_2015_2019_below_proverty) 
                                                                                  %in% census_final_2015_2019_below_proverty_drop_columns)]


#Ten numeric ACS variables which we think may be explanatory variables for tract-level poverty rates
#propcov,familysize,foodstamp,
#propmortgage,proppayrent,
#proputilgas,proputilelectric
#propinternet,propwrkfromhome,
#proppublictranstowrk

#list of available variables
str(census_final_2015_2019_below_proverty)
#linear regression with all numeric ACS variables except median income
all.lm <- lm(proppov~.-medhhinc-geoid-name,census_final_2015_2019_below_proverty)
summary(all.lm)
#logically familysize determines foodstamp and addedbenefits (the bigger the family the more aid they receive)
inter.lm <- lm(proppov~.-medhhinc-geoid-name+(familysize*foodstamp)+(familysize*addedbenefits),census_final_2015_2019_below_proverty)
summary(inter.lm)
#remove predictor variables that are not statistically significant (large p-values) to reduce model to include only 10 variables (R^2=87% for both models)
red1.lm <- lm(proppov~.-medhhinc-geoid-name-housingtenure-proppayrent-totpop-propwrkfromhome-propmortgage-propcov-proppublictranstowrk+(familysize*foodstamp)+(familysize*addedbenefits),census_final_2015_2019_below_proverty)
summary(red1.lm)

red2.lm <- lm(proppov~.-medhhinc-geoid-name-housingtenure-propvehicles-totpop-propwrkfromhome-propmortgage-propcov-proppublictranstowrk+(familysize*foodstamp)+(familysize*addedbenefits),census_final_2015_2019_below_proverty)
summary(red2.lm)

#Use the Kolmogorov-Smirnov test and the Breusch-Pagan test to examine the normality and the heteroskedasticity of the residuals
#Reduced model passes the Kolmogorov-Smirnov test but not the Breusch-Pagan test
hist(red1.lm$residuals)
ks.test(red1.lm$residuals/summary(red1.lm)$sigma,pnorm)

plot(red1.lm$fitted.values,red1.lm$residuals)
bptest(red1.lm)

#Reduced model 2 passes the Kolmogorov-Smirnov test and the Breusch-Pagan tests
hist(red2.lm$residuals)
ks.test(red2.lm$residuals/summary(red2.lm)$sigma,pnorm)

plot(red2.lm$fitted.values,red2.lm$residuals)
bptest(red2.lm)

#So, reduced model 2 is identified as the best model to explain poverty rates using 10 numeric ACS variables
best.lm <- lm(proppov~.-medhhinc-geoid-name-housingtenure-propvehicles-totpop-propwrkfromhome-propmortgage-propcov-proppublictranstowrk+(familysize*foodstamp)+(familysize*addedbenefits),census_final_2015_2019_below_proverty)
summary(best.lm)

# 9.a Check to make sure that all of your predictors are at least 90% non-missing.

# Remove the rows from dataframe that contains at least one NA
census_final_2015_2019_below_proverty <- na.omit(census_final_2015_2019_below_proverty)
st_geometry(census_final_2015_2019_below_proverty) <- NULL

# 9.b Select either AIC or Adjusted R^2 as a metric, and then choose either the best possible
#     model or a good model suggested by model selection algorithms.


# 9.b.1 Make upfront modeling choices
head(census_final_2015_2019_below_proverty)
census_final_2015_2019_below_proverty.naive.lm <- lm(proppov~propcov+familysize+foodstamp+
                                                       propmortgage+proppayrent+medage+
                                                       propinternet+propwrkfromhome+
                                                       proppublictranstowrk+proputilelectric+proputilgas, census_final_2015_2019_below_proverty)
summary(census_final_2015_2019_below_proverty.naive.lm)

# 9.b.2 Test IID assumptions

#Kolmogorov-Smirnov test for normality
hist(census_final_2015_2019_below_proverty.naive.lm$residuals)
ks.test(census_final_2015_2019_below_proverty.naive.lm$residuals/summary(census_final_2015_2019_below_proverty.naive.lm)$sigma, pnorm)

#Breusch-Pagan test for normality heteroscedasticity
plot(census_final_2015_2019_below_proverty.naive.lm$fitted.values,census_final_2015_2019_below_proverty.naive.lm$residuals)
bptest(census_final_2015_2019_below_proverty.naive.lm)

# 9.b.3 Identify best model based on Adj R^2
census_final_2015_2019_below_proverty_best_subset  <- 
  regsubsets(x = census_final_2015_2019_below_proverty[,c(-1:-3,-5,-6,-8,-9,-12,-13,-18)],y = census_final_2015_2019_below_proverty[,8])
census_final_2015_2019_below_proverty_summary_best_subset <- summary(census_final_2015_2019_below_proverty_best_subset)
as.data.frame(census_final_2015_2019_below_proverty_summary_best_subset$outmat)
census_final_2015_2019_below_proverty_summary_all_adjr2 <- census_final_2015_2019_below_proverty_summary_best_subset$adjr2
before_transform_number_of_predictors <- which.max(census_final_2015_2019_below_proverty_summary_all_adjr2)
census_final_2015_2019_below_proverty_summary_best_subset$which[before_transform_number_of_predictors,]

#Model Performance at Different Sizes
matplot(1:8,cbind(summary(census_final_2015_2019_below_proverty_best_subset)$rsq,summary(census_final_2015_2019_below_proverty_best_subset)$adjr2),type='l',
        main='Model Performance at Different Sizes',
        sub='Using Best Model At Each Size',
        xlab='# Predictors',ylab='')
legend(x='bottomright',legend=c('R-Squared','Adj R^2'),
       col=c(1,2),lty=c(1,2))

# 9.b.4 Identify best model based on forward selection
step(lm(proppov~1, census_final_2015_2019_below_proverty),scope=formula(census_final_2015_2019_below_proverty.naive.lm),direction='forward')

# 9.b.4 Identify best model based on backward elimination
step(census_final_2015_2019_below_proverty.naive.lm,direction='backward')

#best predictors are based the output from leaps package
census_final_2015_2019_below_proverty.lm.best <- lm(formula = proppov ~ 
                                                      familysize + propcov + medage + foodstamp +
                                                      proputilelectric + propwrkfromhome + proppublictranstowrk, 
                                                    data = census_final_2015_2019_below_proverty)
summary(census_final_2015_2019_below_proverty.lm.best)

# 9.b.5 Plot fitted versus residual
plot(fitted(census_final_2015_2019_below_proverty.lm.best), resid(census_final_2015_2019_below_proverty.lm.best), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


# 9.b.6 Test IID assumptions

#Kolmogorov-Smirnov test for normality
hist(census_final_2015_2019_below_proverty.lm.best$residuals)
ks.test(census_final_2015_2019_below_proverty.lm.best$residuals/summary(census_final_2015_2019_below_proverty.lm.best)$sigma, pnorm)

#Breusch-Pagan test for normality heteroscedasticity
plot(census_final_2015_2019_below_proverty.lm.best$fitted.values,census_final_2015_2019_below_proverty.lm.best$residuals)
bptest(census_final_2015_2019_below_proverty.lm.best)

#Diagnostic plots with multiple predictors
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019_below_proverty.lm.best)

# 9.c Save the RMSE (root mean square error, or ‘sigma’) for later.
RMSE_census_final_2015_2019_below_proverty <- summary(census_final_2015_2019_below_proverty.lm.best)$sigma


#---------------------------------------------------Q9::BEGIN SUMMARY--------------------------------------------------------#
#We feel below are the ten numeric ACS variables which we think may be explanatory variables for tract-level poverty rates


#propcov               (Health insurance coverage)
#familysize            (People in families)
#foodstamp             (Food Stamp/SNAP benefits)
#propmortgage          (Housing units with a mortgage)
#proppayrent           (Occupied units paying rent)
#propinternet          (Total households!!With a broadband Internet subscription)
#propwrkfromhome       (COMMUTING TO WORK!!Worked from home)
#proppublictranstowrk  (COMMUTING TO WORK!!Public transportation)
#proputilgas           (Occupied housing units!!Utility gas)
#proputilelectric      (Occupied housing units!!Electricity)

#As per Kolmogorov-Smirnov Test, the corresponding p-value is 0.2578 and is greater than .05, 
#we accept the NULL hypothesis and have sufficient evidence to say that the sample data does come from a normal distribution

#As per Breusch-Pagan Test, the residuals become much more spread out as the fitted values get larger. 
#This “cone” shape is a telltale sign of heteroscedasticity. The corresponding p-value is 0.0121. 
#Since the p-value is less than 0.05, we reject the null hypothesis and have sufficient evidence 
#to say that heteroscedasticity is present in the regression model.

#Then, we ran the model on Adjusted R^2 model selection algorithm and have identified model with Adj. R^2 
#value of 0.6831 as the good model. This model considers the predictors such as 'Health insurance coverage',
#'People in families','Utility Electricity','Median age',
#'Worked from home' as the key contributor in predicting tract-level household poverty rates


#---------------------------------------------------Q9::END SUMMARY--------------------------------------------------------#



# ---
# Perform Step 10
#Experiment with possible transformations. Consider at least one transformation to the
#response variable and at least one transformation of a predictor. Transformations can
#include logs or roots or powers of variables, but also functions of two different variables, or
#step functions derived from continuous predictors.
# ---

#By running the best model 'census_final_2015_2019_below_proverty.lm.best' summary, we notice 
#how the residual standard error is high: 3.757. Also the best model only explains 68% variability in the data.
#This may imply that we need to transform our dataset further or try different methods of transformation.


# 10.a.1 Make upfront modeling choices

#Maybe a transformations in the values might help us to improve the model. Here we will do different transformation
#for response variable 'POVERTY LEVEL' and the predictor variables 'Health insurance coverage',
#'Food Stamp/SNAP benefits', 'People in families', 'Worked from home'
#which has more explanatory power than other predictor variables in predicting the tract-level household poverty rates

invBoxCox <- function(x, lambda)
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)

boxcox <- boxcox(census_final_2015_2019_below_proverty.lm.best, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)

#find optimal lambda for Box-Cox transformation 
optimal_lambda_boxcox <- boxcox$x[which.max(boxcox$y)]

#Using the Box-Cox method, we see that λ=-0.25 is both in the confidence interval, and is extremely close to the maximum, 
#which suggests a transformation of the form

##(y^λ − 1)/λ = (y^0.2 − 1)/0.2

#Box-Cox transformations of response variable
census_final_2015_2019_below_proverty$boxcoxproppov <- ((census_final_2015_2019_below_proverty$proppov^optimal_lambda_boxcox - 1) / optimal_lambda_boxcox)

#Functions of two different variables transformations of predictor variables
census_final_2015_2019_below_proverty$proputiltot <- census_final_2015_2019_below_proverty$proputilelectric+census_final_2015_2019_below_proverty$proputilgas

#Polynomial transformations of predictor variables
#add the second order term we need to use the I() function in the model specification
census_final_2015_2019_below_proverty$polymedage <- I(census_final_2015_2019_below_proverty$medage ^ 2) + 
  I(census_final_2015_2019_below_proverty$medage ^ 3) + 
  I(census_final_2015_2019_below_proverty$medage ^ 4)
census_final_2015_2019_below_proverty$polyfoodstamp <- I(census_final_2015_2019_below_proverty$foodstamp ^ 2)
census_final_2015_2019_below_proverty$polypropcov <- I(census_final_2015_2019_below_proverty$propcov ^ 2) + 
  I(census_final_2015_2019_below_proverty$propcov ^ 3) + 
  I(census_final_2015_2019_below_proverty$propcov ^ 4)
census_final_2015_2019_below_proverty$polypropwrkfromhome <- I(census_final_2015_2019_below_proverty$propwrkfromhome ^ 2)
census_final_2015_2019_below_proverty$polyproputiltot <- I(census_final_2015_2019_below_proverty$proputiltot ^ 2)


# 10.b.1 Make upfront modeling choices
census_final_2015_2019_below_proverty_after_transform.naive.lm <- lm(formula = boxcoxproppov ~ 
                                                                       familysize + 
                                                                       medage + polymedage +
                                                                       foodstamp + polyfoodstamp +
                                                                       propcov + polypropcov +
                                                                       propwrkfromhome + polypropwrkfromhome +
                                                                       proputiltot + polyproputiltot, 
                                                                     data = census_final_2015_2019_below_proverty)
summary(census_final_2015_2019_below_proverty_after_transform.naive.lm)


#But does adding 'n' order terms make sense practically? 
#The following plot should gives hints as to why it doesn’t
ggplot(data = census_final_2015_2019_below_proverty, aes(x = medage, y = proppov)) +
  stat_smooth(method = "lm", se = FALSE, color = "green", formula = y ~ x) +
  stat_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x + I(x ^ 2)) +
  stat_smooth(method = "lm", se = FALSE, color = "orange", formula = y ~ x + I(x ^ 2)+ I(x ^ 3)) +
  stat_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x + I(x ^ 2)+ I(x ^ 3) + I(x ^ 4)) +
  geom_point(colour = "black", size = 4)


#Diagnostic plots with multiple predictors after log-transformation
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019_below_proverty_after_transform.naive.lm)


# 10.b.2 Test IID assumptions

#Kolmogorov-Smirnov test for normality
hist(census_final_2015_2019_below_proverty_after_transform.naive.lm$residuals)
ks.test(census_final_2015_2019_below_proverty_after_transform.naive.lm$residuals/summary(census_final_2015_2019_below_proverty_after_transform.naive.lm)$sigma, pnorm)

#Breusch-Pagan test for normality heteroscedasticity
plot(census_final_2015_2019_below_proverty_after_transform.naive.lm$fitted.values,census_final_2015_2019_below_proverty_after_transform.naive.lm$residuals)
bptest(census_final_2015_2019_below_proverty_after_transform.naive.lm)


# 10.b.2 Identify best model based on backward elimination
step(census_final_2015_2019_below_proverty_after_transform.naive.lm,direction='backward')

#best predictors are based the output from leaps package
census_final_2015_2019_below_proverty_after_transform.lm.best <- lm(formula = boxcoxproppov ~ 
                                                      familysize + medage + polymedage + 
                                                      foodstamp + polyfoodstamp + 
                                                      propcov + proputiltot + polyproputiltot, 
                                                    data = census_final_2015_2019_below_proverty)
summary(census_final_2015_2019_below_proverty_after_transform.lm.best)

# 10.b.4 Plot fitted versus residual
plot(fitted(census_final_2015_2019_below_proverty_after_transform.lm.best), resid(census_final_2015_2019_below_proverty_after_transform.lm.best), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#Looking at a fitted versus residuals plot verifies that there likely are not any issues with the assumptions of this model, 
#which Breusch-Pagan and Kolmogorov-Smirnov tests verify.

# 10.b.5 Test IID assumptions
hist(census_final_2015_2019_below_proverty_after_transform.lm.best$residuals)
ks.test(census_final_2015_2019_below_proverty_after_transform.lm.best$residuals/summary(census_final_2015_2019_below_proverty_after_transform.lm.best)$sigma, pnorm)

plot(census_final_2015_2019_below_proverty_after_transform.lm.best$fitted.values,census_final_2015_2019_below_proverty_after_transform.lm.best$residuals)
bptest(census_final_2015_2019_below_proverty_after_transform.lm.best)

#Diagnostic plots with multiple predictors
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019_below_proverty_after_transform.lm.best)

# 10.b.6 ‘un-transform’ your predictions, manually compute RMSE, and compare that to your first model.
census_final_2015_2019_below_proverty_after_transform_predicted_y <- predict(census_final_2015_2019_below_proverty_after_transform.lm.best, 
                                newdata= census_final_2015_2019_below_proverty)

census_final_2015_2019_below_proverty_after_transform_predicted_y_untransform <- invBoxCox(census_final_2015_2019_below_proverty_after_transform_predicted_y, optimal_lambda_boxcox)
census_final_2015_2019_below_proverty_after_transform_predicted_y_residuals <- census_final_2015_2019_below_proverty$proppov - census_final_2015_2019_below_proverty_after_transform_predicted_y_untransform
RMSE_census_final_2015_2019_below_proverty_after_transform <- sqrt(mean(census_final_2015_2019_below_proverty_after_transform_predicted_y_residuals^2))


#---------------------------------------------------Q10::BEGIN SUMMARY--------------------------------------------------------#

#First, we have to assess the base line model with un-transformed response variable and selected predictors. 
#Here the response variable is 'Provert Level' and selected predictors are 'People in families', 'Median age', 
#'Health insurance coverage', 'Food Stamp/SNAP benefits', 'Electricity', 'Worked from home', 'Public transportation to work'.
#Then we used AIC backward model selection algorithm to choose the good model. The model explains 68.75% variability in the data
#and has residual error of 3.906

#Then we have evaluated the IID assumptions:

#As per Kolmogorov-Smirnov test, the corresponding p-value is 0.5789 and is greater than .05, we accept the NULL 
#hypothesis and have sufficient evidence to say that the sample data does come from a normal distribution and residuals 
#are somewhat normally distributed

#As per Breusch-Pagan Test, the residuals become much more spread out as the fitted values get larger. 
#This “cone” shape is a telltale sign of heteroscedasticity. The corresponding p-value is 0.005369. 
#Since the p-value is less than 0.05, we reject the null hypothesis and have sufficient evidence 
#to say that severe heteroscedasticity is present in the regression model.

#Then we took the RMSE (root mean square error, or ‘sigma’) of the base line model and it's 3.7572


#To alleviate these issues we perform transformation to the predictors and response variable from the same dataset. 
#We did the following steps of transformation to base line model to improve the IID assumptions, 
#model explainability by increasing adjusted R^2 and reduce RMSE (root mean square error).

#1) Applied Box-Cox transformation on response variable so that the data closely resembles a normal distribution.
#   This assumption allows us to construct confidence intervals and conduct hypothesis tests
#2) Applied Polynomial transformation on predictor variables. They are extremely useful as they allow for more flexible models, 
#   but do not change the units of the variables like the way units changed when using logarithmic transformations.
#   Polynomial Regression models are usually fit with the method of least squares.The least square method minimizes the 
#   variance of the coefficients, under the Gauss Markov Theorem. Polynomial Regression does not require the relationship 
#   between the independent and dependent variables to be linear in the data set. The relationship between the dependent 
#   variable and any independent variable is linear or curvilinear (specifically polynomial)
#3) functions of two different variables


#After apply transformations, then we used AIC backward model selection algorithm to choose the good model. 
#The model explains 80.23% variability in the data and has decreased the residual error to 0.02088

#Then we have evaluated the IID assumptions:

#As per Kolmogorov-Smirnov test, the corresponding p-value is 0.7916 and is greater than .05, we accept the NULL 
#hypothesis and have sufficient evidence to say that the sample data does come from a normal distribution and residuals 
#are much more normally distributed than the base line model which had p-value of 0.5789

#As per Breusch-Pagan Test, the residuals are much less spread out as the fitted values get larger. 
#The presence of heteroscedasticity drastically got reduced and the corresponding p-value is 0.253 which is much lower 
#compared to base line model p-value of 0.005369. 
#Since the p-value is greater than 0.05, we accept the null hypothesis and have sufficient evidence 
#to say that heteroscedasticity is minimized in the regression model.


#Then we 'un-transformed' the Box-Cox transformed response variable before manually computing the 
#RMSE of the improved model and the value turns to be 2.7599 which much lower than the starting model's
#RMSE value 3.7572


#---------------------------------------------------Q10::END SUMMARY--------------------------------------------------------#

