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
census_final_2015_2019.lm.summary <- summary(census_final_2015_2019.lm)
census_final_2015_2019.lm.summary

#Diagnostic plots with single predictor
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019.lm)

#Plotting regression line from actual median household income versus baccalaureate attainment rate 
ggplot(census_final_2015_2019, aes(x=c(medhhinc), y=propbac)) +
  geom_point(color='steelblue',) +
  geom_smooth(method='lm', formula= y~x, se=FALSE, color='turquoise4')  +
  stat_regline_equation(label.y = 68, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 66, aes(label = ..rr.label..)) +
  theme_minimal() +
  labs(x='Median Household Income ($)', 
       y='Baccalaureate Attainment Rate (%)', 
       title='Baccalaureate Attainment Rate vs Median Household Income') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 


#Regression line with all predictors
census_final_2015_2019.lm.all <- lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019)
census_final_2015_2019.lm.all.summary <- summary(census_final_2015_2019.lm.all)
census_final_2015_2019.lm.all.summary

# diagnostic plots with all predictor
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019.lm.all)


# 2.b Perform an ANOVA-based F test to determine whether the difference in explanatory
#   power between these two models is significant.

#Let’s use the anova() function to compare these models and see which one provides the best parsimonious fit of the data.
#This ANVOA will test whether or not including multiple predictor variables propcov,proppov,proprent,totpop,medage 
#(both models use medhhinc) leads to a significant improvement over using just single predictor variable 'medhhinc'

#ANOVA uses the following null and alternative hypotheses:
  
#H0: Single predictor is enough to predict the baccalaureate attainment rate (propbac)
#HA: Single predictor is not enough to predict the baccalaureate attainment rate (propbac)

#Compare model with single predictor to model with all predictor
census_final_2015_2019_anova <- anova(census_final_2015_2019.lm, census_final_2015_2019.lm.all)
census_final_2015_2019_anova
summary(census_final_2015_2019_anova)


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
         title="Empirical Densities of the Residuals (Single versus Multiple Predictor Model)",
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
    theme_minimal()

    
# ---
# Perform Step 3
# In one to two paragraphs, summarize the difference in performance between the two models and make a recommendation 
# of which to use when deciding future policy actions
# ---

#Mean Square Error(MSE)/Root Mean Square Error(RMSE)
RMSE_model_1 <- sqrt(mean(census_final_2015_2019.lm.summary$residuals^2))
sprintf("Root Mean Square Error(RMSE) for Model 1 : %s", round(RMSE_model_1, digits = 4))
RMSE_model_2 <- sqrt(mean(census_final_2015_2019.lm.all.summary$residuals^2))
sprintf("Root Mean Square Error(RMSE) for Model 2 : %s", round(RMSE_model_2, digits = 4))

#Mean Absolute Error(MAE)
MAE_model_1 <- mean(abs(census_final_2015_2019.lm.summary$residuals))
sprintf("Mean Absolute Error(MAE) for Model 1 : %s", round(MAE_model_1, digits = 4))
MAE_model_2 <- mean(abs(census_final_2015_2019.lm.all.summary$residuals))
sprintf("Mean Absolute Error(MAE) for Model 2 : %s", round(MAE_model_2, digits = 4))


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

# 4.a Predictors with no significant explanatory power

summary(lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019))
summary(lm(propbac ~ medhhinc+propcov+proppov+proprent, data = census_final_2015_2019))
anova(lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019))

#For coefficient to be statistically significant, we usually want a P-value of less than 0.05. Here totpop (Total population) 
#and medage (Median age) has P-value greater than 0.05 and hence has no significant explanatory power.
#Besides without the predictors 'Total population' and 'Median age' added to the model, the R-squared and Adjusted R-squared are
#negligible affected


# 4.b Predictors with explanatory power, useful as control variables, but without a policy “lever” that 
#decision makers could use to increase college degree attainment


#Predictors like medhhinc (Median household income) has statistically significant explanatory power because it 
#explains 53% of the baccalaureate attainment rate in the model.


# 4.c Predictors with both explanatory power and a corresponding policy “lever”

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

#Adjusting the predictor variable values to desired amount to get the 5% increase in 
#college degree attainment rate for Cook County

medhhinc_target <- medhhinc_mean +  (medhhinc_mean*5.5)/100
propcov_target <- propcov_mean +  (propcov_mean*7)/100
proppov_target <- proppov_mean - (proppov_mean*0.1)/100
proprent_target <- proprent_mean +  (proprent_mean*5)/100

attainment_rate_target <- predict(census_final_2015_2019.lm.all, newdata= list(medhhinc=medhhinc_target, propcov=propcov_target, proppov=proppov_target, proprent=proprent_target))

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


# 6.a Filter to tracts with non-missing population, non-missing college degree data, and
#population of at least 100.


# Remove the rows from dataframe that contains at least one NA
all_census_tidy_2015_2019_test <- all_census_tidy_2015_2019[!(is.na(all_census_tidy_2015_2019$DP02_0065PE) | is.na(all_census_tidy_2015_2019$DP05_0001E)), ]
all_census_tidy_2015_2019 <- na.omit(all_census_tidy_2015_2019)

#Rename the remaining columns
all_census_2015_2019 <- all_census_tidy_2015_2019 %>%  
  rename('geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065PE', 'totpop' = 'DP05_0001E')

#Drop the columns that are not required
all_census_2015_2019 <- all_census_2015_2019[,!(names(all_census_2015_2019) %in% c("DP05_0001M", "DP02_0065PM"))]

#Filter to population of at least 100
all_census_final_2015_2019 <- filter(all_census_2015_2019, totpop >= 100)

#Remove the rows from dataframe that contains at least one NA
all_census_final_2015_2019 <- na.omit(all_census_final_2015_2019)



# 6.b Calculate the national average for tract-level college degree attainment, using both an
#equal-weight average as well as weighting by population. For these calculations, exclude
#Cook County, IL.

#Exclude- Cook County, IL.
all_census_exclude_cook_county_2015_2019 <- filter(all_census_final_2015_2019, iscookcounty == 0)

all_census_exclude_cook_county_2015_2019["totpop_percentage"] = all_census_exclude_cook_county_2015_2019$totpop/(sum(all_census_exclude_cook_county_2015_2019$totpop))
all_propbac_exclude_cook_county_mean <- mean(all_census_exclude_cook_county_2015_2019$propbac)
all_propbac_exclude_cook_county_weighted_mean <- sum(all_census_exclude_cook_county_2015_2019$totpop_percentage*all_census_exclude_cook_county_2015_2019$propbac)


sprintf("The national average for tract-level college degree attainment using both an equal-weight average as well as weighting by population exclude
Cook County, IL is %s", round(all_propbac_exclude_cook_county_weighted_mean, digits = 4))


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

sprintf("The Cook County average for tract-level college degree attainment using equal-weight average by population 
        is %s", round(all_propbac_only_cook_county_weighted_mean, digits = 4))


#Cook County equal-weighted average college degree attainment rate is '22.2978' greater than the national average
#of '19.4961'. Hence we are rejecting the NULL hypothesis (H0)



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


#TODO

# ---
# Perform Step 8
#Prepare a brief (at most 1-page) memo to someone who has asked you to model college degree
#attainment using a linear model. Assume they have already seen the raw model output and
#that they know how to read it. Discuss your confidence in a new public school program
#encouraging college attendance, which will target the tracts with residuals in the lowest
#quartile, with reference to your findings in 6) and 7) above and anything else that seems
#relevant.
# ---

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


# 9.a Check to make sure that all of your predictors are at least 90% non-missing.

# Remove the rows from dataframe that contains at least one NA
census_final_2015_2019_below_proverty <- na.omit(census_final_2015_2019_below_proverty)
st_geometry(census_final_2015_2019_below_proverty) <- NULL

# 9.b Select either AIC or Adjusted R^2 as a metric, and then choose either the best possible
#     model or a good model suggested by model selection algorithms.


# 9.b.1 Make upfront modeling choices
head(census_final_2015_2019_below_proverty)
census_final_2015_2019_below_proverty.naive.lm <- lm(proppov~propcov+familysize+foodstamp+
                                                     propmortgage+proppayrent+
                                                     propinternet+propwrkfromhome+
                                                     proppublictranstowrk+proputilelectric+proputilgas, census_final_2015_2019_below_proverty)
summary(census_final_2015_2019_below_proverty.naive.lm)

# 9.b.2 Test IID assumptions
hist(census_final_2015_2019_below_proverty.naive.lm$residuals)
ks.test(census_final_2015_2019_below_proverty.naive.lm$residuals/summary(census_final_2015_2019_below_proverty.naive.lm)$sigma, pnorm)

plot(census_final_2015_2019_below_proverty.naive.lm$fitted.values,census_final_2015_2019_below_proverty.naive.lm$residuals)
bptest(census_final_2015_2019_below_proverty.naive.lm)

# 9.b.3 Identify best model based on Adj R^2
census_final_2015_2019_below_proverty_best_subset  <- 
  regsubsets(x = census_final_2015_2019_below_proverty[,c(-1:-5,-6,-8,-9,-12,-13,-18)],y = census_final_2015_2019_below_proverty[,8])
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
                                                 familysize + propcov + 
                                                 propmortgage + proputilelectric +
                                                 propwrkfromhome, 
                                               data = census_final_2015_2019_below_proverty)
summary(census_final_2015_2019_below_proverty.lm.best)

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

#As per Kolmogorov-Smirnov Test, the corresponding p-value is 0.3294 and is greater than .05, 
#we accept the NULL hypothesis and have sufficient evidence to say that the sample data does come from a normal distribution

#As per Breusch-Pagan Test, the residuals become much more spread out as the fitted values get larger. 
#This “cone” shape is a telltale sign of heteroscedasticity. The corresponding p-value is 0.0093. 
#Since the p-value is less than 0.05, we reject the null hypothesis and have sufficient evidence 
#to say that heteroscedasticity is present in the regression model.

#Then, we ran the model on Adjusted R^2 model selection algorithm and have identified model with Adj. R^2 
#value of 0.6622 as the good model. This model considers the predictors such as 'Median household income', 
#'Health insurance coverage','People in families','Income and Benefits','Housing units with a mortgage',
#'Worked from home' as the key contributor in predicting tract-level household poverty rates


#---------------------------------------------------Q9::END SUMMARY--------------------------------------------------------#



# ---
# Perform Step 10
#Experiment with possible transformations. Consider at least one transformation to the
#response variable and at least one transformation of a predictor. Transformations can
#include logs or roots or powers of variables, but also functions of two different variables, or
#step functions derived from continuous predictors.
# ---

#By running the best model 'census_final_2015_2019_below_proverty.lm' summary, we notice 
#how the residual standard error is high: 3.78. Also the best model only explains 68% variability in the data.
#This may imply that we need to transform our dataset further or try different methods of transformation.

#Maybe a log-transformation in the values might help us to improve the model. Here we will do logs transformation
#for response variable 'POVERTY LEVEL' and the predictor variables 'Median household income', 'Health insurance coverage',
#'INCOME AND BENEFITS' which has more explanatory power than other predictor variables in predicting thetract-level household poverty rates

census_final_2015_2019_below_proverty$logproppov <- log1p(census_final_2015_2019_below_proverty$proppov)
census_final_2015_2019_below_proverty$logpropcov <- log1p(census_final_2015_2019_below_proverty$propcov)
census_final_2015_2019_below_proverty$logpropmortgage <- log1p(census_final_2015_2019_below_proverty$propmortgage)
census_final_2015_2019_below_proverty$logproputilelectric <- log1p(census_final_2015_2019_below_proverty$proputilelectric)
census_final_2015_2019_below_proverty$logpropwrkfromhome <- log1p(census_final_2015_2019_below_proverty$propwrkfromhome)
