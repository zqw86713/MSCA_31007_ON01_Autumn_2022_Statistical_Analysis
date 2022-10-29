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
#library(scatterplot3d) #Plots a three dimensional (3D) point cloud
#library(rgl)
#library(ggiraph)
#library(ggiraphExtra)
#library(plyr)

# Enter Census API Key
census_api_key("0c4a2a2815a8d526966f2490024ef157e19478db", overwrite = TRUE, install = TRUE)


# ---
# Perform Step 1
#Restore the dataset on Cook County census tracts and the model you used in the previous group assignment 
#which linked household income with college degree attainment.


# a) Define specific ACS variables we need to pull
# And, bring in tract-level data from the 2015-2019 American Community Survey (ACS) 5-year estimates for Cook County, IL
# 5-year ACS with the argument survey = "acs5", starting from 2015 till 2019
acs_var <- c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE')
census_tidy_2015_2019 <- get_acs(
  geography = "tract", variables = acs_var, county = "Cook", state = "IL", year = 2019, geometry = TRUE, survey = "acs5"
)

# b) Drop the columns which report margin of error
census_tidy_dropcols_2015_2019 <- census_tidy_2015_2019[,!(names(census_tidy_2015_2019) %in% "moe")]
# Format your output as a ‘wide’ table, not a ‘tidy’ table
census_wide_2015_2019 <- census_tidy_dropcols_2015_2019 %>% 
  pivot_wider(names_from = 'variable', values_from = c('estimate'))

# c) Rename the remaining columns
# DP02_0065P -> propbac  (Bachelor's degree)
# DP03_0062  -> medhhinc (Median household income)
# DP03_0096P -> propcov  (Health insurance coverage)
# DP03_0128P -> proppov  (PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL)
# DP04_0047P -> proprent (Renter-occupied)
# DP05_0001  -> totpop   (Total population)
# DP05_0018  -> medage   (Median age)
census_wide_final_2015_2019 <- census_wide_2015_2019 %>%  
  rename('geoid' = 'GEOID', 'name' = 'NAME', 'propbac' = 'DP02_0065P', 
         'medhhinc' = 'DP03_0062', 'propcov' = 'DP03_0096P', 'proppov' = 'DP03_0128P', 
         'proprent' = 'DP04_0047P', 'totpop' = 'DP05_0001', 'medage' = 'DP05_0018')


ggplot(data = census_wide_final_2015_2019, aes(fill = propbac)) +
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



# ---
# Perform Step 2
#Create a new regression which uses income as well as all the remaining variables (excluding geography) 
#to predict college degree attainment levels.
# ---

# Drop the columns that contains geography
drop_columns <- c("geoid")
census_final_2015_2019 <- census_wide_final_2015_2019[,!(names(census_wide_final_2015_2019) %in% drop_columns)]
st_geometry(census_final_2015_2019) <- NULL

# Remove the rows from dataframe that contains at least one NA
census_final_2015_2019 <- na.omit(census_final_2015_2019)


#a) What is the change in R^2 between the model with a single predictor and the model with
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


#scatter3d(propbac ~ medhhinc + propcov, data=census_final_2015_2019)


#Regression line with all predictors
census_final_2015_2019.lm.all <- lm(propbac ~ medhhinc+propcov+proppov+proprent+totpop+medage, data = census_final_2015_2019)
#census_final_2015_2019.lm.all <- lm(propbac ~ medhhinc*propcov*proppov*proprent*totpop*medage, data = census_final_2015_2019)
census_final_2015_2019.lm.all.summary <- summary(census_final_2015_2019.lm.all)
census_final_2015_2019.lm.all.summary

# diagnostic plots with all predictor
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(census_final_2015_2019.lm.all)


#b) Perform an ANOVA-based F test to determine whether the difference in explanatory
#   power between these two models is significant.

anova(census_final_2015_2019.lm, census_final_2015_2019.lm.all)


#c) Plot the empirical densities of the residuals from these two models, with both
#   distributions appearing on the same graph. Make the graph as close to publication-ready
#   as you can. Be prepared to discuss whether the full model has not just added explanatory
#   power, but improved the fit with OLS (Ordinary Least Squares) model assumptions.

plot(ecdf(census_final_2015_2019.lm[['residuals']]))
plot(ecdf(census_final_2015_2019.lm.all[['residuals']]))

