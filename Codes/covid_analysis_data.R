# Clear memory
rm(list=ls())

# Packeges to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
install.packages("lspline")
library(lspline)
# Estimate robust SE
install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
install.packages("texreg")
library(texreg)
# For different themes
install.packages("ggthemes")
library(ggthemes)
library(psych)

# Call the cleaned data from github
my_url <- "https://raw.githubusercontent.com/XinqiW/DA2_and_Coding1_Assign_COVID19/main/Data/Clean/covid_pop_10_13_2020_clean.csv"
df <- read.csv(my_url)


# (b). My potential Y variable is "Number of registered death", and potential X variable is "Number of registered case"

# (c). Check all variables: with the help of histograms, summary statistics and checking extreme
# values, and make a conscious decision on which observation(s) to drop.


df %>%
  select(-one_of(c('recovered', 'active'))) %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() 

df %>%
  select(-one_of(c('recovered', 'active'))) %>% 
  summary()

# check extrame values using scatter plot
ggplot(df, aes(x=confirmed, y=death)) +
  geom_point(size=3, shape=23) +
  annotate("text", x = 7860634, y = 216074, label = "USA", size=5, color= 'red') +
  annotate("text", x = 7239389, y = 110586, label = "India", size=5, color= 'orange') +
  annotate("text", x = 5113628, y = 150998, label = "Brazil", size=5, color= 'red') +
  annotate("text", x = 825340, y = 84420, label = "Mexico", size=5, color= 'orange')

# Check extrame values in confirmed

df %>% filter( confirmed > 800000 )
df %>% filter( confirmed < 10000)

# It seems that the variance is there only because of country size, no errors. 
# Since the purpose of my analysis is to find the relationship between confirmed cases and death cases from countries,
# and due to my limited sample size, I have decided not to drop any data.

# (d). No per capita data required, so skipped this step

# (e). population is already scaled in ten-thousands in the data cleaning step. No further steps required here.

# (f). Check and report your distributions for y and x variables: use histograms and summary statistics
# table (mean, median, min, max, standard deviation)

### Summary statistics table combined:
confirmed_sum_stat <- data.frame(summarise(df,
                                variable_name = 'confirmed',
                                mean = mean(confirmed),
                                median = median(confirmed),
                                std = sd(confirmed),
                                min = min(confirmed),
                                max = max(confirmed)))
                          
active_sum_stat <- data.frame(summarise(df,
                                variable_name = 'active',
                                mean = mean(active,na.rm = TRUE),
                                median = median(active,na.rm = TRUE),
                                std = sd(active,na.rm = TRUE),
                                min = min(active,na.rm = TRUE),
                                max = max(active,na.rm = TRUE)))

death_sum_stat <- data.frame(summarise(df,
                                variable_name = 'death',
                                mean = mean(death),
                                median = median(death),
                                std = sd(death),
                                min = min(death),
                                max = max(death)))

population_sum_stat <- data.frame(summarise(df,
                                variable_name = 'population',
                                mean = mean(population),
                                median = median(population),
                                std = sd(population),
                                min = min(population),
                                max = max(population)))

recovered_sum_stat <- data.frame(summarise(df,
                                 variable_name = 'recovered',
                                 mean = mean(recovered),
                                 median = median(recovered),
                                 std = sd(recovered),
                                 min = min(recovered),
                                 max = max(recovered)))

all_sum_stat <- confirmed_sum_stat %>% full_join(active_sum_stat) %>% full_join(death_sum_stat) %>% 
 full_join(population_sum_stat) %>% full_join(recovered_sum_stat)

### Histograms:


##################?????????????????????????????????????????????????????????????????????????????????????????????


# (g). Check the possible different ln transformation for the variables with plotting different scatter-
# plots with lo(w)ess. Make a substantive and statistical reasoning, where and when to use
# ln transformation. You do not need to fit any model here, only use statistical reasoning based on
# the graphs.
# i. Take care when it is possible to make ln transformation: you may need to drop or change
# some variables.


# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log

# 1) a). confirmed - death: level-level model without scaling
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point(color='orange') +
  geom_smooth(method="loess", color = 'dodgerblue3')+
  labs(x = "confirmed cases",y = "death cases") 


# b). confirmed - death: change the scale for confirmed cases for checking log-transformation (level-log model)
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point(color='orange') +
  geom_smooth(method="loess", color = 'dodgerblue3')+
  labs(x = "confirmed cases, ln scale",y = "death case") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1000,10000,80000,900000,5000000) )

# c). confirmed - death: change the scale for death cases for checking log-transformation (log-level model)
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point(color='orange') +
  geom_smooth(method="loess", color = 'dodgerblue3')+
  labs(x = "confirmed cases, ln scale",y = "death case") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,20,50,200,1000,10000,80000) )

# d). confirmed - death: change the scale for confirmed cases and death cases for checking log-transformation (log-log model)
ggplot( df , aes(x = confirmed, y = death ))  +
  geom_point(color='orange') +
  geom_smooth(method="loess", color = 'dodgerblue3')+
  labs(x = "confirmed cases, ln scale",y = "death case, ln scale") +
  scale_x_continuous( trans = log_trans() , breaks = c(1000,10000,80000,900000,5000000) )+
  scale_y_continuous( trans = log_trans(), breaks = c(1,20,50,200,1000,10000,80000) )

####
# Conclusions:
#   1) using only confirmed and death is possible, but we need to model the non-linearity in data
#       - Substantive: Level changes is harder to interpret and our aim is not to get absolute based comparison
#       - Statistical: log transformation is a better approximation to make simplification
#   2) taking log of confirmed cases and death cases is making the association closer to linear
#   3) taking both log of confirmed cases and death cases, because both of them are likely affected in multiplicative ways
#   - Substantive: it gives a better interpretation with percentage increases or decreases in confirmed and death cases 
#   - Statistical: it makes sense to take log as variables have skewed distribution with long right tail. Since the 
#     distribustions of confirmed and death cases are skewed with a long right tail, it makes sense to take the natural
#     logs of confirmed and death cases, the distributions are close to symmetric.


# drop.cols <- c('recovered', 'active', 'population')
# df %>% select(-one_of(drop.cols))

# Filter out countries with 0 death in order to take log of death:
df<- df %>% filter(!death==0)

# Take Log of confirmed and death
df <- df %>% mutate( ln_confirmed = log( confirmed ),
                     ln_death = log( death )) 



# (h). Choose your specification for the ln transformation and estimate the following models with graphical visualizations:
#  i. Simple linear regression
#  ii. Quadratic (linear) regression
#  iii. Piecewise linear spline regression
#  iv. Weighted linear regression, using population as weights.

######
# Make some models:
#    i. Simple linear regression:
#        reg1: ln_death = alpha + beta * ln_confirmed
#    ii. Quadratic (linear) regression: 
#        reg2: ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2
#    iii. Piecewise linear spline regression: 
#        reg3: ln_death = alpha + beta_1 * ln_confirmed * 1(confirmed < 15000) + beta_2 * ln_confirmed * 1(confirmed >= 15000)
#    iv. Weighted linear regression, using population as weights: 
#        reg4: ln_death = alpha + beta * ln_confirmed, weights: population

## Preperation: 
# 1) Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_confirmed_sq = ln_confirmed^2,
                     ln_confirmed_cb = ln_confirmed^3)
                     
# First model: Simple linear regression (log-log):
reg1 <- lm_robust( ln_death ~ ln_confirmed , data = df , se_type = "HC2" )
reg1
# Summary statistics
summary( reg1 )
# Visual inspection:
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='orange') +
  geom_smooth( method = lm , color = 'dodgerblue3' )


# Second model: Quadratic (linear) regression (reg2):
reg2 <- lm_robust( ln_death ~ ln_confirmed + ln_confirmed_sq , data = df )
summary( reg2 )
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='orange') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'dodgerblue3' )


# Third model: Piecewise linear spline regression:
# 1st: define the cutoff for confirmed
cutoff <- 15000
# 2nd: log transformation cutoff
cutoff_ln<- log( cutoff )
# Use simple regression with the lspline function
reg3 <- lm_robust(ln_death ~ lspline( ln_confirmed , cutoff_ln ), data = df )
summary( reg3 )
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='orange') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'dodgerblue3' )


# Fourth model: Weighted-OLS: use reg1 setup and weight with population
reg4 <- lm_robust(ln_death ~ ln_confirmed, data = df , weights = population)
summary( reg4 )

ggplot(data = df, aes(x = ln_confirmed, y = ln_death)) +
  geom_point(data = df, aes(size=population),  color = 'orange', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='dodgerblue3')+
  scale_size(range = c(1, 15)) +
  coord_cartesian(ylim = c(0, 15)) +
  labs(x = "ln(Confirmed) ",y = "ln(Death)")+
  annotate("text", x = 11.417053, y = 8.4635814, label = "China", size=5)+
  annotate("text", x = 15.795047,  y = 11.6135488, label = "India", size=5)




## (i) Compare the models and choose your preferred one
# i. Use substantive and statistical reasoning for your chosen model.
# ii. Show the model results in the report along with the graph.
# iii. Report the model comparison (all the estimated model results) in the appendix of your report.

#####
# Creating model summary with texreg
data_out <- "/Users/xinqi/Desktop/Data Analysis 2/COVID Assignment/Output/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 ),
         type = 'html',
         custom.model.names = c("ln_confirmed - linear","ln_confirmed - quadratic",
                                "ln_confirmed - PLS", "ln_confirmed - weighted linear"),
         caption = "Modelling COVID-19 Death Cases and Confirmed Cases of Countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)


# reg1: Simple linear regression
# The slope of this regression is 1.03, means that death cases is higher, on average, by approximately 10.3% in 
# countries with 10% higher in confirmed cases. The adj-R^2 is 0.89 which is pretty high. 


# reg2: Quadratic (linear) regression
# The graph shows the quadratic fit settles for slight nonlinearity. The pattern is a positive association through the
# entire range of observed ln_confirmed. The two slope parameters are 0.59 and 0.02, with no clear interpretation except
# the second, positive, number showing that the parabola is convex. It didn't lead to very different conclusions with
# the simple linear model because the pattern of association turned out to be fairly linear overall. 


# reg3: PLS
# The slope of first line segment is 0.87, just a little flatter than reg1- the simple linear regression, where the slope
# is 1.03. The slope of the other line segment is 1.15. Comparing countries with confirmed cases below 15000, death cases
# is higher, on average, by approximately 8.7% in countries with 10% higher in confirmed cases. With confirmed cases 
# above 15000, death cases is also higher, on average, by approximately 11.5% in countries with 10% higher in confirmed
# cases. The adj-R^2 is 0.89 here, same with the one from the simple linear regression. The improvement on the fit is very
# small in this case and only provides a better fit for only a few observations.

# reg4: Weighted-OLS with population
# The scatterplot for the weighted regression shows the size of each country: the area of the circle is proportionate to
# their population. 
# The same linear regression using population as weight gives a slope of 0.95, which turns out to be similar. 
# This shows that countries with 10% more confimred cases have, on average, 9.5% more death cases. 
# The adj-R^2 is improved by 0.04 compare with reg1, but is not a big difference.
# And RMSE is very high at 42.97 reflects the poor ability of the model to accurately predict the data even though my goal
# here is not model prediction, it is worthwhile to point out.

# Overall, the two regressions (reg1 and reg4) show similar result because larger countries do not tilt the regression
# line much. As the weighted regression produces results that are similar to the unweighted regression. 


# Conclusion: In this analysis, those more complicated specifications (reg2-reg4) didn't lead to very different conclusions
# becasue the pattern of association turned out to be fairly linear overall.


######
# Based on model comparison, my choice is to go with reg1 - Simple linear regression ln_death ~ ln_confirmed
#   Substantive: - log-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - Comparatively high R2 and captures variation well



## (j) You need to test your beta parameter for your chosen model, which interacts with your explanatory
## variable. (In case of quadratic or piecewise linear spline, test beta_1)
## i. Carry out the following test: H0 : beta = 0; HA : beta != 0
## We want to know if Dependent variable and the explanatory variable are related at all?
library(car)
linearHypothesis( reg1 , "ln_confirmed = 0")
# the p value of the hypothesis is < 2.2e-16 which is much lower than 0.1%, so we can reject the null at significance level
# of 0.1%. Meaning that beta cannot be 0. 



# (k) Finally, using your selected model, analyse the residuals:
#        i. Find countries who lost (relatively) the most people due to covid using the model result:
#               worst 5 residual.
#        ii. Find countries who saved (relatively) the most people due to covid using the model result:
#               best 5 residual.


# Get the predicted y values from the model
df$reg1_y_pred <- reg1$fitted.values
# Calculate the errors of the model
df$reg1_res <- df$ln_death - df$reg1_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg1_res ) %>% 
  select( country , ln_death , reg1_y_pred , reg1_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg1_res ) %>% 
  select( country , ln_death , reg1_y_pred , reg1_res )


