# library statements
library(mice)
library(tidyverse)
library(modelsummary)
library(Rcpp)

df <- as_tibble(read.csv("wages.csv")) # read in the data

df_dropped <- df %>% drop_na(hgc) %>% drop_na(tenure) # drop the NAs in the specified columns
head(df_dropped) # check that things look right

# create the modelsummary output
datasummary_skim(df_dropped) # this looks good, time to get the latex output
datasummary_skim(df_dropped, output = "latex")

# Do the regressions

# create an empty list to hold all the regression info
mods <- list()

# MCAR - listwise deletion
df_ld <- df %>% drop_na(logwage)
lm_ld <- lm(logwage~hgc + college + tenure + I(tenure^2) + age + married, data = df_ld)
mods[['Listwise deletion']] <- lm_ld

# mean imputation
df_mi <- df 
df_mi$logwage[is.na(df_mi$logwage)] <- mean(df_mi$logwage, na.rm = TRUE) # some code I found online for mean imputation
lm_mi <- lm(logwage~hgc + college + tenure + I(tenure^2) + age + married, data = df_mi) # regress with mi data
mods[['Mean Imputation']] <- lm_mi # add to list

# predicted values
# create a vector with all the predicted values from the lm_ld
pred_val <- predict(lm_ld, df)
# create a for-loop that will replace all the values
df_pv <- df
for(i in 1:nrow(df_pv)){
  if(is.na(df_pv[i, 1]) == TRUE){
    df_pv[i,1] = pred_val[i]
  }
}
lm_pv <- lm(logwage~hgc + college + tenure + I(tenure^2) + age + married, data = df_pv) # regress with pv data
mods[['Predicted Values']] <- lm_pv # add to list

# multiple imputation regression
df_mice <- mice(df, m = 5, printFlag = FALSE)
mods[['Mice']] <- with(df_mice, lm(logwage~hgc + college + tenure + I(tenure^2) + age + married)) # run model and add

# display all four models
modelsummary(mods) # this looks good, now add the LaTeX output
modelsummary(mods, output = "latex")
