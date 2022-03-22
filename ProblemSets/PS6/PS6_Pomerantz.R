# Library statements
library(tidyverse)
library(ggplot2)
library(gridExtra) # using this to load the .dat data into R

# load the NLSY79 cohort data in first 

# this simply doesn't work because it didn't preserve the variable names which is a #problem
NLSY79 <- read.table("/Users/leahpomerantz/Desktop/Spring\ 2022/MA\ Thesis/FirstDataDownload/default/default.dat")

# source the function I created to load in and execute the code
source("/Users/leahpomerantz/Desktop/Spring\ 2022/MA\ Thesis/R\ Code/open_NLSY79_data.R")
NLSY79_total <- open_NLSY79_data() # takes both values that the function produces
NLSY79_data <- NLSY79_total$data # something here is going wrong in my function, because it's staying as a list
NLSY79_colnames <- NLSY79_total$cat_labels # this tracks our column names, which we can use later
# the plan with the column names is to first filter our data, but use this as a guide to easily reference, i.e., make
# it the first row and then just exclude the first row from our data analysis

# check that things are looking right
head(NLSY79_data) # this shows the data

# store the number of observations to keep track of how it's filtering down
num_obs1 <- nrow(NLSY79_data)


# next, i'm going to trickle down to just the variables that I'm using for this specific assignment
use_df <- NLSY79_data[,c(1,3:5,8)]
use_df <- cbind(use_df, NLSY79_data$HGC_EVER_XRND)
head(use_df) # just check that things are looking right

# make my column names a little easier to work with
colnames(use_df) <- c("CaseID", "SampleID", "Race", "Sex", "AFTQ3", "HGC_Ever")

# things seem okay, but we need to check for NAs
use_df2 <- na.omit(use_df)
num_obs2 <- nrow(use_df2) # find the number of observations to see how many we throw out
head(use_df2)
# only got rid of about 300 observation, so I'm not going to worry too much about it


# now that we have our observations (CASEID, SampleID, Race, Sex, and AFQT score as an approximation of intelligence),
# we can start creating the graphs (Education level vs. Intelligence score, Education vs Race, Education vs. Sex)

# First graph: Education level vs. intelligence
graph1 <- ggplot(use_df2, aes(HGC_Ever, AFTQ3, color = HGC_Ever)) + 
  geom_jitter(height = 2, width = 2)
graph1

# this graph is interesting, but suggests that AFTQ scores may be so close together that we should
# try treating it as continuous and see what we get
graph2 <- ggplot(use_df2, aes(factor(HGC_Ever), AFTQ3, color = HGC_Ever)) + 
  geom_violin(scale = "area")
graph2

# this graph is looking a little squished, so let's split into two graphs and lay them next to each other
use_df3 <- use_df2 %>% filter(HGC_Ever %in% c(0:10)) # creating new data frames to make the graphing easier
use_df4 <- use_df2 %>% filter(HGC_Ever %in% c(11:20))

# graph3 contains levels 0-10
graph3 <- ggplot(use_df3, aes(factor(HGC_Ever), AFTQ3, color = HGC_Ever)) + 
  geom_violin(scale = "area")
graph3

# graph4 contains levels 11-20
graph4 <- ggplot(use_df4, aes(factor(HGC_Ever), AFTQ3, color = HGC_Ever)) + 
  geom_violin(scale = "area")
graph4

# now try putting them together
grid.arrange(graph3, graph4, nrow = 2) # this gets us a much more useful looking plot
# this is also super helpful because we just have so darn many observations

# now we can do a plot of Education vs race, and these are both more categorical, so our graph will
# make more sense
graph5 <- ggplot(use_df2, aes(HGC_Ever, Race, color = HGC_Ever)) + 
  geom_jitter(height = 2, width = 2)
graph5 # this doesn't really work because of the jitter feature, so too overwhelming

# this graph is also quite overwhelming, so let's try something a little different
graph6 <- ggplot(use_df2, aes(HGC_Ever, Race, color = HGC_Ever)) + 
  geom_count()
graph6 # this is a winner!

# now we need one more, let's try Education vs Sex
graph7 <- ggplot(use_df2, aes(HGC_Ever, Sex, color = HGC_Ever)) + 
  geom_count()
graph7 # heck yeah let's use this 



