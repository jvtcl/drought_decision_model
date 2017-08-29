# CPER Data Prep
# Created 8/23/17 by Trisha Shrum
# Uses code from StatisticalTests.R created by Evan Lih

#reading packages
library(magrittr)
library(lattice)
library(dplyr)

# Load data from CPER (full 10-round simulation)
cperGameOutputs <- read.csv("results/cperGameOutputs.csv")

#subsetting. We remove all years that are 0 (glitch in output file), remove "practiceround" mTurkID
cperGameOutputs <- cperGameOutputs %>% subset(yr != 0) %>%
  subset(mTurkID != "practiceRound")

#Ifelse statement creating new insurance column. 1 = Insurance,  0=  new insurance. 
cperGameOutputs$insurance <- ifelse(cperGameOutputs$cost.ins > 0, 1, 0)

#removes columns in data output that are just 0's/blank. Makes for reading data easier. 
cperGameOutputs$adapt_choice = NULL
cperGameOutputs$cap.purch = NULL
cperGameOutputs$forage.production = NULL
cperGameOutputs$household.exp = NULL
cperGameOutputs$taxes = NULL
cperGameOutputs$forage.potential = NULL

