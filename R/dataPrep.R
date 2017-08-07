
library(dbConnect)
library(dplyr)
library(readr)
library(stringr)

#### Simulation Data ####

# Connecting to AWS MySQL server
con <- dbConnect(MySQL(),
                 user = 'cowgame',
                 password = 'cowsrock',
                 host = 'teamriskcowgame.cvkdgo9ryjxd.us-west-2.rds.amazonaws.com',
                 dbname = 'cowgame')

# Reading current tables on server
dbListTables(conn = con)

# Downloading mTurk practice runs
prac <- dbReadTable(conn = con, "mTurkRun1Practice")
sim <- dbReadTable(conn = con, "mTurkRun1Sim")

# Taking out unnecessary columns
sim %>% select(-adapt_choice, -household.exp, -precipWeight.change, -cap.purch) -> sim

# Creating dummy variable for insurance treatment
sim$ins <- ifelse(sim$cost.ins > 0, 1, 0)

# Year dummies
sim$yr <- factor(sim$yr)


#### Survey Data ####
s <- read_csv("data/No Ins Experiment - MTurk Beta_July 13, 2017_18.05.csv")
s <- s[-1:-2,]  # taking out first two rows

s %>% filter(str_sub(s$StartDate, 1, 10) == "2017-07-13") -> s # keeping only data from 7/13/17

