
#reads output file. Both this .R script and the output file should be in the same directory. Also, make sure the csv file is correct format - not xlsx. 
cperGameOutputs = read.csv("results/cperGameOutputs.csv")

#installs pipe action
install.packages("magrittr")

#reading packages
library(magrittr)
library(lattice)
library(dplyr)

#subsetting. We remove all years that are 0 (glitch in output file), remove "practiceround" mTurkID
cperGameOutputs <- cperGameOutputs %>% subset(yr != 0) %>%
  subset(mTurkID != "practiceRound")

#Ifelse statement creating new insurance column. 1 = Insurance,  0=  new insurance. 
cperGameOutputs$Insurance <- ifelse(cperGameOutputs$cost.ins > 0, 1, 0)

#removes columns in data output that are just 0's/blank. Makes for reading data easier. 
cperGameOutputs$adapt_choice = NULL
cperGameOutputs$cap.purch = NULL
cperGameOutputs$forage.production = NULL
cperGameOutputs$household.exp = NULL
cperGameOutputs$taxes = NULL
cperGameOutputs$rev.ins = NULL
cperGameOutputs$forage.potential = NULL

#List of final net worth of each mTUrk ID. 2008 is the final year, so we filter by selecting only that year. 
Netvalues = cperGameOutputs %>% select(mTurkID,yr,net.wrth, Insurance)                                                               %>% filter(yr == 2008)

#This is code to calculate average net worth for each user/mTurk ID. It can be easily modified to calculate averages of other variables - simply change "net.wrth" to whatever variable you desire. 

#select() - selects the columns in dataframe. 
#group_by() - groups the columns by whatever variable you decide. All pipes past this will work by the specified group. In this case, we are grouping by mTurkID, so the mean function we will apply will be applied separately to each mTurk ID

#mutate - mutate is creating a new column. 

#distinct - keeps only unique/distnct rows from input table. IN this case, we are keeping meanWorth and Insurance. 

AvNet = cperGameOutputs %>% select(mTurkID,yr,net.wrth, Insurance) %>% group_by(mTurkID) %>%                                      mutate(meanWorth = mean(net.wrth)) %>% distinct(meanWorth,Insurance)


#t.test(y~x)  - y is numeric, x is a binary factor
#t.test(y1,y2) - y1 and y2 are numeric 

# lmtest lm(y ~ x1 +x2 + x3, data = mydata ) - linear regression test. Type in summary(lmtest) to see summary of output. 