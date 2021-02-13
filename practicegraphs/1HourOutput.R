GraphOutput = read.csv("practicegraphs/practiceGameOutputs.csv")

#Theoretical Output
#Let's say we have a dataframe with mTurk ID. A certain range of mTURK ID's indicate insurance, while another range of mTURK ID's indicate no insurance. This would allow me to sort based on a certain range of values. 


#Creating empty dataframe
experimentallist <- data.frame(mTurkID=numeric(200), herdsize=numeric(200), year=numeric(200), name = numeric(200), insurance = numeric(200))

experimentallist$mTurkID = sample(30:60, 200, replace = TRUE)

experimentallist$herdsize = rnorm(200, mean = 500, sd=1)

experimentallist$year = sample(2000:2011, 200, replace = TRUE)

experimentallist$insurance = ifelse(experimentallist$mTurkID < 39, "noinsurance","insurance")

experimentallist$mTurkID =  factor(experimentallist$mTurkID )


 experimentallist$name = ifelse(experimentallist$mTurkID == 30, "Test",
ifelse(experimentallist$mTurkID == 31, "Rob",
       ifelse(experimentallist$mTurkID == 32, "Simon",
              ifelse(experimentallist$mTurkID == 33,"Yousef",
              ifelse(experimentallist$mTurkID == 34, "Leah",                   ifelse(experimentallist$mTurkID == 35,"Job",ifelse(experimentallist$mTurkID == 36,"Lob",ifelse(experimentallist$mTurkID == 37,"Mob",ifelse(experimentallist$mTurkID == 38,"Sob",ifelse(experimentallist$mTurkID == 39,"Aob",ifelse(experimentallist$mTurkID == 40,"Rub",ifelse(experimentallist$mTurkID == 41,"Sub",ifelse(experimentallist$mTurkID == 42,"Dud",ifelse(experimentallist$mTurkID == 43,"Lub",ifelse(experimentallist$mTurkID == 44,"B12b",ifelse(experimentallist$mTurkID == 45,"Bsb",ifelse(experimentallist$mTurkID == 46,"B13b",ifelse(experimentallist$mTurkID == 47,"B2b",ifelse(experimentallist$mTurkID == 48,"B14b",ifelse(experimentallist$mTurkID == 49,"B6b",ifelse(experimentallist$mTurkID == 50,"B15b",ifelse(experimentallist$mTurkID == 51,"B4b",ifelse(experimentallist$mTurkID == 52,"B16b",ifelse(experimentallist$mTurkID == 53,"B8b",ifelse(experimentallist$mTurkID == 54,"B17b",ifelse(experimentallist$mTurkID == 55,"B9b",ifelse(experimentallist$mTurkID == 56,"B18b",ifelse(experimentallist$mTurkID == 57,"B10b",ifelse(experimentallist$mTurkID == 58,"B19b",ifelse(experimentallist$mTurkID == 59,"B11b",ifelse(experimentallist$mTurkID == 60,"B20b", NA)))))))))))))))))))))))))))))))
 
#removes 0 ouput years
test13 = subset(GraphOutput, yr != 0) 
library(ggrepel)
library(scales)
 ggplot(test13, aes(yr, net.wrth, group = mTurkID, color = profit)) +
   geom_jitter() +
   #geom_smooth(se = FALSE)+ 
   #geom_text_repel(aes(label = rev.ins), box.padding = unit(.50, "lines"), point.padding = unit(.5, "lines")) +
   scale_y_log10(labels = comma, breaks = c(-500000, 0,100000)) +
   labs(x="Year", y="Profit in $")
 
 ggplot(test13, aes(yr, herd, group = mTurkID, color = profit)) +
   geom_jitter(size = 2) +
   labs(x="Year", y = "Herd Size") +
   ggtitle("Herd Size vs Year, insurance")


  library(ggplot2)
ggplot(experimentallist, aes(year, herdsize, group = mTurkID, color =  insurance)) +
  geom_smooth() +
  geom_jitter() +
  ggtitle("Herd Size per Year, Insurance vs. No Insurance") +
  labs(x = "Year", y = "Herd Size")

names



library("plyr")
GraphOutput = plyr::rename(GraphOutput, c("herd" = "herdsize", "yr" = "year", "rev.ins" = "revinsurance"))
names(GraphOutput)

test = subset(GraphOutput, mTurkID!=0 & year!=0)

View(test)

ggplot(experi, aes(mTurkID, herdsize, color = mTurkID)) +
  geom_line()

ggplot(test, aes(revinsurance, herdsize, color = mTurkID)) +
  geom_line()

list(is.na(test))

