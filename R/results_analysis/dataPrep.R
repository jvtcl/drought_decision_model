
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


#EVan Data Code
#load required packages
library(ggplot2)
library(zoo)
library(scales)
library(magrittr)
library(RMySQL)
library(extrafont)
library(dplyr)
library(plotly)

#import fonts - first time running takes ~5min
font_import()
loadfonts(device = "win")

#connecting to SQL Server
con <- dbConnect(MySQL(), user = 'cowgame', 
                 password = 'cowsrock', 
                 host = 'teamriskcowgame.cvkdgo9ryjxd.us-west-2.rds.amazonaws.com', 
                 dbname = 'cowgame')

dbListTables(conn = con)

cowOutput = dbReadTable(conn = con, "cowGameOutputs")

cowOutput = cowOutput %>% subset(yr!= 0)

#rolling average 

summ <- data.frame(y=c(9436,5655),
                   Legend=c("Mean Insurance Payout","Cost of Insurance"))


ggplot(cowOutput, aes(yr, rev.ins)) +
  geom_bar(aes(x=yr, y=rev.ins), stat="identity", fill="blue", alpha = .5) +
  scale_x_continuous(breaks = c(seq(from = 1950, to = 2015, by = 5 ))) +
  scale_y_continuous(labels = comma, breaks = c(seq(from = 0, to = 55000, by = 10000))) +
  theme_bw() +
  geom_hline(data=summ,aes(yintercept = y,colour=Legend), lwd=1.05, linetype = 4) + 
  scale_color_manual(values = c(rgb(0,0,0,maxColorValue = 255),"gold","black"))+
  ggtitle("Hypothetical PRF Indemnity Payments") +
  xlab("Year") +
  ylab("Insurance Payout ($)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        axis.title.x = element_text(size= 15 ),
        axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"))



#optimal levels of herd and hay purchase
#plot rainfall and hay purchase required to keep cows at full health for each year.  - dig into the functions for the different rainfall levels. Use those to calculate optimal hay to keep herd at 600 average pounds/full health - full adaptation level. dig into model/function. 

#plot rainfall for each year - practice using original dates (19##). see full adaptation amounts. called full adapt. isn't shown to the user. 

#plot the full yearset of rainfall over PRF Indemnity. Use background stripe/color to indicate whether it is practice round or full round. 
# Default location is CPER site



#pulling from local dataset
noaadata = monthlyNOAA_long

#removing unecessary columns
noaadata[,c("AVG", "index", "grid", "value")] = NULL

#naming correct columns
colnames(noaadata)[1] <- "Month"

#creating copy of dataset
noaadata2 <- noaadata

#only keeping May-Aug months
noaadata2 <- noaadata2 %>%
  filter(Month %in% c("MAY", "JUNE", "JULY", "AUG"))

#grouping the remaining months, and summing the values
noaadata2 <- noaadata2 %>%
  group_by(Year) %>%
  summarise(Inches = sum(realValue)) 

#Changing Year to numeric, so we can use scalexcontinuous in ggplot
noaadata2$Year = as.numeric(noaadata2$Year)

#converting INches to mm
noaadata2 = mutate(noaadata2, mm = Inches * 25.4)

#creating dummy dataframe to highlight our drought decision model rainfall areas
df <- data.frame(xmin = c(1951,1999),
                 xmax = c(1956,2010),
                 ymin=c(0,0),
                 ymax=c(Inf,Inf),
                 Legend = c("Practice Round","Ranch Round"))

#ggplot for plotting rainfall
ggplot(noaadata2, aes(Year, mm)) +
  scale_x_continuous(breaks = c(seq(from = 1950, to = 2015, by = 5 ))) +
  scale_y_continuous(breaks = c(seq(from = 0, to = 300, by = 50 ))) +
  geom_rect(data=df, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, fill = Legend),
            alpha = .5, inherit.aes = FALSE) +
  scale_fill_manual(values=c("olivedrab2", "tomato1")) +
  geom_bar(aes(Year,mm), stat = "Identity", fill = "blue", alpha = .5) +
  ggtitle("Rainfall") +
  theme_bw() +
  labs(x="Year", y="Rainfall (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        axis.title.x = element_text(size= 15 ),
        axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"))


#creating monthly precip dataframe
monthlyPrecipWeights <- data.frame("Rainfall" = c(0.0, 0.0, 0.02, 
                                                  0.08,0.20,0.28,
                                                  0.15,0.12,0.10,
                                                   0.05,0.0,0.0), 
                                   "Months" = c("Jan", "Feb", "Mar", "Apr", 
                                                "May", "Jun", "Jul", "Aug", 
                                                "Sep", "Oct", "Nov", "Dec"))

monthlyPrecipWeights$Months <- factor(monthlyPrecipWeights$Months, 
                                      levels = c("Jan", "Feb", "Mar", "Apr", 
                                                 "May", "Jun", "Jul", "Aug", 
                                                 "Sep", "Oct", "Nov", "Dec"))


#monthly precip graphd data
t1 = ggplot(monthlyPrecipWeights, aes(Months, Rainfall)) +
  geom_bar(aes(Months, Rainfall), stat = "identity", fill = "blue", alpha = .5) +
  geom_text(data=subset(monthlyPrecipWeights, Rainfall !=0), aes(label = Rainfall), position = position_dodge(width = .9), vjust =-.25) + 
  scale_y_continuous(breaks = c(seq(0,.30,.05))) +
  ggtitle("Forage Production Weights for Monthly Rainfall") +
  scale_fill_manual(values="olivedrab2") +
  theme_bw()+
  xlab("Month") +
  ylab("Weight (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        axis.title.x = element_text(size= 15 ),
        axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"))



#Forage Proudction weights for monthly ra  infall
#Weight (%)
library(plotly)

ggplotly(t1)

#creating dataframe. Ft is Forage, Ft.1 is Forage year minus 1.
initialw = data.frame("Ft.1" = c(rep(seq(from = .1,to = .9,by = .1),times=9)),
                      "Ft" = rep(seq(.1,.9,by=.1),each=9))

initialww = initialw

initialww = mutate(initialww, w=ifelse(initialww$Ft >1 & initialww$Ft.1>=1, 88, NA))

#Function "4" - w*Ft^(.25)*Ft-1 if Ft,Ft-1<1. 
#mutate - create new column. first argument is dataframe, w = new column. 
initialw = mutate(initialw, 
                  w = ifelse(initialw$Ft < 1 & initialw$Ft.1 < 1, 
                             ((.88*initialw$Ft)^.25*initialw$Ft.1)*100, NA))



#convert to matrix. use reshape2 package. value.var is the name of column which stores values, while Ft~Ft.1 is x variable ~ y variable. 
z <- acast(initialw, Ft ~ Ft.1, value.var = "w")

#Now we have the matrix. the next step is to create the x and y axis labels for the 3d plot. If we don't create these, plotly will auto populate these values with each of the corresponding value's number - ex .1->1, .2->2....9->9, etc. 

#because our row and column names are the same, we can name both x and y with just the rownames of z. 
x <- y <- as.numeric(rownames(z))

#now, we plot this shit out. 
plot_ly(x = x,y = y,z = z, type = "surface", colors = c("red","orange","green")) %>% 
  layout(scene = list(xaxis = list(title = 'Forage'),
                      yaxis = list(title = 'Forage Year-1'),
                      zaxis = list(title = 'Weaning Success Rate (%)')))

#3d scatter plot of same function (4)
plot_ly(initialw, x=~Ft,y=~Ft.1,z = ~w, color = ~"identity") %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Forage'),
                      yaxis = list(title = 'Forage year minus 1'),
                      zaxis = list(title = 'Weaning Success Rate')))
