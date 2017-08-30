"
#~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~
A script to graph the ranching decision model results
from a conference at the CPER in 2017.

Earth Lab Risk Team 
Travis Williams
###~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~
"
#setwd("C:/Users/Travis/GitHub/drought_decision_model/practicegraphs")
require(dplyr)
require(stringr)
require(ggplot2)
require(ggthemes)
library(lazyeval)
library(scales)

##########################################################################
####################### GameResults Data #################################
##########################################################################
full <- read.csv("results/cperPracticeGameOutputs.csv")
#load("C:/Users/Travis/Google Drive/Risk Project/ranch_market_weather/data/monthlyNOAA.RData")

##########################################################################
####################### Organizing the dataframe #########################
##########################################################################
# Because the set I built this on had repeat mTurkIDs I created an index. 
# You can probably just use the mTurkIDs if they are all different, but 
numbers = full%>%na.omit()%>%filter(yr>0)
# numbers$index = as.numeric(row.names(numbers))
# numbers[order(numbers$index), ]
# numbers$index[1:5] = 1
# numbers$index[6:10] = 2
# numbers$index[11:15] = 3
# numbers$index[16:20] = 4
# numbers$index[21:25] = 5
# numbers$index[26:30] = 6
# numbers$index[31:35] = 7
# numbers$index[36:40] = 8
# numbers$index[41:45] = 9
# numbers$index[46:50] = 10
# numbers$index[51:55] = 11
# numbers$index[56:59] = 12
# numbers$index[60:64] = 13
# numbers$index[65:69] = 14
# numbers$index[70:74] = 15
# numbers$index[75:79] = 16
# numbers$index[80:84] = 17
# numbers$index[85:89] = 18
# numbers$mTurkID = numbers$index
numbers$insured = 0
numbers$insured[which(numbers$cost.ins > 0)] = 1
numbers$insured = factor(numbers$insured)
##########################################################################
###################### Precipitation Data ################################
##########################################################################
gameStats = function(numbers,year1,year2){
  "
  Take in the decision model results and outputs a dataframe with a graph of a variable of choice 
  Things to do:
    add a legend for indexed, not indexed, insured and not insured
    add an insured aggregate option
    somehow add precipitation?
      Which grid is CPER in?
      select appropriate years
      plot on second y-axis

  "
  require(dplyr)
  require(stringr)
  require(ggplot2)
  require(ggthemes)
  continue = "y"
  cat("Follow the prompts to generate a chart and a tibble for any variable in the dataset. Type 'q' to break out of the function")
  numbers=tbl_df(numbers)
  while (TRUE){
    if (continue == tolower("y")){  
      print(paste("Variables for the results dataset"))
      names = as.data.frame(names(numbers))
      print(names)
      variable = readline(prompt = "Enter the number for the variable that you want a time series of? ")
      if (tolower(variable =="q")){break}
      index = as.numeric(variable)
      variable = as.character(names[index,])
      how = readline(prompt = "Aggregate or indexed?(Type the word or '1' or '2') ")        
      if(how=="indexed"|how=="2"){         
        numbers2 = numbers%>%select_(~yr,~index,~insured,~variable)        
        names(numbers2) = c("Year","Index","Insured",variable)
        Year = "Year"
        mTurkID= "mTurkID"
        Index = "Index"
        Insured = "Insured"
        timeseries = ggplot(data = numbers2,aes_string(x=Year,y=variable,group = Index,color=Insured,fill=Insured))
      }else{
        call = lazyeval::interp(~sum(a),a = as.name(variable))
        numbers2 = numbers%>%group_by(yr,insured)%>%mutate_(.dots=setNames(list(call),'newvar'))%>%distinct(newvar) 
        names(numbers2) = c("Year","Insured",variable)
        Year = "Year"
        Insured = "Insured"
        timeseries = ggplot(data = numbers2,aes_string(x=Year,y=variable,group=Insured,color = Insured))
      }
      plot = timeseries+
        geom_line(lwd=1.25)+
        #geom_smooth()+
        scale_x_discrete(breaks=seq(year1,year2,by = 1)) +
        scale_y_continuous(labels = comma)+
        ylab(toupper(variable))+
        xlab("YEAR")+
        #theme_fivethirtyeight()+
        #theme_economist() #+ scale_colour_economist() +
        #theme_stata() + scale_colour_stata()+
        theme_minimal()+ 
        ggtitle("Results")+      
        scale_fill_manual(values=c("orange","red"))+
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position="right",
          plot.title = element_text(hjust = 0.5,color="black", size=15, face="bold"),
          axis.title.x = element_text(color="black", size=10, face="bold"),
          axis.title.y = element_text(color="black", size=10, face="bold")
        )
      print(plot)
      answer = readline(prompt="start over(Y/N)? ")
      if(tolower(answer)=="q"){break}
      if(tolower(answer)=="y"){print("Okay, starting over")}
      else{
        print(paste("Okay, returning the dataset for",variable))
        return(numbers)
      }
      continue == answer
      }
  }
} # if the index is needed
x=gameStats(numbers,2000,2004)

gameStats2 = function(numbers,year1,year2){
  "
  Take in the decision model results and outputs a dataframe with a graph of a variable of choice 
  Things to do:
  add a legend for indexed, not indexed, insured and not insured
  add an insured aggregate option
  somehow add precipitation?
  Which grid is CPER in?
  select appropriate years
  plot on second y-axis
  
  "
  require(dplyr)
  require(stringr)
  require(ggplot2)
  require(ggthemes)
  continue = "y"
  cat("Follow the prompts to generate a chart and a tibble for any variable in the dataset. Type 'q' to break out of the function")
  numbers=tbl_df(numbers)
  while (TRUE){
    if (continue == tolower("y")){  
      print(paste("Variables for the results dataset"))
      names = as.data.frame(names(numbers))
      print(names)
      variable = readline(prompt = "Enter the number for the variable that you want a time series of? ")
      if (tolower(variable =="q")){break}
      index = as.numeric(variable)
      variable = as.character(names[index,])
      how = readline(prompt = "Aggregate or indexed?(Type the word or '1' or '2') ")        
      if(how=="indexed"|how=="2"){         
        numbers2 = numbers%>%select_(~yr,~mTurkID,~insured,~variable)        
        names(numbers2) = c("Year","mTurkID","Insured",variable)
        Year = "Year"
        mTurkID= "mTurkID"
        Index = "Index"
        Insured = "Insured"
        timeseries = ggplot(data = numbers2,aes_string(x=Year,y=variable,group = mTurkID,color=Insured,fill=Insured))
      }else{
        call = lazyeval::interp(~sum(a),a = as.name(variable))
        numbers2 = numbers%>%group_by(yr,insured)%>%mutate_(.dots=setNames(list(call),'newvar'))%>%distinct(newvar) 
        names(numbers2) = c("Year","Insured",variable)
        Year = "Year"
        Insured = "Insured"
        timeseries = ggplot(data = numbers2,aes_string(x=Year,y=variable,group=Insured,color = Insured))
      }
      plot = timeseries+
        geom_line(lwd=1.25)+
        #geom_smooth()+
        scale_x_discrete(breaks=seq(year1,year2,by = 1)) +
        scale_y_continuous(labels = comma)+
        ylab(toupper(variable))+
        xlab("YEAR")+
        #theme_fivethirtyeight()+
        #theme_economist() #+ scale_colour_economist() +
        #theme_stata() + scale_colour_stata()+
        theme_minimal()+ 
        ggtitle("Results")+      
        scale_fill_manual(values=c("orange","red"))+
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position="right",
          plot.title = element_text(hjust = 0.5,color="black", size=15, face="bold"),
          axis.title.x = element_text(color="black", size=10, face="bold"),
          axis.title.y = element_text(color="black", size=10, face="bold")
        )
      print(plot)
      answer = readline(prompt="start over(Y/N)? ")
      if(tolower(answer)=="q"){break}
      if(tolower(answer)=="y"){print("Okay, starting over")}
      else{
        print(paste("Okay, returning the dataset for",variable))
        return(numbers)
      }
      continue == answer
    }
  }
} # if the index is needed
x=gameStats2(numbers,2000,2004)

