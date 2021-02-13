"
#~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~
A function to graph the ranching decision model results.

Earth Lab Risk Team 
Travis Williams
###~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~
"
setwd("C:/Users/Travis/Google Drive/Risk Project/ranch_market_weather/data")
load("gameStats.R") # data2
numbers = data2

gameStats = function(numbers){
  "
  Take in the decision model results and output a dataframe with a graph of a variable of choice 
  "
  require(dplyr)
  require(stringr)
  require(ggplot2)
  continue = "y"
  cat("Follow the prompts to generate a chart and a tibble for any variable in the dataset. Type 'q' to break out of the function")
  numbers=tbl_df(numbers)
  while (TRUE){
    if (continue == tolower("y")){  
      print(paste("Variables for the results dataset"))
      #glimpse(data) 
      names = as.data.frame(names(numbers))
      print(names)
      variable = readline(prompt = "Enter the number for the variable that you want a time series of? ")      
      if (tolower(variable =="q")){break}
      index = as.numeric(variable)
      variable = as.character(names[index,])
      how = readline(prompt = "Aggregate or indexed?(Type the word or '1' or '2') ")        
      if(how=="indexed"|how=="2"){ 
        yr = "yr"
        mTurkID= "mTurkID"
        timeseries = ggplot(data = numbers,aes_string(x=yr,y=variable,group = mTurkID,color=mTurkID))
      }else{
        variable = as.name(variable)
        numbers2 = numbers%>%group_by(yr)%>%mutate(newvar=sum(variable))%>%distinct(newvar)        
        yr = "yr"
        timeseries = ggplot(data = numbers,aes_string(x=yr,y=variable))
      }
      plot = timeseries+
        geom_line(lwd=1.50)+
        #theme(legend.position="none")+
        scale_x_continuous(breaks=seq(2000,2009,by = 1)) +
        #geom_smooth(method = "lm", se = TRUE, color = "red") +
        #yscale +
        ylab(toupper(variable))+
        xlab("YEAR")+
        #theme_fivethirtyeight()+
        theme_economist() + scale_colour_economist() +
        #theme_stata() + scale_colour_stata()+
        #theme_minimal()+ 
        ggtitle("Results")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none",plot.title = element_text(hjust = 0.5))+
        theme(
          plot.title = element_text(color="black", size=15, face="bold"),
          axis.title.x = element_text(color="black", size=10, face="bold"),
          axis.title.y = element_text(color="black", size=10, face="bold")
        )
        
      print(plot)
      answer = readline(prompt="start over(Y/N)? ")
      if(tolower(answer)=="q"){break}
      if(tolower(answer)=="y"){print("Okay, starting over")}
      else{
        print(paste("Okay, returning the chart for",variable))
        return(numbers)
      }
      continue == answer
      }
  }
} 

gameStats(numbers)     