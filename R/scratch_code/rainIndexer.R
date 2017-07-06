myRain <- station.gauge$stgg
indexedRain <- data.table(matrix(0, nrow = nrow(myRain), ncol = 11))
for(i in 1:nrow(myRain)){
  for(j in 1:11){
    print(myRain[i, (j + 1)])
    indexedRain[i, j] <- myRain[i, j + 1, with = F] + myRain[i, j + 2, with = F]
  }
}

indexedRain[, Year := myRain[, Year]]
setnames(indexedRain, c(paste0("I", 1:11), "Year"))
indexedRain <- indexedRain[!Year %in% c("2016", "AVE"),]

indexedRain <- melt(indexedRain, id.vars = "Year")
indexedAve <- indexedRain[, mean(value), by = variable]
setnames(indexedAve, "V1", "AVG")
indexedRain <- merge(indexedRain, indexedAve, by = "variable")
indexedRain[,index := value/AVG]
indexedRain[, grid := 25002]
setnames(indexedRain, c("value", "index"), c("realValue", "value"))
indexedRain[, value := value * 100]
intervalNOAA <- indexedRain
