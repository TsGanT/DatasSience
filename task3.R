corr <- function(directory, threshold = 0){
	
	complete <- function(directory, id = 1:332){
	allData <- NULL
	obsFileName <- function(directory, i){
	  if (i < 10){
	    filename = paste(directory, "/", '00', i, ".csv", sep="")
	  }
	  if (i >= 10 && i < 100){
	    filename = paste(directory, "/", "0", i, ".csv", sep="")
	    
	  }
	  if(i >= 100){
	    filename = paste(directory, "/", id, ".csv",sep="")
	    print("aaaa")
	  }
	  filename
	}
	for (i in id) {
	  filename <- obsFileName(directory, i)
	  data <- data.frame(read.csv(filename), header = 1)
	  p <- !is.na(data["sulfate"]) & !is.na(data["nitrate"])
	  index <- length(p)
	  q <- data[which(p>0),]	  
	  allData <- rbind(allData, q)
	  
	}
	allData
	}	
	
	cor_results <- numeric(0)
	observations <- complete(directory, 1:10)
	observations <- observations[which(observations[,4] >= threshold)]
	print(observations)
	if(nrow(observations) > 0){
	  for(monitor in observations$id){
	    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
	    #print(path)
	    monitor_data <- read.csv(path)
	    #print(monitor_data)
	    interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
	    interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
	    sulfate_data <- interested_data["sulfate"]
	    nitrate_data <- interested_data["nitrate"]
	    cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
	    
	  }
	}
	cor_results
	print(cor_results)
	#rownumber <- nrow(observations)
	#w <- observations[threshold:rownumber, ]
	#result <- cor(w["sulfate"], w["nitrate"])
	#print(result)
	#print(rownumber)
	
}