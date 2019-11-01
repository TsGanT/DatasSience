pollutantmean <- function(directory, pollutant, id = 1:332){

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
	  }
	  filename
	}
	for (i in id) {
	  filename <- obsFileName(directory, i)
	  data <- data.frame(read.csv(filename), header = 1)
	  if(pollutant == "sulfate"){
	    datau <- data['sulfate']
	    allData <- rbind(allData, datau)
	  }
	  if(pollutant == "nitrate"){
	    datau <- data['nitrate']
	    allData <- rbind(allData, datau)
	  }
	}
	c <- is.na(allData)
	absData <- allData[!c]
	m <- mean(absData)
	print(m)
}



#compdata <- function(id = 1:332){
#  comp <- data.frame(id=numeric(), nobs=numeric())
#	for(i in id){
#		filename <- obsFilename(i)
#		data <- read.csv(filename)
#		comp <- rbind(comp, data.frame(id = 1, nobs=nrow(data[complete.cases(data), ])))
#	}
 # comp
#}
