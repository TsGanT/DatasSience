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
	  }
	  filename
	}
	for (i in id) {
	  filename <- obsFileName(directory, i)
	  data <- data.frame(read.csv(filename), header = 1)
	  p <- !is.na(data["sulfate"]) & !is.na(data["nitrate"])
	  q <- sum(p)
	  nobs <- c(id = i,nobs = q)
	  allData <- rbind(allData, nobs)
	}
	x <- cbind(1:length(id), allData)
	print(x)
}     