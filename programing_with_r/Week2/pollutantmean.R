pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    pollutant.total <- 0
    pollutant.mean <- 0
    for(i in id){
        ## Adds .csv to the end of the id and padding them with zeroes
        monitor_id <- c()
        if(i < 10){
            monitor_id <- paste("00", i, ".csv", sep="")
        } else if(i<100){
            monitor_id <- paste("0",i, ".csv", sep="")
        } else {
            monitor_id <- paste(i, ".csv", sep="")
        }
        
        file.name <- paste(directory, monitor_id , sep="/")
        data <- read.csv(file.name)
        pollutant.total <- c(pollutant.total, data[,pollutant])
    }
    pollutant.mean <- mean(pollutant.total, na.rm = TRUE)
    return(pollutant.mean)
}