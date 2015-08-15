complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    cs.id <- c()
    cs.num <- c()
    
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
        
        file_name <- paste(directory, monitor_id , sep="/")
        pollutant.data <- read.csv(file_name)
        pollutant.cs <- pollutant.data[complete.cases(pollutant.data),]
        
        cs.id <- c(cs.id, i)
        cs.num <- c(cs.num, nrow(pollutant.cs))
    }
    return (data.frame("id"=cs.id, "nobs"=cs.num))
}