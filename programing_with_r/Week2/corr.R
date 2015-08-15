corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    data.completecases <- complete(directory)
    data.threshold <- subset(data, nobs > threshold)
    
    if(nrow(data.threshold) == 0){
        return (vector(mode = "numeric"))
    }
    corr_total <- c()
    id <- as.vector(data.threshold$id)
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
        
        pollutant.data <- pollutant.data[complete.cases(pollutant.data),]
        sulfate.data <- pollutant.data["sulfate"]
        nitrate.data <- pollutant.data["nitrate"]
        corr_total <- c(corr_total, cor(sulfate.data, nitrate.data))
    }
    return (corr_total)
}