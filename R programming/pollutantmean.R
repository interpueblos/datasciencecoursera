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
        
        means <- numeric()
        for (i in id){
                data <- read.csv(paste(directory,"/",
                        sprintf("%03d", i),".csv",sep = ""))
                data_pol <- data[,pollutant]
                na_vector <- is.na(data_pol)
                means <- c(means,data_pol[!na_vector])
        }
        mean(means)
        #mean(means[!is.na(means)])
        
}