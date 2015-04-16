corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        corre <- numeric(0)
        df <- complete(directory)
        ids <- df[,"i"]
        
        if (threshold > 0) {
          nobs <- df[,"nobs"]
          nobscond <- nobs>threshold
          ids <- ids[nobscond]
        }
          
        
        for (i in ids){
                data <- read.csv(paste(directory,"/",
                        sprintf("%03d", i),".csv",sep = ""))
                data_nit <- data[,"nitrate"]
                data_sul <- data[,"sulfate"]
                nitrate <- is.na(data[,"nitrate"])
                sulfate <- is.na(data[,"sulfate"])
                nit <- data_nit[!(nitrate | sulfate)]
                sul <- data_sul[!(nitrate | sulfate)]
                cortmp <- cor(nit,sul)
                if(!is.na(cortmp))
                  corre <- c(corre,cortmp)
        }
        corre
}