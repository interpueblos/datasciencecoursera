rankall <- function(outcome, num = "best") {
## Read outcome data
		outcomecsv <- read.csv("outcome-of-care-measures.csv", 
                               colClasses = "character")

## Check that the outcome is valid
		if(outcome != "heart attack" & outcome != "heart failure" &
                   outcome != "pneumonia") {
                stop("invalid outcome")
        }

## For each state, find the hospital of the given rank
        fac<-factor(outcomecsv$State)
        lfac<-levels(fac)
        df <- data.frame()
        for (i in lfac) {
                substate <- subset(outcomecsv, outcomecsv$State==i)
                
                if(outcome == "heart attack"){
                        substate[, 11] <- as.numeric(substate[, 11])
                        substate<-subset(substate,!is.na(substate[,11]))
                        substate <- substate[order(substate[,11],substate$Hospital.Name),]
                } else if (outcome == "heart failure") {
                        substate[, 17] <- as.numeric(substate[, 17])
                        substate<-subset(substate,!is.na(substate[,17]))
                        substate <- substate[order(substate[,17],substate$Hospital.Name),]
                } else {
                        substate[, 23] <- as.numeric(substate[, 23])
                        substate<-subset(substate,!is.na(substate[,23]))
                        substate <- substate[order(substate[,23],substate$Hospital.Name),]
                }
                
                records<-nrow(substate)
                if(num=="best"){
                        #minval <- min(substate[,11],na.rm=TRUE)
                        rate<-substate[1,2]
                } else if(num=="worst") {
                        rate<-substate[records,2]
                } else if (is.numeric(num)) {
                        if (records<num) {
                                rate<-NA
                        } else {
                                rate<-substate[num,2]
                        }
                        
                } else {
                        rate<-NA
                }
                dftmp <- data.frame(rate,i)
                df <- rbind(df,dftmp)
        }
        names(df)<-c("hospital","state")

## Return a data frame with the hospital names and the
## (abbreviated) state name
        df
}