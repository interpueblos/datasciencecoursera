rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomecsv <- read.csv("outcome-of-care-measures.csv", 
                               colClasses = "character")
        
        ## Check that state and outcome are valid
        if(!(state %in% outcomecsv$State)) {
                stop("invalid state")
        }
        
        if(outcome != "heart attack" & outcome != "heart failure" &
                   outcome != "pneumonia") {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        substate <- subset(outcomecsv, outcomecsv$State==state)
        
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
        
        ## rate
        rate
}