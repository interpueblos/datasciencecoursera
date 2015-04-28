best <- function(state, outcome) {
	## Read outcome data
	outcomecsv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid
	if(!(state %in% outcomecsv$State)) {
                stop("invalid state")
	}
        
        if(outcome != "heart attack" & outcome != "heart failure" &
                   outcome != "pneumonia") {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## First validate tah all the rates are avalaible
	substate <- subset(outcomecsv, outcomecsv$State==state)
	
	if(outcome == "heart attack"){
	        substate[, 11] <- as.numeric(substate[, 11])
	        #cond <- is.na(substate[,11])
	        minval <- min(substate[,11],na.rm=TRUE)
	        ret<-subset(substate, substate[,11]==minval)
	        message(nrow(ret))
	} else if (outcome == "heart failure") {
	        substate[, 17] <- as.numeric(substate[, 17])
	        #cond <- is.na(substate[,17])
	        minval <- min(substate[,17],na.rm=TRUE)
	        ret<-subset(substate, substate[,17]==minval)
	} else {
	        substate[, 23] <- as.numeric(substate[, 23])
	        #cond <- is.na(outcome[,23])
	        minval <- min(substate[,23],na.rm=TRUE)
	        ret<-subset(substate, substate[,23]==minval)
	}
	
        ## rate
	
        if (nrow(ret)==1) {
                rate<-ret[,2]
        } else {
                
                ##order dataframe
                rate<-ret[1,2]
        }
        rate
}