#PART 2

best <- function(state, outcome) {
      
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      #create new dataframe with required columns
      df <- as.data.frame(cbind(data[,2],
                                data[,7],
                                data[,11],
                                data[,17],
                                data[,23]),
                          stringsAsFactors = FALSE)
      
      colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
      
      chosen_state <- state
      
      ## Check that state and outcome are valid
      if(!chosen_state %in% df[["state"]]){
            stop("invalid state")
      }
      
      else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
            stop("invalid outcome")
      }
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
      else {
            x <- which(df[,"state"]==chosen_state)
            y <- df[x,]
            
            vals <- as.numeric(y[,eval(outcome)]) #convert into numeric type
            
            min_val<-min(vals,na.rm=TRUE)
            
            hos <- y[which(vals==min_val),"hospital"]
            
            
      }
      
      hos
}





