#PART 3

rankhospital <- function(state, outcome, num = "best") {
      
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
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      else if(is.numeric(num)) {
            x <- which(df[,"state"]==chosen_state)
            y <- df[x,] #new df filtered by state
            
            y[,eval(outcome)] <- as.numeric(y[,eval(outcome)]) #convert into numeric type
            z <- y[order(y[,eval(outcome)],y[,"hospital"]),]
            result <- z[,"hospital"][num]
      }
      else if (!is.numeric(num)){
            if(num == "best"){
                  result <- best(state, outcome)
            }
            else if(num == "worst"){
                  x <- which(df[,"state"]==chosen_state)
                  y <- df[x,] #new df filtered by state
                  
                  y[,eval(outcome)] <- as.numeric(y[,eval(outcome)]) #convert into numeric type
                  z <- y[order(y[,eval(outcome)],y[,"hospital"],decreasing = TRUE),]
                  result <- z[,"hospital"][1]
            }
            else{
                  stop("invalid rank")
            }
      }      
      result
}