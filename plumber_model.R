#
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
#
# In RStudio 1.1 or older, see the Plumber documentation for details
# on running the API.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

#* @apiTitle Decision Tree predict if product can be a top seller in Polk County.

#* @apiDescription This API is for the distributor to test if the product that has been launched and 
#* whose samples were being sold are capable of being a potential product of generating more revenue in Polk County

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#loc <- "C:/Users/kinja/OneDrive/Desktop/All/2. SEM2/DPA/Descision_Tree/DecisionTree/DecisionTree_Plumber"
#load(paste(loc, "/train.rdata", sep = ""))
#load(paste(loc, "/test.rdata", sep = ""))

list_of_packages <- c("tidyverse", "plumber", "caret")
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  print("Installing packages\n")
  install.packages(new.packages())
}

path <- "C:/Users/kinja/OneDrive/Desktop/All/2. SEM2/DPA/Descision_Tree/DecisionTree/DecisionTree_Plumber/decisiontree.rdata"
model1 <- load(path) 

model_pred<- function(model1, Bottle_vol, State.Bottle.Cost, Bottles.Sold){
  
  log_Bottle_vol <- log(Bottle_vol)
  log_State.Bottle.Cost <- log(State.Bottle.Cost)
  log_Bottles.Sold <- log(Bottles.Sold)
  
  input_data <- data.frame(log_Bottle_vol,
                           log_State.Bottle.Cost,
                           log_Bottles.Sold)
  
  pred <<- predict(get(model1), input_data, type = "class")
  return(pred)
  
}


#' Model adding Volume Sold, Number of bottles, Sales in Dollars per pack. 
#' predict the probability of an product being potential seller(y/n).
 
#' @param Bottle_vol:numeric Volume in litres.
#' @param State.Bottle.Cost:numeric Costo of bottles
#' @param Bottles.Sold:numeric Number of bottles per pack 
#' @param Brand_Alcoholtype Give the brand alcohol combination
#' @post /model_pred
#' @html
#' @response Return prediction
 
predict_potential_brand <- function(Bottle_vol, State.Bottle.Cost, Bottles.Sold){
  
  log_Bottle_vol <- log(Bottle_vol)
  log_State.Bottle.Cost <- log(State.Bottle.Cost)
  log_Bottles.Sold <- log(Bottles.Sold)
  
  input_data <- data.frame(log_Bottle_vol,
                           log_State.Bottle.Cost,
                           log_Bottles.Sold)
  
  predict(get(model1), input_data, type = "class")
  
} 


