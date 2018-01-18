Object <- setClass(
  # Set the name for the class
  "Object",
  
  # Define the slots
  slots = c(
    name = "character",
    links = "list"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    name = "",
    links = NULL
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(FALSE) {
      return("The parameters are not correct.")
    }
    return(TRUE)
  }
)

Link <- setClass(
  # Set the name for the class
  "Link",
  
  # Define the slots
  slots = c(
    property = "Object",
    object = "Object"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    property = NULL,
    object = NULL
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(FALSE) {
      return("The parameters are not correct.")
    }
    return(TRUE)
  }
)