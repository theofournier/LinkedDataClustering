# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("D:/MesDocuments/ESILVA4/Parcours_Recherche/Code/R/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
#source("application_global.R")

# installation de la librairie  FactoMineR : install.packages("FactoMineR")
library(FactoMineR)




# ********************************************************
# BUILDING THE LIST OF LINK AND THE COLUMNS NAMES AND ROWS NAMES
# ********************************************************

col <- list() # list of all links possibles
#row <- c()
col_to_string <- c()
row_to_string <- c()
number_of_columns <- 0
for (j in 1:length(list_of_subjects)){
  # selecting the current subject :
  subject <- list_of_subjects[[j]] 
  
  # adding the new row name :
  row_to_string[j] <- subject@name
  
  for (i in 1:length(subject@links)){
    #for (i in 1:4){
    # selecting the current link :
    link <- subject@links[[i]]
    
    # adding the link in the list if not already added
    if(!contains(col, link)){
      
      # adding the link in the list (no need to add it twice):
      col[number_of_columns +1] <- link
      
      # building column name :
      col_to_string[number_of_columns + 1] <- paste("||" , link@property@name ,"||", link@object@name)
      
      # increment :
      number_of_columns <- number_of_columns + 1
    }
  }
}



# ***************************************************************************************************************
# ***************************************************************************************************************
#
#
# AFC & HCPC
#
#
# ***************************************************************************************************************
# ***************************************************************************************************************

# ********************************************************
# CREATING THE EMPTY MATRIX
# ********************************************************
data <- matrix(NA, length(list_of_subjects), length(col))
#colnames(data) <- col_to_string
rownames(data) <-  row_to_string




# ********************************************************
# PREPARING MATRIX FOR DBSCAN
# ********************************************************
for (j in 1:length(list_of_subjects)){
  # selecting the current subject :
  subject <- list_of_subjects[[j]] 
  
  for (i in 1:length(col)){
    # select the current link :
    link <- col[[i]]
    
    col_num <- i*2-1
    if(containsPropertyAndObject(subject@links, link)){
      data[j,i] <- 2
    } else if(containsProperty(subject@links, link)){
      data[j,i] <- 1
    } else {
      data[j,i] <- 0
    }
  }
}




# ********************************************************
# APPLYINF FCA & HCPC
# ********************************************************

res.ca <- CA(data)
res.hcpc = HCPC(res.ca, graph = FALSE)
plot(res.hcpc)
