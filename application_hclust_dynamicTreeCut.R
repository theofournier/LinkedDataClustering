# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("D:/MesDocuments/ESILVA4/Parcours_Recherche/Code/R/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
#source("application_global.R")

# installation de la librairie  dynamicTreeCut : install.packages("dynamicTreeCut")
library(dynamicTreeCut)




# ********************************************************
# BUILDING THE DATA FRAME TO BE USED IN ACP
# ********************************************************

contains <- function (list_of_objects , item){ 
  if(length(list_of_objects) > 0){
    for( i in 1:length(list_of_objects)){
      if(identical(list_of_objects[[i]], item)){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

T5<-Sys.time()
col <- list()
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
      col1 <- number_of_columns + 1
      col2 <- number_of_columns + 2
      
      # adding the link in the list (no need to add it twice):
      col[number_of_columns/2 +1] <- link
      
      # building column name :
      col_to_string[col1] <- paste("||" , link@property@name)
      col_to_string[col2] <- paste("||" , link@property@name ,"||", link@object@name)
      
      # increment :
      number_of_columns <- number_of_columns + 2
    }
  }
}


# ***************************************************************************************************************
# ***************************************************************************************************************
#
#
# HCLUST & DYNAMIC TREE CUT
#
#
# ***************************************************************************************************************
# ***************************************************************************************************************


# ********************************************************
# CREATING THE EMPTY MATRIX
# ********************************************************
data <- matrix(NA, length(list_of_subjects), number_of_columns)
colnames(data) <- col_to_string
rownames(data) <-  row_to_string




# ********************************************************
# BUILDING THE BINARY MATRIX
# ********************************************************

for (j in 1:length(list_of_subjects)){
  # selecting the current subject :
  subject <- list_of_subjects[[j]] 
  
  for (i in 1:length(col)){
    # select the current link :
    link <- col[[i]]
    
    col_num <- i*2-1
    if(containsPropertyAndObject(subject@links, link)){
      data[j,col_num] <- 1
      data[j,col_num+1] <- 1
    } else if(containsProperty(subject@links, link)){
      data[j,col_num] <- 1
      data[j,col_num+1] <- 0
    } else {
      data[j,col_num] <- 0
      data[j,col_num+1] <- 0
    }
  }
}
binary_matrix <- data




# ********************************************************
# BUILDING THE DISTANCE MATRIX
# ********************************************************

dist_matrix = dist(binary_matrix, method = "binary")




# ********************************************************
# USING HCLUST TO BUILD THE DENDOGRAM
# ********************************************************

dendogram <- hclust(dist_matrix)
plot(dendogram)




# ********************************************************
# DYNAMICALLY DEFINING MINIMUM CLUSTER SIZE
# ********************************************************
number_of_results = length(data[,1])
maximum_number_of_cluster = 10
minClusterSize = number_of_results / maximum_number_of_cluster




# ********************************************************
# BUILDING CLUSTERS USING A DYNAMIC CUT
# ********************************************************
clusters <- NULL
#clusters = cutreeDynamic(dendogram, cutHeight = NULL, minClusterSize = 20,distM = as.matrix(dist_matrix), pamStage = FALSE)
#clusters = cutreeDynamic(dendogram, distM = as.matrix(dist_matrix))
clusters = cutreeDynamic(dendogram, minClusterSize=minClusterSize, method="hybrid",  distM = as.matrix(dist_matrix))
plot(clusters)