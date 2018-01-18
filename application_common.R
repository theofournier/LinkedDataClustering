# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("D:/Dropbox/Yassin/ESILV/A4/RESEARCH/R/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
source("classes.R")

# installation de la librairie  SPARQL : install.packages("SPARQL")
library(SPARQL)




# ********************************************************
# SPARQL ENDPOINT
# ********************************************************

endpoint <- "http://dbpedia.org/sparql"
# endpoint <- "http://localhost:8890/sparql"




# ********************************************************
# SPARQL QUERY
# ********************************************************

query <- "PREFIX db: <http://dbpedia.org/resource/>
PREFIX dbo: <http://dbpedia.org/ontology/>
SELECT DISTINCT ?oeuvre
WHERE {
?oeuvre dbo:author ?j.
#?oeuvre rdfs:label ?titre.
?oeuvre rdf:type  dbo:Book.
} LIMIT 10"




# ********************************************************
# EXECUTING THE QUERY AND SELECTING THE RESULT
# ********************************************************

QueryResult <- SPARQL(endpoint,query)       # Exécution de la requête "query" via le endpoint

dataFrameResult <- QueryResult$results    # Enregistrer les résultats de la requête dans un data frame




# ********************************************************
# CREATING A LIST OF ALL RESULT OBJECTS
# ********************************************************
T1<-Sys.time()
list_of_subjects = list()
for (i in 1:length(dataFrameResult)){
  new_obj <-  Object(name=dataFrameResult[1,i], links = list())
  list_of_subjects[i] <- new_obj
}
T2<-Sys.time()



# ********************************************************
# Fonction permettant de r�cup�rer le voisinage d'un objet
# ********************************************************
RequestNeighborhood<-function(QueryResult, endpoint){
  
  if (grepl("%", QueryResult) ){
    
  }
  else{
    queryvoisinage <- paste("SELECT DISTINCT ?b ?c
                            WHERE{
                            {",
                            QueryResult, "?b ?c.
                            FILTER ( !( regex (?b, 'abstract', 'i') || regex ( ?b, 'comment', 'i')))
                            FILTER isIRI (?c)
                            } UNION
                            {",
                            QueryResult , " ?b ?c.
                            FILTER ( !( regex (?b , 'abstract', 'i') || regex ( ?b, 'comment', 'i') ))
                            FILTER isLiteral(?c)
                            FILTER ( lang(?c) = 'en')
                            }
                            }")

    QueryResultFrame <- SPARQL(endpoint, queryvoisinage)  # pour chaque résultat de la requete initiale
    
    results <- QueryResultFrame$results # ici nous aurons que les résultats de SELECT
  }
  return (results)
}
#obj <- list_of_subjects[[1]] 
#results <- RequestNeighborhood(obj@name, endpoint)




# ********************************************************
# CREATING LINKS OBJECTS FROM NEIGHBORHOOD
# ********************************************************
T3<-Sys.time()
for (j in 1:length(list_of_subjects)){
  # selecting the current subject :
  subject <- list_of_subjects[[j]] 
  
  
  # getting the subject's neighborhood :
  neighborhood <- RequestNeighborhood(subject@name, endpoint)
  
  # creating a list of links :
  list_of_links <- list()

  # for each link in the neighborhood :
  for (i in 1:length(neighborhood[,1])){
    # we create a predicate :
    new_obj_predicate <-  Object(name=neighborhood[i,1], links = list())
    
    # we create an object :
    new_obj_object <-  Object(name=neighborhood[i,2], links = list())
    
    # we create a link using the predicate and the object :
    new_link <- Link(property = new_obj_predicate, object = new_obj_object)
    
    # we add the link in list dedicated list :
    list_of_links[i] <- new_link
  }

  # we affect the list to the subject :
  subject@links <- list_of_links
  
  # we update the list of subjects :
  list_of_subjects[[j]] <- subject
}
T4<-Sys.time()
