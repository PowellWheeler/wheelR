#' @title Reveal what's in your workspace.
#'
#' @description Function prints the libraries currently loaded and all the objects available in the global environment. The objects are organized by their container type.  Vectors are further organized by their data class.
#'
#' @return  Technically returns a NULL vector b/c I can't figure out how to force R to not return an object. However, the intent of the function is just print to the screen.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @examples
#' revelio()
#'
#' @export

revelio <- function(...){

baseLibraries <- c("base", "datasets", "graphics", "grDevices", "methods", "stats", "utils")

cat("\n\n The following base-R libraries are loaded into the Workspace:\n")
print(sort(.packages()[.packages() %in% baseLibraries]))

cat("\n\n The following user-added libraries are loaded into the Workspace:\n")
print(sort(.packages()[!.packages() %in% baseLibraries]))

objectsVector <- ls(envir = .GlobalEnv) #functions happen in their own environment, so I have to tell it so get the objects from the Global Environment

if (length(objectsVector) == 0){
  cat("\n\n There are no objects in the Global Environment.\n")
  print("Such lonely")

}else{
  cat("\n\n The following objects are in the Global Environment:\n\n")

  df <- data.frame(class = character(length(objectsVector)), object = objectsVector, stringsAsFactors = FALSE)

  #this for loop goes through and finds the class of each object
  for (index in seq_along(objectsVector)){
    df[index,1] <- class(get(objectsVector[index]))[1] #Notice that objects.vector[index] is a string, 'get' tells R that I want the object that matches the string. Also ggplot graphs have two classes (??????), so only get the first class.
  }

  #better class names for vectors - I think this is all the classes
  df$class <- replace(df$class, df$class == 'Date',      'vector_Date')
  df$class <- replace(df$class, df$class == 'character', 'vector_Character')
  df$class <- replace(df$class, df$class == 'factor',    'vector_Factor') #apparently factors are a data structure that is on top of a data type (integer) - wtf?
  df$class <- replace(df$class, df$class == 'numeric',   'vector_Numeric')
  df$class <- replace(df$class, df$class == 'integer',   'vector_Integer')
  df$class <- replace(df$class, df$class == 'logical',   'vector_Logical')
  df$class <- replace(df$class, df$class == 'complex',   'vector_Complex')

  df <- df[order(df$class, df$object),] #sort by class then object - suprisingly this order keeps everything right for the rest of the function
  rownames(df) <- NULL #re-set the row numbers
  classVector <- unique(df$class) #make a vector of the classes in the global environment
  vectorOfVectorNames <- vector() #initalize a vector

  ## Make individual vectors of the objects in all the classes
  for (index in seq_along(classVector)){
    tempDf <- df[df$class == classVector[index],] #make a temp df just of the class this loop is focused on
    tempVec <- as.vector(tempDf$object) #make a vector of the object name that this vector is focused on
    outputVectorName <- toupper(paste(classVector[index])) #make vectors with names like 'data.frame'
    vectorOfVectorNames[index] <- outputVectorName #keep up with the names of the vectors
    assign(outputVectorName,tempVec) #make vectors with names like 'data.frame.vector'
 }

outputList <- mget(vectorOfVectorNames) # this is the BOMB - makes a list from the names of vectors

print(outputList)

cat("\n\n")

NULL
cat("\r") #erase the line that prints 'NULL'
}
}
