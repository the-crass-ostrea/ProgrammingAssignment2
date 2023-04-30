## Functions included are "makeCacheMatrix" and "cacheSolve".
## "makeCacheMatrix" takes a matrix and stores it for later reference, as well as 
## creating several variables for later use.
## "cacheSolve" takes the saved matrix, calculates its inverse matrix, and stores it
## in one of the pre-made variables. 
## If the inverse matrix has already been calculated and stored, "cacheSolve" will
## not recalculate the matrix but will instead retrieve it from the stored variable.


#Initial code to set up a test matrix and its inverse
matrix <- rbind(c(2,3), c(3,2))
matrix
solve(matrix)

## makeCacheMatrix function - function stores new matrix in variable saved to the global environment.
## Function additionally creates a saved state of original matrix ("recall_matrix") and
## creates a variable to recall the inverse matrix ("recall_inverse"). Function first checks if
## "recall_inverse" has already been created and creates the new variable if not. If it already has been created
## "makeCacheMatrix" will reset "recall_inverse" to NULL. 
makeCacheMatrix <- function(x = matrix()) {
  new_matrix <- x
  if(exists("recall_inverse") == FALSE){
    recall_inverse <- NULL
    assign("recall_inverse", recall_inverse, envir = .GlobalEnv)
  }else{
    if(sum(recall_inverse != solve(matrix)) != 4){
      recall_inverse <- NULL
      assign("recall_inverse", recall_inverse, envir = .GlobalEnv)
    }
  }
  recall_matrix <- new_matrix
  assign("recall_matrix", recall_matrix, envir = .GlobalEnv)
}

##Code to run function with sample matrix labeled "matrix" and to check status of created variables
makeCacheMatrix(x = matrix)
recall_matrix
recall_inverse

## cacheSolve function - function uses original matrix input, assesses for presence of inverse matrix
## and calculates inverse matrix if not already extant. Function prints text to delineate whether
## it is calculating the inverse matrix or retrieving the inverse matrix. The inverse matrix
## is additionally saved to the global environment as "recall_inverse" until the "makeCacheMatrix"
## function is used again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_to_use <- x
  if(!is.null(recall_inverse)) {
    message("retrieving cached inverse")
    return(recall_inverse)
  }
  message("calculating inverse & storing")
  data <- recall_matrix
  recall_inverse <- solve(data, ...)
  assign("recall_inverse", recall_inverse, envir = .GlobalEnv)
  recall_inverse
}

#Test code to run "cacheSolve". Run twice in a row to see initial calculation vs retrieval. 
cacheSolve(matrix)


