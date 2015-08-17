## This function returns a list containing functions to
##   1. set the matrix
##   2. get the matrix
##   3. set the inverse
##   4. get the inverse
## This function does not compute the inverse of the matrix.
## Rather is used to as input for the function that computes inverse
## and uses the list for caching the inverse computed

makeCacheMatrix <- function(x = matrix()) {
       
       ## Variable to hold the inverse of the input matrix
       inverseMatrix <- NULL
       
       ## Function to set the input matrix
       ## Also resets the inverse of matrix to null on new input
       setMatrix = function(y){
              x <<- y
              inverseMatrix <<- NULL
       }
       
       ## Function that returns the input matrix
       getMatrix = function(){
              return (x)
       }
       
       ## Function to set the inverse matrix
       setInvMatrix = function(inverse){
              inverseMatrix <<- inverse
       }
       
       ## Function that returns the inverse matrix
       getInvMatrix = function(){
              return (inverseMatrix)
       }
       
       ## List contaning refernce to all functions 
       list(setMatrix=setMatrix, getMatrix=getMatrix, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}


## Function that returns inverse of the  matrix input 
## to makeCacheMatrix function. 
## This method returns  the cached inverse, if available.
## Else computes the inverse, caches the inverse , and returns it
## The function assumes that the cache is a square, invertible matrix 
cacheSolve <- function(x, ...) {

       ## Return a matrix that is the inverse of 'x'
       inverse_x <- x$getInvMatrix()
       
       if (!is.null(inverse_x)) {
              message("Getting the cached inverse matrix")
              return(inverse_x)
       } else {
              inverse_x <- solve(x$getMatrix())
              x$setInvMatrix(inverse_x)
              return(inverse_x)
       }
}
