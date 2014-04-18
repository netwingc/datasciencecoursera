## This function takes a square matrix as input variable
## It will return a list of 4 functions
## 2 setters and 2 getters
## With these functions a 'matrix/inverse matrix' pair can be created 
## And the 'matrix/inverse matrix' pair will be saved/retrieved in/from the cache


makeCacheMatrix <- function(mat = matrix()) {

    inv <- NULL  ##variable to hold the inverse matrix    
##  setter for the input matrix variable    
    set <- function(y) {
      mat <<- y
##  input variable get new value, reset the variable that holds the calculated inverse     
      inv <<- NULL
    }
##  getter for the input matrix variable
    get <- function() {mat}
##  setter for the calculated inverse
    setinv <- function(inverse) {inv <<- inverse}
##  getter for the calculated inverse
    getinv <- function() {inv}
##  return the result as a list of functions
    list(set=set, get=get,
         setinv = setinv,
         getinv = getinv)
    
}


## This functions takes a list of functions(pointers) 
## create by the function makeCacheMatrix as input variable
## It calculates and returns the inverse matrix of the matrix which can
## be retrieved by the get function from the function list.
## The 'matrix/inverse matrix' pair will be saved in the cache

cacheSolve <- function(mat, ...) {
        
  inv <- mat$getinv()  ## try to retrieve the inverse matrix
  if (!is.null(inv)) { ## inverse matrix was in the cache!!  
        message("getting cached data")
        return(inv)
  }
## inverse matrix was not in the cache  
  data <- mat$get()  ## get the input matrix
  inv <- solve(data, ...)  ## calculate the inverse matrix
  mat$setinv(inv)  ## save the inverse matrix in cache
  inv
  
}
