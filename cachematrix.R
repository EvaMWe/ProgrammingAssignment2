## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL                      #initializing an object for the inverse matrix in later code
  
  set <- function(y) {            #setter, sets stored matrix, set inverse matrix to NULL
    x <<- y
    invers <<- NULL
  }
  
  get <- function() x            #getter, get stored matrix
  
  setinv <- function(inv) invers <<- inv   #setter 2, sets calculated inverse matrix
  getinv <- function () invers             #getter 2, accessor for the inverse matrix
  
  #output
  list( set = set, get = get,                       
        setinv = setinv, getinv = getinv)
}
  
 


## calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #check if inverse matrix already exists and return i
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
  # get stored matrix
    dataMat <- x$get()
    
  # check if it'S a square matrix
    if(nrow(dataMat)!= ncol(dataMat)){
      message("this is not a square matrix")
      return(NULL)
    }
    
  # check if matrix has a determinant!= 0
    if(det(dataMat) == 0){
      message("not an invertible matrix")
      return(NULL)
    }
    
  # compute the inverse and store it in object
    inv <- solve(dataMat, ...)
    x$setinv(inv)
    inv
  }
  

