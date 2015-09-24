## This R script create 2 functions to compute the inverse of a matrix. This is 
## achieved with 2 functions. This first function providess methods to set and get
## a matrix and its inverse from memory. The second method computes the inverse for
## a matrix. This checks if the inverse i already computed and in the memory, to 
## avoid unnecessary computation.

## Functionm makeCacheMatrix(). provides set of 4 functions. 2 functions to get 
## and set a matrix and 2 functions to get and set the inverse of the matrix.
## Returns the list of callable functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y=matrix()) {
    x <<- y
    i <<- NULL
    get()
  }
  get <- function() x
  setinverse <- function(inv = matrix()) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function cacheSolve(), computes the inverse of a quare matrix. This function first 
## checks if the inverse is in the memory variable  i. if not, then it 
## computes the inverse of the matrix and stores it in the memory varriable i
## This way,it avoids computing inverse if already in memory.
## Returns the inverse of the input matrix.Indicates with a message iff the inverse 
## is from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <-  x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setinverse(i)
  i
}
