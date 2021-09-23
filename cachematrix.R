## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
## set the elements of the matrix
## get the elements of the matrix
## set the elements of the matrix inverse
## get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The following function calculates the inverse of the special “matrix” created with the above function.
## it first checks if the inverse is already present , if yes then prints the inverse from the cache 
## Else, it calculates the inverse of the matrix and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##matdat <- c(2,4,7,1,5,9,3,6,10)
##m <- matrix(matdat, nrow = 3, ncol = 3)
##m
##m1 <- makeCacheMatrix(m)
##m1
##cacheSolve(m1)
