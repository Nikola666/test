## inverse of a matrix is calculated
## 1. it takes an argument x of type matrix
## 2. it returns a list with 4 list items 
a <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve returns a matrix that is the inverse of 'x'
## if inverse was calculated by makeCacheMatrix then it grabs the value from it
## else cacheSolve calculates the inverse
cacheSolve <- function(x, ...) {
  m <- x$getinv()           #query the x matrix's cache         
  if(!is.null(m)) {           #if there is a cache
    message("getting cached data") 
    return(m)                #just return the cache, no computation needed
  }
  data <- x$get()             #if there's no cache
  m <- solve(data, ...)        #we actually compute them here
  x$setinv(m)                #save the result back to x's cache
  message("calculate inverse")
  return (m)                          #return the result
  
}