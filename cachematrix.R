makeCacheMatrix <- function(x = matrix()) {
  #this function is for creating the matrix, setting and getting the value
  #of matrix and setting and getting the inverse of the matrix
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  #the function does try to fetch the inverse of the matrix 
  #if it really exists it gets it from the cache with a message and returns
  #if not calculates and sets the inverse itself
  m <- x$getInv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  #we can use solve for inversing
  x$setInv(m)
  m
}