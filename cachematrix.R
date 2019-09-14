# Write a short comment describing this function
# create a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL # set the cache to null
  set <- function(y) {
    x <<- y
    i <<- NULL
  } # set function to set new value to the matrix
  get <- function() x # get value of the matrix
  setinverse <- function(inverse) i <<- inverse # set the inverse matrix 
  getinverse <- function() i # get the inverse matrix
  list(set = set, get = get,
       setinverse =  setinverse,
       getinverse = getinverse) # get a list of function to be called in cacheSolve
}


## Write a short comment describing this function
## calculate the inverse matrix when there is no inverse matrix in the cache
cacheSolve <- function(x,...) {
  i <- x$getinverse() #get the inverse matrix first
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } # test whether there is an inverse matrix
  data <- x$get() ## if no, get the original matrix
  i <- solve(data, ...) ## solve the inverse matrix
  x$setinverse(i) ##set the inverse matrix
  i ## return the inverse matrix
}

## test
a<- makeCacheMatrix(A)
cacheSolve(a)