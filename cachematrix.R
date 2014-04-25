## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function is mainly used to create and retrieve both a matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL # we are telling R that 'm' is an empty vector
  set <- function(y) { # Here, we are setting up the contents in the matrix
    x <<- y  # this tells R that 'x' is 'y'
    m <<- NULL # this tells R to make 'm' an empty vector (NULL) each time we set up the contents in the matrix
  } 
  get <- function() x # this retrieves the matrix 
  setinverse <- function(inverse) m <<- inverse # this sets up the inverse of the matrix
  getinverse <- function() m # this retrieves the inverse of the matrix
  list(set = set, get = get, #this tells R that the 4 functions(set, get, setinverse, and getinverse) are all under the class "list". Also, we are telling R that each function equals itself
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve function is mainly used to see if the inverse matrix of the matrix made from the makeCacheMatrix
## was calculated yet. If an inverse matrix already exists, then, this function returns the inverse matrix. If
## it doesn't exist yet, then it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()           # we are telling R call the inverse matrix ('m') should be found from 'x$getinverse()'        
  if(!is.null(m)) {           # this asks whether 'm' from 'x$getinverse' was 'NULL' or an actual inverse matrix
    message("getting cached data!") # if there is an actual inverse matrix, then R will produce the message: "gettig cached data!"
    return(m)                # and also to return the inverse matrix
  }
  data <- x$get()             # However, if there was no cache, then 'm' needs to be solved. So, we create a new variable 'data' and say that it equals the original matrix, 'x$get()'
  m <- solve(data, ...)        # Using the original matrix, we find the inverse matrix ('m') by using the "solve()" function
  x$setinverse(m)             # Now, that we have the inverse matrix, we now have the 'm' value, which we can plug back into the 'x$setinverse' function to set up the inverse matrix
  m                           # Now, we can print out the inverse matrix

}
