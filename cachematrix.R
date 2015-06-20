## Defines functions to demonstrates use of cache to curtail computation time
##  in matrix inverse calculations.

# makeCacheMatrix creates a a special matrix which is really a list containing 
# functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
  
  inv <- NULL
  
  set <- function(temp)
  {
        x <<- temp
  
      inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

# cacheSolve function returns the inverse of the "special" matrix created using makeCacheMatrix function.
# It makes use of cache to store results. If the inverse has already been computed it 
# fetches the result and skips the computation otherwise it computes the inverse of the matrix 
# using "solve" function and sets the value in the cache via setinverse function associated with the matrix.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    
    message("Fetching inverse matrix from cached data")
  
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  return(inv)
}
