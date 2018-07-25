# The following two functions: "makeCacheMatrix" and "cacheSolve" 
# are used to cache the inverse of a matrix.

# The makeCacheMatrix function below creates a list containing a function to do the following:
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of inverse of the matrix
# 4) get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# "cacheSolve," the function below returns the inverse of the matrix. 
# cacheSolve first checks to determine if the inverse has already been calculated. 
# If that is the case, the function gets the result and skips the computation. 
# Otherwise, it calculates the inverse of the matrix, then sets the value in the 
# cache using the "setinv" function.

# It is assumed by this function that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Getting Cached Data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

# Below is a test to see if the functions are working properly
x <-rbind(c(2, -1/2), c(-1/2, 2))
b <-makeCacheMatrix(x)
b$get()

# The result we get is the following:
#     [,1] [,2]
#[1,]  2.0 -0.5
#[2,] -0.5  2.0

# When we run the function "cacheSolve" for the first time, there
# is no cache in the first run

cacheSolve(b) 

# Thus, the output we get is the following:
# [,1]      [,2]
# [1,] 0.5333333 0.1333333
# [2,] 0.1333333 0.5333333

cacheSolve(b)  

# When we run the function "cacheSolve" the second time, it retrieves the
# cache and prints out the following output shown below:

# Getting Cached Data...
# [,1]      [,2]
# [1,] 0.5333333 0.1333333
# [2,] 0.1333333 0.5333333

#Therefore, the code is written properly as desired.
