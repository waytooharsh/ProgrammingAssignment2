#The code consists of two functions: one for creating a matrix and
#one for solving the inverse of the matrix and retrieving results
#from the cache if it has been computed before

## makeCacheMatrix creates a matrix with relevant setter and getter functions
#for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Setter child function
  set <- function(val) {
    x <<- val
    
    inv <<- NULL
  }
  #Getter child function
  
  get <- function() x
  #Set and Get for inversion
  invSetter <- function(inverse) inv <<- inverse
  invGetter <- function() inv
  #Create a list to represent the matrix
  list(set = set, get = get,invSetter = invSetter,invGetter = invGetter)

}


#cacheSolve function fetches cached results of previously computed inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversed <- x$invGetter()
  flag <- is.null(inversed)
  if(flag==FALSE) {
    message("Retrieving data from cache")
    return(inversed) #Returning cached matrix inverse
  }
  inversibleMat <- x$get()
  inversed <- solve(inversibleMat, ...)
  x$invSetter(inversed)
  inversed
}

