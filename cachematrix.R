## The following two functions cache the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      
      invM <- NULL                               # initialise inverse property
      set <- function(y) {                       # set the matrix  
              x <<- y
              invM <<- NULL
      }
      get <- function() x                       #get the matrix
      setInverse <- function(inverse) invM <<- inverse #set the inverse matrix
      getInverse <- function() invM                    #get the inverse matrix
      list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {  
       
    invM <- x$getInverse()                             ## Return a matrix that is the inverse of x
    if(!is.null(invM)){                                #return the inverse matrix if already set
        message("getting cached data")                 
        return(invM)                                   
    }
    
    data <- x$get()                                    #get the matrix data
    invM <- solve(data)                                #use solve function to inverse matrix
    x$setInverse(invM)                                 #set the inverse matrix
    invM                                               #return the inverse matrix
    
}
