##These functions create, store and recall the matrix and its inverse from cache

## Write a short comment describing this function
## makeCacheMatrix creates custom matrix type capable of running four functions
## set() store the matrix in cache whereas get() recall the matrix

makeCacheMatrix <- function(x = matrix()){    
    m <- NULL
    set <- function(y){
    x <<- y  
    m <<- NULL #store matrix in cache 
    }
    get <- function() x 
    setInverse <- function(solve) m<<- solve 
    getInverse <- function() m 
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)  ## create a list of functions
}


## cacheSolve take a custom matrix type created by the makeCacheMatrix function and calculates the inverse matrix of it

cacheSolve <- function(x, ...) {
   
    m <- x$getInverse() #query the x matrix's cache
    if(!is.null(m)){     
   
    message("getting cached data")  # send message indicating this is just cache 
    return(m) # return the cache  
    }
    data <- x$get()                     
    m <- solve(data, ...)              
    x$setInverse(m)         
}
