## Creates a new "object" with some getters and setters, for get/set a matrix, and get/set its inverse
makeCacheMatrix <- function(x = matrix()) {
          #x remains defined in the scope of this function until the destroy of the value
          #thats the magic of the lexical scope
          inv <- NULL #In the first run, sets the inverse as null
          set <- function(y) { #Sets a new value for the matrix x, also sets in null the inverse
                    x <<- y
                    inv <<- NULL
          }
          get <- function() x  #get the matrix x
          setinv <- function(i) inv <<- i #set the inverse field
          getinv <- function() sol #get the inverse field
          list(get = get, set = set, #returns the list of funcions, like a interface
               getinverse = getinv, setinverser = setinv) 
}

## Given a object created with makeCacheMatrix, it set the inverse of the matrix, and get it
## or get the cached inverse, in which case prints a message in stdout
cacheSolve <- function(x, ...) {
          #If x has the inverse cached, return the cached
          sol <- x$getinverse()
          if(!is.null(sol)) {
                    message("getting cached inverse")
                    return(sol) #With return, the function ends returning the value passed
          }
          data <- x$get() #get the matrix of the object x
          sol <- solve(data) #Long computation, the inverse is computed
          x$setinverse(sol) #set the cache inverse in x
          sol ## Return a matrix that is the inverse of 'x'
}
