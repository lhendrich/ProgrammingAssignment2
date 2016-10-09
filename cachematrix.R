## Create a structure to cache the inverse of a matrix.
##      Function makeCacheMatrix creates the structure used to hold the inverse
##      Function cacheSolve creates a strucure to either retrieve the stored
##           inverse or compute and store it for later.

## Function makeCacheMatrix creates the structure to save an retrieve a stored 
##      matrix inverse.  There are 5 methods:
##           $set - sets the matrix to be inversed, and clears the inverse
##           $get - retrieves the original matrix
##           $setinv - sets the inverse matrix to be stored
##           $getinv - retrives the stored inverse matrix
##           $list - list the current structure

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## $set the initial matrix
    set <- function(y) {
         x <<-y
         inv <<- NULL
    }
    
    ## $get the initial matrix
    get <- function() x
    
    ## $setinv - set the inverse matrix
    setinv <- function(solve) inv <<- solve
    
    ## $getinv - get the inverse matrix
    getinv <- function() inv
    
    ## $list - list the data structure
    list(set = set, 
         get = get, 
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve creates the structure to retrive or compute a matrix inverse.
##      The stored matrix inverse is checked first.  If the stored value is not null, 
##      the stored inverse is returned.  If the stored value is null, an inverse is 
##      computed and stored for later retrieval before being returned.

cacheSolve <- function(x, ...) {
  
    ## retrieve the stored inverse
    inv <- x$getinv()
    
    # If the inverse is not null (ie pre-computed) return inverse
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    } else {
    # Else compute the matrix inverse, store and return it      
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      return(inv)
    }
}
