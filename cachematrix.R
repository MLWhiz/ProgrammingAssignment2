## Put comments here that give an overall description of what your
## functions do

## Creates a list of set,get,setmatrix,getmatrix functions for an input matrix and outputs them as list

makeCacheMatrix <- function(x = matrix()) {
					m <- NULL
					  set <- function(y) {
						x <<- y
						m <<- NULL
					  }
					  get <- function() x
					  setmatrix <- function(inverse) m <<- inverse
					  getmatrix <- function() m
					  list(set = set, get = get,
						   setmatrix = setmatrix,
						   getmatrix = getmatrix)

}


## Gives the inverse of a matrix if it is cached else calculates it using solve() and sets the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
					m <- x$getmatrix()
					  if(!is.null(m)) {
						message("getting cached data")
						return(m)
					  }
					  data <- x$get()
					  m <- solve(data)
					  x$setmatrix(m)
					  m
}
