## Here we create a "matrix" using code that essentially models verbatim the example function which creates a 
## special "vector" provided in the Programming Assignment 2 instructions.  The code defines a function to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse of the matrix (using the "solve" function)
##  4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}

## Here we obtain the inverse of the special "matrix" created by function makeCacheMatrix above.
## The code here in function "cacheSolve' follows essentially verbatim the code in the example function "cachemean" 
## in the instructions for Programming Assignment 2.

## It checks to see if the inverse of the special "matrix" has already been calculated, and if it has been calculated
## previously, the function retrieves the inverse from cache.  

## If the inverse has not been previously calculated, the function first checks to see if the special "matrix" is 
## invertible (i.e., whether it has an inverse).  It does this by testing whether the determinant of the "matrix" is
## is zero (using the "det" function). [I recognize that we assume that the "matrix" passed to cacheSolve will have an
## inverse, but I thought it can't hurt to check (it allowed me to play a bit with the "det" function).]

## If the determinant is zero, the "matrix" is not invertible (aka it is a singular matrix)
## and the function simply issues a message to that effect and returns.  Otherwise, the function uses the "solve" function
## to calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
       if (!is.null(m))  {
       	       message("getting cached data")
       	       return(m)
       }               
       data <- x$get()
       if (det(data) == 0) {message("The matrix is not invertible. Determinant is 0."); ## Determine if the matrix has an inverse
                            return};  
       m <- solve(data, ...)
       x$setInverse(m)
       m	       
               
}


