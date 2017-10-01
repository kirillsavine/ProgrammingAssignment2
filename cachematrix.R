#############################
# FUNCTIONS DEFINITION
#############################


# makeCacheMatrix() created an object conaining a list of functions as follows:
#   - set(): sets the value of the matrix
#   - get(): gets the value of the matrix
#   - setinverse(): sets the value of the inverse matrix
#   - getinverse(): gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
	set <- function(y) {
        x <<- y
        inv <<- NULL
    }
	
	get <- function(){x}
    
	# sets inverse in parent environment
	setinverse <- function(inverse){inv <<- inverse}
    
	# returns inverse
	getinverse <- function(){inv}
    
	res=list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
	
	return(res)
	
}


# cacheSolve() produces an inverse of a matrix created with makeCacheMatrix()
# it uses cached version of the matix if available, othewise calculates inverse

cacheSolve <- function(x,...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
	message("calculating inverse matrix")
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    return(i)
}


#############################
# EXAMPLE
#############################

# sample matrix
my_mat=matrix(c(1,2,3,4), 2,2)

# make cache matrix
m <- makeCacheMatrix(my_mat)

# return inverse (use cached inverse if available)
cacheSolve(m)

