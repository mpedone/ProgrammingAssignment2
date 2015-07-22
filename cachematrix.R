## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix defines four functions used for caching the inverse value of a
# matrix and then for retrieving that cached value later; it stores these 
# functions in a list that can be subsetted to call different functions. Each
# time this function is called, it clears any previously cached solution.
# 
# cacheSolve checks to see if there is a cached solution, and if there is, 
# returns that solution. If not, it calculates the solution and stores that 
# value in the cache. If this function is called again before makeCacheMatrix, 
# this cached solution is returned.

## Write a short comment describing this function

# The first thing this function does is uncache any previously stored solutions.
# Then, it creates the set, get, setInverse, and getInverse functions. Finally, 
# it creates a list containing the names of these functions so that they can be 
# called later. set is used to change the matrix to be inverted. get displays the matrix cached in x.
# setInverse allows the user to explicitly set the cached solution matrix.
# getInverse displays the cached solution.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)

}


## Write a short comment describing this function

# cacheSolve takes as its argument the output from makeCacheMatrix and returns 
# the inverse of the matrix that had been passed to that function. First, it 
# checks to see if there is a solution already cached in m via the getInverse 
# function. If a solution is cached, this value is returned, and the function 
# exits. If not (the value of m in NULL), the matrix to be solved is stored in 
# the variable data, which is then passed to the solve() function. Next, this 
# solution is cached in m using the setInverse function. If getInverse is
# called, or if cacheSolve is called for the same variable again, this value of
# m will be returned without the function solve() running again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
