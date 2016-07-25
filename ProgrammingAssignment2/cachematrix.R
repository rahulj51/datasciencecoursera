
## The functions below create a mecahnism to cache the inverse of a matrix for reuse since 
## inverse is a costly operation, specially for large matrices.



## This function returns a list of functions to help construct a special "cacheable matrix"  
## with an ability to cache some info (e.g., matrix inverse) for the matrix
makeCacheMatrix <- function(x = matrix()) {

    cachedValue <- NULL
    set <- function(y) {
        x <<- y
        cachedValue <<- NULL
    }
    get <- function() x
    setCache <- function(cachedValue) cachedValue <<- cachedValue
    getCache <- function() cachedValue
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache)
}



## This function takes the cacheable-matrix as input and computes the inverse.
## It first checks the matrix's cache for the inverse value. If the inverse wasn't cached,
## it proceeds to compute the inverse using 'solve' function and then stores the value in 
## the cache for future retrieval.
cacheSolve <- function(x, ...) {
    
    inv <- x$getCache()
    if (! is.null(inv)) {
        message("fetching from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setCache(inv)
    inv
}
