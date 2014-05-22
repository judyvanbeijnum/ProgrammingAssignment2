
## Two functions are present, one that stores the inverse of a matrix in a 'cache'
# in order for the second function to check whether it is already present so it does 
# not have to calculate it again.

# First a function to store m in a cache. Create components that are only accessible within the function, not in the global environment


makeCacheMatrix <- function(x = matrix()) {     
      m <- NULL                                 # Set an empty variable m within the makeCacheMatrix function
      set <- function(y) {                      # Condtional setting of variable environment; components of the function
            x <<- y
            m <<- NULL
      }
      get <- function() x                       
      setCache <- function(solve) m <<- solve    
      getCache <- function() m
      list(set = set, get = get,
           setCache = setCache,
           getCache = getCache)
}


# The inverse of a matrix, determined by the solve() function is either retrieved from cache or computed

cacheSolve <- function(x, ...) {
      m <- x$getCache()                 # When function cacheSolve is called, it seeks for cached value of m from makeCacheMatrix function
      if(!is.null(m)) {                   # When m is not null (i.e. is present), cached version of m is retrieved
            message("getting cached data")
            return(m)
      }
      data <- x$get()                     # If no data in cache, the matrix is retrieved ...
      m <- solve(data, ...)               # ... and the inverse is calculated..
      x$setCache(m)                       # ...and subsequently stored as cache...
      m                                   # .. and then returned
}


# Validations of matrix functions- some examples

x <- 2000
m <- matrix(runif(x^2),x)
c <- makeCacheMatrix(m)
cacheSolve(c) # when running the first time, it takes several seconds to retrieve m; but running for the second time and retrieving from scratch, less than a second



b <- makeCacheMatrix(matrix(rnorm(1:9), 3,3))
b$get()         # Returns original matrix
cacheSolve(b)   # Computes, caches, and returns    matrix inverse
b$getCache()    # Returns cached matrix inverse using previously computed matrix inverse

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getinverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinverse()  # Returns matrix inverse