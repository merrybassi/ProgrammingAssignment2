makeCacheMatrix <- function(x = numeric()) {    # input will be a vector
        m <- NULL   # m will be the matrix and it's reset to Null every
                    # time makeCacheMatrix is called
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x    # this function returns the value of the original vector
        setinv <- function(inv) m <<- inv     # this is called by cacheSolve during the first cacheSolve
                                              # access and it will store the value using superassignment
        getinv <- function() m      # returns the cached value to cacheSolve() on 
                                    # subsequent accesses
        list(set = set, get = get,    # this is accessed each time makeCacheMatrix() is called (each time
             setinv = setinv,         # we make a new object).   This is a list of the internal functions
             getinv = getinv)         # ('methods') so a calling function knows how to access those methods.
}

cacheSolve <- function(x, ...) {    # the input x is an object created by makeCacheMatrix
  m <- x$getinv()                   # accesses teh object 'x' and gets the values of the matrix
  if(!is.null(m)) {                 # if the matrix was already cached (not Null)...
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the matrix ..."return" ends
  }                                 # the function cacheSolve(), note
  data <- x$get()                   # we reach this code only if x$getinv() returned NULL
  m <- solve(data, ...)             # if m was NULL then we inverse the matrix
  x$setinv(m)                       # store the inverse matrix value in x (see setinv()) in the makeCacheMatrix
  m                                 # returns the inverse matrix to the code that called this function
}