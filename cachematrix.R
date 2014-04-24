
## The following functions are the solution for the Coursera
## R Programming Assignment 2 - Caching the inverse of a matrix


###
#
#  makeCacheMatrix
#
#  The funtion makeCacheMatrix creates an matrix object to store
#  the matrix(x) and the calculated inversed matrix(m_inv).
#  
#  %history 2014/04/24, hv, initial revision
#
###
makeCacheMatrix <- function(x = matrix())
{
   # reset cache matrix
   m_inv <- NULL

   # define functions
   set <- function(y)
   {
      x     <<- y       # set original matrix to x
      m_inv <<- NULL    # reset inverted matrix
   }
   get         <- function() x
   setinverse  <- function(inverse) m_inv <<- inverse
   getinverse  <- function() m_inv

   # define the 
   list(
        set = set
      , get = get
      , setinverse = setinverse
      , getinverse = getinverse
       )
}


###
#
#  cacheSolve
#
#  The funtion cacheSolve computes the inverse of the matrix x
#  To minimize spent time and spare hardware resources the inverse
#  is cached.
#  
#  %history 2014/04/24, hv, initial revision
#
###
cacheSolve <- function(x, ...)
{
   m_inv    <- x$getinverse()
   
   # if data is found in cache, return it
   if(!is.null(m_inv))
   {
      message("is cached, returning data...")
      return(m_inv)
   }
   else  # otherwise compute it
   {
      data  <- x$get()
      m_inv <- solve(data, ...)
      
      # and store it in cache
      x$setinverse(m_inv)
      return(m_inv)
   }
}
