## Use a cached matrix to make inverse calculations more effective

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL ## Matrix inverse
  
  ## Set the inner matrix to the given one and set inverse to NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the inner matrix
  get <- function()
  {
    x
  }
  
  ## Set the inner matrix inverse to the given one
  setInverse <- function(i)
  {
    inv <<- i
  }
  
  ## Get the inverse of the inner matrix
  getInverse <- function ()
  {
    inv
  }
  
  ## Return the 'cachedMatrix'
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...)
{
  ## Get the inverse and check for NULL
  m <- x$getInverse()
  if(!is.null(m))
  {
    ## Return cached inverse if present
    return(m)
  }
  ## Calculate inverse if not cached and set it to the cachedMatrix object
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
