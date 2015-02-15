# create a inversible matrix
#data<-matrix(c(1,0,1,2,4,0,3,5,6),3,3)

## Put comments here that give an overall description of what your
## functions do

## Create a new environment to store cache
cacheEnv<-new.env()
# This following function returns a list of four functions
# 1. set the matrix in cache
# 2. get the matrix in cache
# 3. set the inverse matrix in cache
# 4. get the inverse matrix in cache

## x should is a numeric vector that can be converted into a square matrix
makeCacheMatrix <- function(x=numeric()) 
{	i <- NULL
  y <-matrix(x,sqrt(length(x)),sqrt(length(x)))
  set <- function(z) 
  {      
    y <<- z
    i <<- NULL
  }
  get<-function()
  {
    y
  }
  setInverse<-function(inverse)
  {
    i<<-inverse
  }
  getInverse<-function()
  {
    i
  }
  return(list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}

# This function decides if we pull the inverse matrix from cache or calculate it.
## If the matrix is newly created and it's the first time we obtain the inverse, we calculate it using solve()
## If the matrix hasn't been changed and we want to obtain the inverse, retrieve it from cache.
cacheInverse <- function(l, ...) 
{
  i <- l$getInverse()
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  else
  {
    
    data <- l$get()
    Inverse <- solve(data)
    l$setInverse(Inverse)
    return(Inverse)
  }
  
}
