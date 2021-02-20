## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL              #inistalising inverse as NULL
  set<-function(y){
                    x<<-y
                    inv<<-NULL
                    }
  get<-function()x       #function to obtain matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                      inver<-ginv(x)
                      inver%%x      #function to invert the matrix
  }
  list(set=set,get=get,
       setinv=setinv
       getinv=getinv)
}


## Write a short comment describing this function
#This is a function used to get the cache data

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){              #checks whether or not inverse is NULL
                          message("getting cached data")
                          return(inv)   #returns the inverse value
        }
        data<-x$get()
        inv<-solve(data....)            #calculates the inverse of the matrix
        x$setinv(inv)
        inv                             #returns the inverse of the matrix x
}
