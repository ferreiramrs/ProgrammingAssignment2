## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Inicializa o cache da inversa como NULL
    
    # Função para configurar (set) uma nova matriz
    set <- function(y) {
        x <<- y      # Atribui a matriz ao ambiente pai
        inv <<- NULL # Limpa o cache se a matriz mudar
    }
    
    # Função para obter (get) a matriz atual
    get <- function() x
    
    # Função para definir (setinv) a inversa calculada no cache
    setinv <- function(inverse) inv <<- inverse
    
    # Função para obter (getinv) a inversa do cache
    getinv <- function() inv
    
    # Retorna uma lista com as quatro funções
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv() # Tenta obter a inversa do cache
    
    # Verifica se o cache existe
    if(!is.null(inv)) {
        message("getting cached data") # Informa que está usando o cache
        return(inv) # Retorna a inversa sem recalcular
    }
    
    # Se não estiver no cache, calcula agora
    data <- x$get()       # Obtém a matriz original
    inv <- solve(data, ...) # Calcula a inversa usando a função solve()
    x$setinv(inv)         # Armazena o resultado no cache
    inv                   # Retorna a matriz inversa
}
