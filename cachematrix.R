Funções:
cachematrix.R:
  ## Colocando em Cache o Inverso de uma Matriz: 
  ## A inversão de matrizes é geralmente um cálculo caro e pode haver 
  ## benefício para armazenar em cache o inverso de uma matriz em vez de computá-la repetidamente. 
  ## Abaixo estão um par de funções que são usadas para criar um objeto especial que 
  ## armazena uma matriz e armazena em cache seu inverso. 

## Essa função cria um objeto "matriz" especial que pode armazenar em cache seu inverso. 

makeCacheMatrix <- function ( x = matrix ()) { 
  inv <- NULL 
  set <- function ( y ) { 
  x << - y 
  inv << - NULL 
  } 
  obter <- function () x 
  setInverse <- function ( inverse ) inv << - inverse 
  getInverse <- function () inv 
  lista ( set = set , 
  get = get , 
  setInverse = setInverse, 
  getInverse = getInverse) 
  } 

  ## Esta função calcula o inverso da "matriz" especial criada por 
  ## makeCacheMatrix acima.  Se o inverso já foi calculado (e 
  ## matriz não mudou), então deve recuperar o inverso do cache. 

  cacheSolve <- function ( x , ...) { 
  ## Retorna uma matriz que é o inverso de 'x' 
  inv <- x $ getInverse () 
  if (! é. null (inv)) { 
  message ( "obtendo dados em cache" ) 
  retorno (inv) 
  } 
  mat <- x $ get () 
  inv <- solve (mat, ...) 
  x $ setInverse (inv) 
  inv 
  } 

