source("Estado.R")

## Classe e métodos para o problema do Aspirador de Pó
Aspirador <- function(desc = NULL, pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Canibais", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("(Q1 Q2 Q3 Q4 A): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  
  ## h(obj) = (2 * K) +1
  if(is.null(atual$desc))
    return(Inf)
    
   ## h(obj) = (2 * K) +1
  if(is.null(atual$desc[atual$desc[5]]))
    return((2 * sum(atual$desc[-5])) + 1)
    
  ## h(obj) = 2 * K 
  return(2 * sum(atual$desc[-5]))
}

## Sobrecarga da função genérica "custo", definida por Estado.R
custo.Aspirador <- function(atual, ops){
    
    if(all(atual$desc - ops[[1]] == atual$pai$desc)){
       
       return(atual$pai$g + 2)
        
    } else if(all(atual$desc - ops[[2]] == atual$pai$desc)){
       
       return(atual$pai$g + 1)
    
    } else{
       
       return(atual$pai$g + 3)
    
    }
}

geraFilhos.Aspirador <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()

  desc <- obj$desc
  
  qAtual <- as.numeric(desc[5])
  
  ## gera filhos usando todos os operadores  
  if(qAtual == 1){
    
    operadores <- list(c(-1,0,0,0,0), c(0,0,0,0,1), c(0,0,0,0,2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
    
  } else if(qAtual == 2){
    
    operadores <- list(c(0,-1,0,0,0), c(0,0,0,0,-1), c(0,0,0,0,2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
    
  } else if(qAtual == 3){
  
    operadores <- list(c(0,0,-1,0,0), c(0,0,0,0,1), c(0,0,0,0,-2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
  
  } else if(qAtual == 4){
  
    operadores <- list(c(0,0,0,-1,0), c(0,0,0,0,-1), c(0,0,0,0,-2))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
  
  }
  
  ## verifica estados filhos incompatíveis com o problema  
  incompativeis <- sapply(1:length(filhosDesc),
                    function(i) {
                      fDesc <- filhosDesc[[i]]
                      if((fDesc[5] < 1) ||            ## Aspirador não está em nenhum quadrado
                         (fDesc[5] > 4) ||
                         (any(fDesc[1:4] > 1)) ||     ## Se os quadrados não estão nem limpos nem sujos
                         (any(fDesc[1:4] < 0)))
                        i ## é incompatível: retorna índice
                      else
                        0 ## senão é compatível
                    })
  
  ## mantém no vetor apenas os que são incompatíveis
  incompativeis <- incompativeis[incompativeis != 0]
  
  ## remove estados filhos incompatíveis
  filhosDesc <- filhosDesc[-incompativeis]
  
  ## gera os objetos Canibais para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Aspirador(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- custo(filho, operadores)
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}
