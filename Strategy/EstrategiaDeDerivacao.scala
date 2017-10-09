package br.unb.cic.poo.gol

trait EstrategiaDeDerivacao {
   
    def shouldKeepAlive(i: Int, j: Int): Boolean 
    def shouldRevive(i: Int, j: Int): Boolean 
    
}
  class Regras extends EstrategiaDeDerivacao {
    
     def shouldKeepAlive(i: Int, j: Int): Boolean = {
      (GameEngine.cells(i)(j).isAlive) &&
      (GameEngine.numberOfNeighborhoodAliveCells(i, j) == 2 || GameEngine.numberOfNeighborhoodAliveCells(i, j) == 3)
    }
  
   def shouldRevive(i: Int, j: Int): Boolean = {
      (!GameEngine.cells(i)(j).isAlive) && 
      (GameEngine.numberOfNeighborhoodAliveCells(i, j) == 3)
    }
  }
  
  object ModeloStrategy {
    var numero: Int = 1 
    var modelo: EstrategiaDeDerivacao = _
     def Set(numero:Int){
      if (numero == 1){
        modelo = new Regras;
      }
      else { 
        print ("não existe uma regra com tal número") 
      }
     }
  }
  
  
  
  
