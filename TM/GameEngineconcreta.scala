package br.unb.cic.poo.gol


/**
 * Essa classe herda da superclasse GameEngine e aplica as regras para as funções  shouldKeepAlive e shouldRevive 
 * 
 */

object GameEngineconcreta extends GameEngine{
  
  /* verifica se uma celula deve ser mantida viva */
  override def  shouldKeepAlive(i: Int, j: Int): Boolean = {
    (cells(i)(j).isAlive) &&
      (super.numberOfNeighborhoodAliveCells(i, j) == 2 || super.numberOfNeighborhoodAliveCells(i, j) == 3)
  }
  
  /* verifica se uma celula deve (re)nascer */
  override def  shouldRevive(i: Int, j: Int): Boolean = {
    (!cells(i)(j).isAlive) && 
      (super.numberOfNeighborhoodAliveCells(i, j) == 3)
  }

  
}