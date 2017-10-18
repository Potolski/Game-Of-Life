package br.unb.cic.poo.gol

/**
 * Relaciona o componente View com o componente Model. 
 * 
 * @author Breno Xavier (baseado na implementacao Java de rbonifacio@unb.br
 */
object GameController {
  
  def start {
    GameView.update
    CellsCaretaker.persist
  }



  def repManager(line: Int, column: Int, veredict: Boolean ): Unit = {
    if (veredict) {
      CellsRepository(line, column).revive
    } else {
      CellsRepository(line, column).kill
    }
  }
  
  def halt() {
    //oops, nao muito legal fazer sysout na classe Controller
    println("\n \n")
    Statistics.display
    System.exit(0)
  }

  def makeCellAlive(i: Int, j: Int) {
    try {
			GameEngine.makeCellAlive(i, j)
			GameView.update
		}
		catch {
		  case ex: IllegalArgumentException => {
		    println(ex.getMessage)
		  }
		}
  }
  
  def nextGeneration {
    GameEngine.nextGeneration
    GameView.update
  }
	
  def goBack: Unit = {
	CellsCaretaker.undo
	View.updateChart

  }
	
  def goFoward: Unit = {

    CellsCaretaker.redo
    View.updateChart

  }

  def clear: Unit = {

    CellsCaretaker.clear
    CellsRepository.clear
    View.updateChart
  }



  def memory: Unit ={

    CellsCaretaker.persist

  }
}
