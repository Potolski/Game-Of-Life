package GoL

import scala.collection.mutable.ListBuffer

object Cuidador {
  val cellsMementos = new ListBuffer[CellsMemento]
  
  private var current = -1
  
  def persist: Unit = {
    cellsMementos += Repositorio.salvar
    current += 1
  }
  
  def undo: Unit = {
    if (validMemento(current - 1)) {
      current -= 1
      Repositorio.restaura(cellsMementos(current))
    } else {
      Repositorio.restaura(cellsMementos(0))
    }
  }

  def redo: Unit = {
    if (validMemento(current + 1)) {
      current += 1
      Repositorio.restaura(cellsMementos(current))
    } else {
      Repositorio.restaura(cellsMementos.last)
    }
  }

  private def validMemento(position: Int): Boolean = {
    position >= 0 && position < cellsMementos.size
  }

  def clear: Unit = {
    current = -1
    cellsMementos.clear
  }
}
