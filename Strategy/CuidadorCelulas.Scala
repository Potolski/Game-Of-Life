package br.unb.cic.poo.gol

import scala.collection.mutable.ListBuffer

object CuidadorCelulas {
  val cellsMementos = new ListBuffer[CellsMemento]
  
  private var current = -1
  
  def persist: Unit = {
    cellsMementos += Repositorio_cells.salvar
    current += 1
  }
  
  def undo: Unit = {
    if (validMemento(current - 1)) {
      current -= 1
      Repositorio_cells.restaura(cellsMementos(current))
    } else {
      Repositorio_cells.restaura(cellsMementos(0))
    }
  }

  def redo: Unit = {
    if (validMemento(current + 1)) {
      current += 1
      Repositorio_cells.restaura(cellsMementos(current))
    } else {
      Repositorio_cells.restaura(cellsMementos.last)
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
