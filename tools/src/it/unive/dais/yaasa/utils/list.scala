package it.unive.dais.yaasa.utils

/**
 * @author gbarbon
 */

object list {
  class MyList[A](l: List[A]) {
    def cast[B] = l map { _.asInstanceOf[B] }
  }

  implicit def myListWrapper[A](l: List[A]) = new MyList(l)
}
