package it.unive.dais.yaasa.datatype

/**
 * @author esteffin
 */
object lattice {
  trait Lattice[A] {
    def <==(r: A): Boolean
    def join(r: A): A
    def meet(r: A): A
    override def toString(): String
  }

  trait LatticeFactory[A] {
    def top: Lattice[A]
    def bottom: Lattice[A]
    def parse(s: String): Lattice[A]
  }
}
