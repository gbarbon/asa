package it.unive.dais.yaasa.datatype

/**
 * @author esteffin
 */
object lattice {
  trait Lattice[A] {
    def <==(r: A): Boolean
    def join(r: A): A
    def meet(r: A): A
  }

  trait LatticeFactory[A] {
    def top: Lattice[A]
    def bottom: Lattice[A]
    //def parse(s: String): Lattice[A]
  }
}

object widening_lattice {
  trait WideningLattice[A] extends lattice.Lattice[A] {
    def widening(r: A): A
  }

  trait WideningLatticeFactory[A] extends lattice.LatticeFactory[A] {
    override def top: WideningLattice[A]
    override def bottom: WideningLattice[A]
    //override def parse(s: String): WideningLattice[A]
  }
}
