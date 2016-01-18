package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.utils.prelude.Wrapper

/**
 * @author esteffin
 */
object lattice {
  trait Lattice[A] extends Wrapper[A] {
    def <==(r: Wrapper[A]): Boolean
    def join(r: Wrapper[A]): Lattice[A]
    def meet(r: Wrapper[A]): Lattice[A]
  }

  trait LatticeFactory[A] {
    def top: Lattice[A]
    def bottom: Lattice[A]
  }
}

object widening_lattice {
  trait WideningLattice[A] extends lattice.Lattice[A] {
    def widening(r: Wrapper[A]): WideningLattice[A]
  }

  trait WideningLatticeFactory[A] extends lattice.LatticeFactory[A] {
    override def top: WideningLattice[A]
    override def bottom: WideningLattice[A]
  }
}
