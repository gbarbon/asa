package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.utils.prelude.Wrapper

/**
 * @author esteffin
 */
object lattice {
  trait Lattice[+A] extends Wrapper[A] {
    def <==[B >: A](r: Lattice[B]): Boolean
    def join[B >: A](r: Lattice[B]): Lattice[B]
    def meet[B >: A](r: Lattice[B]): Lattice[B]
  }

  trait LatticeFactory[+A] {
    def top: Lattice[A]
    def bottom: Lattice[A]
  }
}

object widening_lattice {
  import it.unive.dais.yaasa.datatype.lattice.Lattice

  trait WideningLattice[+A] extends lattice.Lattice[A] {
    override def <==[B >: A](r: Lattice[B]): Boolean
    override def join[B >: A](r: Lattice[B]): WideningLattice[B]
    override def meet[B >: A](r: Lattice[B]): WideningLattice[B]
    def widening[B >: A](r: WideningLattice[B]): WideningLattice[B]
  }

  trait WideningLatticeFactory[+A] extends lattice.LatticeFactory[A] {
    override def top: WideningLattice[A]
    override def bottom: WideningLattice[A]
  }
}
