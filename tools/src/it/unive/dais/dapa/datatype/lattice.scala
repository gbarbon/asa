package it.unive.dais.dapa.datatype

/**
  * @author esteffin
  * @author gbarbon
  */

import it.unive.dais.dapa.utils.prelude.pretty

object lattice {
  trait Lattice {
    def <==(r: Lattice): Boolean
    def join(r: Lattice): Lattice
    def meet(r: Lattice): Lattice
  }

  trait LatticeFactory {
    def top: Lattice
    def bottom: Lattice
  }
}

object widening_lattice {
  import it.unive.dais.dapa.datatype.lattice.Lattice


  trait WideningLattice extends lattice.Lattice {
    override def <==(r: Lattice): Boolean
    override def join(r: Lattice): WideningLattice
    override def meet(r: Lattice): WideningLattice
    def widening(r: WideningLattice): WideningLattice
  }

  trait WideningLatticeFactory extends lattice.LatticeFactory {
    override def top: WideningLattice
    override def bottom: WideningLattice
  }


  trait WideningOp[A <: WideningLattice] extends pretty {
    def widening(l: A, r: A): A
  }
  trait WideningOpFactory[A <: WideningLattice] {
    def default: WideningOp[A]
  }

}
