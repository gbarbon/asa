package it.unive.dais.dapa.datatype

import it.unive.dais.dapa.datatype.LMH._
import it.unive.dais.dapa.utils.prelude.pretty
import it.unive.dais.dapa.datatype.SimpleInterval._

/**
  * @author esteffin
  */
object GenTypes {

  type Obfuscation = ObfLattice

  trait Annot extends pretty
  case class LabelAnnot(name: String,
                        confidentiality: ConfLattice,
                        dimension: BitQuantity,
                        molteplicity: Int = 1) extends Annot {
    override def pretty = "%s:%s:%s:%s" format (name, confidentiality, dimension.toString(), molteplicity)
  }
  object LabelAnnot {
    def parse(strings: Map[String, String]) =
      {
        val name = strings("labelName")
        val conf = ConfLatticeFactory.parse(strings("conf"))
        val dim = BitQuantity.fromNum(strings("dim") toInt)
        if (strings contains "molt")
          LabelAnnot(name, conf, dim, strings("molt") toInt)
        else
          LabelAnnot(name, conf, dim)
      }
  }

  case class FunAnnot(name: String,
                      obfuscation: Obfuscation) extends Annot {
    override def pretty = "%s:%s" format (name, obfuscation)
  }

  object FunAnnot {
    def parse(strings: Map[String, String]) =
      {
        val name = strings("name")
        val init_c = ObfLatticeFactory.parse(strings("obf"))
        FunAnnot(name, init_c)
      }
  }

  case class BitQuantity(content: Interval) extends pretty {

    def uUpdate() = this.copy(content = content.+^(Interval.interval(1,0)))
    def oUpdate() = this.copy(content = content.+^(Interval.interval(0,1)))
    def update(iter: BitQuantity) = this.copy(content = content.+^(iter.content))

    def uPrint = content.getLeft
    def oPrint = content.getRight

    def join(r: BitQuantity): BitQuantity = BitQuantity(content.join(r.content))
    def meet(r: BitQuantity): BitQuantity = BitQuantity(content.meet(r.content))
    def widening(r: BitQuantity): BitQuantity = BitQuantity(content.widening(r.content))

    override def pretty: String = /*"[%d-%d]" format (content.getLeft, content.getRight) */content.pretty
  }

  object BitQuantity {
    def empty = BitQuantity(Interval.interval(0,0))
    def oneIter = BitQuantity(Interval.interval(1,1))
    def fromNum(b: Int): BitQuantity = BitQuantity(Interval.fromNum(b))
    def fromInterval(l: Int, r: Int): BitQuantity = BitQuantity(Interval.interval(l,r))
    def top: BitQuantity = BitQuantity(Interval.top)
    def bottom: BitQuantity = BitQuantity(Interval.bottom)
  }

  case class Iterations(content: Interval) extends pretty {

    def incr = Iterations(content +^ Interval.interval(1,1))

    def uUpdate() = this.copy(content = content.+^(Interval.interval(1, 0)))

    def oUpdate() = this.copy(content = content.+^(Interval.interval(0,1)))
    def update(iter: Iterations) = this.copy(content = content.+^(iter.content))

    def uPrint = content.getLeft
    def oPrint = content.getRight

    def join(r: Iterations): Iterations = Iterations(content.join(r.content))
    def meet(r: Iterations): Iterations = Iterations(content.meet(r.content))
    def widening(r: Iterations): Iterations = Iterations(content.widening(r.content))

    override def pretty: String = content.pretty
  }

  object Iterations {
    def empty = Iterations(Interval.interval(0,0))
    def oneIter = Iterations(Interval.interval(1,1))
    def fromNum(b: Int): Iterations = Iterations(Interval.fromNum(b))
    def fromInterval(l: Int, r: Int): Iterations = Iterations(Interval.interval(l,r))
    def top: Iterations = Iterations(Interval.top)
    def bottom: Iterations = Iterations(Interval.bottom)
  }

  /**
   * @constructor create a new Label with a name, a confidentiality level and a dimension
   * @param name name of the label
   * @param conf the confidentiality value for the label
   * @param dim dimension in bit of the label
   */
  case class Label(
      name: String,
      conf: ConfLattice,
      dim: BitQuantity) extends pretty {
    override def pretty = "%s:%s:%s" format (name, conf.toString, dim.toString)
  }

  object Label {
    def star = Label("star", ConfLatticeFactory.bottom, BitQuantity.empty)
    def newLabel(ann: LabelAnnot): List[Label] =
      for (i <- List.range(0, ann.molteplicity))
        yield Label("%s_%s" format (ann.name, i), ann.confidentiality, ann.dimension)
  }

}
