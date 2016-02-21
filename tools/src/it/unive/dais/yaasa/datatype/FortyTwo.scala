package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.datatype.ABSValue.{AbstractDegrValue, AbstractValue}
import it.unive.dais.yaasa.datatype.ADType.ADInfo
import it.unive.dais.yaasa.datatype.LMH._
import it.unive.dais.yaasa.utils.prelude.pretty
import it.unive.dais.yaasa.datatype.SimpleInterval._

/**
  * @author esteffin
  */
object FortyTwo {

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

  // Old Quantitative class
  /**
  case class BitQuantity(uQuant: Int = 0, oQuant: Int = 0) extends pretty {
    def this(quant: Int) = this(quant, quant)
    /**
     * Update of the quantitative value
     */
    def oUpdate() = this.copy(oQuant = oQuant + 1)
    def uUpdate() = this.copy(uQuant = uQuant + 1)
    def update(qnt: BitQuantity) = this.copy(uQuant = uQuant + qnt.uQuant, oQuant = oQuant + qnt.oQuant)

    /**
     * Print of the quantitative value
     */
    def oPrint = oQuant
    def uPrint = uQuant

    def join(r: BitQuantity): BitQuantity = {  //@FIXME: check this, not sure
      val l = this
      BitQuantity(l.oQuant + r.oQuant, l.uQuant + r.uQuant)
    }

    def meet(r: BitQuantity): BitQuantity = { //@FIXME: check this!!
      val l = this
      val new_u = math.max(r.uQuant,l.uQuant)
      val new_o = math.min(r.oQuant,l.oQuant)
      if (new_u <= new_o)
        BitQuantity(new_u, new_o)
      else
        BitQuantity.empty
    }

    def widening(r: BitQuantity): BitQuantity = ???  //@FIXME: implement me!

    override def pretty: String = "[%d-%d]" format (oQuant, uQuant)
  }

  object BitQuantity {
    def empty = BitQuantity()
    def oneBit = BitQuantity(1, 1)
  }
  **/
  case class BitQuantity(content: Interval) extends pretty {

    def uUpdate() = this.copy(content = content.+^(Interval.interval(1,0)))
    def oUpdate() = this.copy(content = content.+^(Interval.interval(0,1)))
    def update(iter: BitQuantity) = this.copy(content = content.+^(iter.content))

    def uPrint = content.getLeft
    def oPrint = content.getRight

    def join(r: BitQuantity): BitQuantity = BitQuantity(content.join(r.content))
    def meet(r: BitQuantity): BitQuantity = BitQuantity(content.meet(r.content))
    def widening(r: BitQuantity): BitQuantity = BitQuantity(content.widening(r.content))

    override def pretty: String = "[%d-%d]" format (content.getLeft, content.getRight) //content.pretty
  }

  object BitQuantity {
    def empty = BitQuantity(Interval.interval(0,0))
    def oneIter = BitQuantity(Interval.interval(1,1))
    def fromNum(b: Int): BitQuantity = BitQuantity(Interval.fromNum(b))
    def fromInterval(l: Int, r: Int): BitQuantity = BitQuantity(Interval.interval(l,r))
    def top: BitQuantity = BitQuantity(Interval.top)
    def bottom: BitQuantity = BitQuantity(Interval.bottom)
  }


  // Old Iteration Class
  /**
  case class Iterations(uIter: Int = 0, oIter: Int = 0) extends pretty {
    def this(iter: Int) = this(iter, iter)
    /**
     * Update of the iterations value
     */
    def oUpdate() = this.copy(oIter = oIter + 1)
    def uUpdate() = this.copy(uIter = uIter + 1)
    def update(iter: Iterations) = this.copy(uIter = uIter + iter.uIter, oIter = oIter + iter.oIter)

    /**
     * Print of the iterations value
     */
    def oPrint = oIter
    def uPrint = uIter

    def join(r: Iterations): Iterations = { //@FIXME: check this, not sure
      val l = this
      Iterations(l.oIter + r.oIter, l.uIter + r.uIter)
    }
    def meet(r: Iterations): Iterations = { //@FIXME: check this!!
      val l = this
      val new_u = math.max(r.uIter,l.uIter)
      val new_o = math.min(r.oIter,l.oIter)
      if (new_u <= new_o)
        Iterations(new_u, new_o)
      else
        Iterations.empty
    }
    def widening(r: Iterations): Iterations = join(r) //@FIXME: temporary!!
    override def pretty: String = "[%d-%d]" format (oIter, uIter)
  }

  object Iterations {
    def empty = Iterations()
    def oneIter = Iterations(1, 1)
  }**/

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
    def starUid(uid: String) = Label("star"+uid, ConfLatticeFactory.bottom, BitQuantity.empty)
    def newLabel(ann: LabelAnnot): List[Label] =
      for (i <- List.range(0, ann.molteplicity))
        yield Label("%s_%s" format (ann.name, i), ann.confidentiality, ann.dimension)
  }

   // @FIXME: temporary, same name of the type defined in the analyzer (with ConcreteValue)!!!
   case class ValueWithAbstraction(value: AbstractValue, adInfo: ADInfo[FunAnnot, Uid, AbstractValue]) extends pretty {
     override def pretty: String = "%s -- %s" format (value, adInfo)
   }

}
