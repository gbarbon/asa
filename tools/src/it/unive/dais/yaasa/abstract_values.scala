package it.unive.dais.yaasa

import it.unive.dais.yaasa.absyn._

/**
 * This object contains all the classes used by the analysis.
 */
object abstract_values {

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
  /*
   top  :: a
   bot  :: a*/

  object LMH {
    sealed trait LMHV

    case object Low extends LMHV { override def toString() = "Low" }
    case object Medium extends LMHV { override def toString() = "Medium" }
    case object High extends LMHV { override def toString() = "High" }

    implicit def lattice(l: LMHV): Lattice[LMHV] = new Lattice[LMHV] {

      def <==(r: LMHV): Boolean =
        (l, r) match {
          case (Low, _)      => true
          case (Medium, Low) => false
          case (Medium, _)   => true
          case (High, High)  => true
          case (High, _)     => false
        }
      def join(r: LMHV): LMHV =
        if (this <== r) r else l
      def meet(r: LMHV): LMHV =
        if (this <== r) l else r
      override def toString() = l.toString()
    }

    object LMHFactory extends LatticeFactory[LMHV] {
      def top: LMH = High
      def bottom: LMH = Low
      def parse(s: String): LMH = {
        s match {
          case "L"   => Low
          case "M"   => Medium
          case "H"   => High
          case error => throw utils.parsingUtils.ParseError("Error parsing %s, not a valid HML string." format (error))
        }
      }
    }
  }

  type LMH = Lattice[LMH.LMHV]

  /**
   * Quantitative value class
   */
  case class BitQuantity(oQuant: Int = 0, uQuant: Int = 0) {
    def this(quant: Int) = this(quant, quant)
    /**
     * Update of the quantitative value
     */
    def oUpdate() = this.copy(oQuant = oQuant + 1)
    def uUpdate() = this.copy(uQuant = uQuant + 1)

    /**
     * Print of the quantitative value
     */
    def oPrint = oQuant
    def uPrint = uQuant

    override def toString() = "[%d-%d]" format (oQuant, uQuant)
  }

  /**
   * @constructor create a new Label with a name, a confidentiality level and a dimension
   * @param name name of the label
   * @param conf the confidentiality value for the label
   * @param dim dimension in bit of the label
   */
  case class Label(
      name: String,
      conf: LMH,
      dim: BitQuantity) {
    override def toString() = "%s:%s:%s" format (name, conf.toString(), dim.toString())
  }

  object Label {
    def empty = Label("star", LMH.Low, BitQuantity())
  }

  /**
   * Statement applied to the label. The statement can be a function or an operator.
   * @FIXME:  Does it must also have an associated label?
   *          If so, it is sufficient the name of the associated label, or we want a link to the other label object instance?
   * @constructor create a new statement instance with a name, an obfuscation power and a quantity of released bit
   * @param name name of the function or the operator
   * @param obf the obfuscation power of the statement
   * @param implq the quantity of bits released by the statement
   * @param aLabel the associated label @FIXME: is this correct?
   */
  // changed aLabel from Label to String
  //@FIXME: added List[LMH} => LMH to fix error, but not sure it is correct
  case class Statement(name: String, obf: List[LMH] => LMH, implq: BitQuantity, aLabel: Label) {
    /**
     * It prints the Statement operator or function, with the associated label, see @FIXME above
     */
    def print = "<" + name + ", " + aLabel + ">" //"(" + name + ", " + aLabel + ")"
    override def toString() = print
  }

  object Statement {

    /**
     * def BOPlusPlus(aLabel: Label) = Statement("++", LMH.Low, BitQuantity(0, 0), aLabel) //BOPlusPlus,++,L,0
     * def BOPlus(aLabel: Label) = Statement("+", LMH.Low, BitQuantity(0, 0), aLabel) //BOPlus,+,L,0
     * def BOMinus(aLabel: Label) = Statement("-", LMH.Low, BitQuantity(0, 0), aLabel) //BOMinus,-,L,0
     * def BOMul(aLabel: Label) = Statement("*", LMH.Low, BitQuantity(0, 0), aLabel) //BOMul,*,L,0
     * def BODiv(aLabel: Label) = Statement("/", LMH.Low, BitQuantity(0, 0), aLabel) //BODiv,/,L,0
     * def BOAnd(aLabel: Label) = Statement("&&", LMH.Low, BitQuantity(0, 0), aLabel) //BOAnd,&&,L,0
     * def BOOr(aLabel: Label) = Statement("||", LMH.Low, BitQuantity(0, 0), aLabel) //BOOr,||,L,0
     * def BOMod(aLabel: Label) = Statement("%", LMH.Low, BitQuantity(0, 0), aLabel) //BOMod,%,L,0
     * def BOLt(aLabel: Label) = Statement("<", LMH.Low, BitQuantity(0, 0), aLabel) //BOLt,<,L,0
     * def BOLeq(aLabel: Label) = Statement("<=", LMH.Low, BitQuantity(0, 0), aLabel) //BOLeq,<=,L,0
     * def BOEq(aLabel: Label) = Statement("==", LMH.Low, BitQuantity(0, 0), aLabel) //BOEq,==,L,0
     * def BOGt(aLabel: Label) = Statement(">", LMH.Low, BitQuantity(0, 0), aLabel) //BOGt,>,L,0
     * def BOGeq(aLabel: Label) = Statement(">=", LMH.Low, BitQuantity(0, 0), aLabel) //BOGeq,>=,L,0
     * def BONeq(aLabel: Label) = Statement("!=", LMH.Low, BitQuantity(0, 0), aLabel) //BONeq,!=,L,0
     * def UNeg(aLabel: Label) = Statement("-", LMH.Low, BitQuantity(0, 0), aLabel) // UNot,!,L,0
     * def UNot(aLabel: Label) = Statement("!", LMH.Low, BitQuantity(0, 0), aLabel) // UNeg,-,L,0
     *
     */

    def sCreator(aLabel: Label, annot: FunAnnot) = Statement(annot.name, annot.obfuscation, annot.quantity, aLabel)
  }

  /**
   * @constructor create a new atomic data expression of a certain label.
   * @param label the associated label
   * @param oExpStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
   * @param uExpStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
   * @param oImplStm Over approximation of the statements applied to the label (implicit flow)
   * @param uImplStm Under approximation of the statements applied to the label (implicit flow)
   * @param oImplQuant Over approximation of the quantitative value released in the implicit flow
   * @param uImplQuant Under approximation of the quantitative value released in the implicit flow
   * Notice:  We use over-approximation variables only in case of concrete-only analysis
   */
  case class ADExp(
      label: Label,
      //type: String, @FIXME: add also the type of the label???
      oExpStm: List[Statement] = List[Statement](),
      uExpStm: List[Statement] = List[Statement](),
      oImplStm: List[Statement] = List[Statement](),
      uImplStm: List[Statement] = List[Statement](),
      implQuant: BitQuantity = new BitQuantity()) {
    val name = label.name

    override def toString() = {
      def print_stmts(l: List[Statement]) = utils.pretty_print.xhcat(",")(l map { _.toString() })
      "<(%s), %s, %s, %s>" format (label.toString(), print_stmts(oExpStm), print_stmts(oImplStm), implQuant.toString())
    }

    /**
     * Print only the name
     */
    def namePrint = name

    /**
     * "add" methods for statements lists
     * @param stm a statement
     */
    def addOExpStm(stm: Statement) = this.copy(oExpStm = stm :: oExpStm)
    def addUExpStm(stm: Statement) = this.copy(uExpStm = stm :: uExpStm)
    def addOImplStm(stm: Statement) = this.copy(oImplStm = stm :: oImplStm)
    def addUImpltm(stm: Statement) = this.copy(uImplStm = stm :: uImplStm)
    def addExpStm(stm: Statement) = this.copy(oExpStm = stm :: oExpStm).copy(uExpStm = stm :: uExpStm)
    def addImpltm(stm: Statement) = this.copy(oImplStm = stm :: oImplStm).copy(uImplStm = stm :: uImplStm)

    /**
     * "update" methods for quantitative values
     */
    def updateImplQuant() = this.copy(implQuant = implQuant.oUpdate())

    /**
     * Concrete Print function.
     * It prints the extended atomic data expression for the current label.
     */
    def concretePrint = "{" + concExplFlowPrint + "}, {" + concImplFlowPrint + "}"
    def concExplFlowPrint = "<" + name + "{" + (oExpStm map print) + "}>" // explicit flow print function (concrete)
    def concImplFlowPrint = "<" + name + "{" + (oImplStm map print) + "}>" // implicit flow print function (concrete)

    // implicit quantitative value print function (concrete)
    def concImplQuantPrint = "<" + name + ", " + implQuant.oPrint + ">"

    /**
     * Abstract print functions.
     */
    def abstractPrint = "{" + abstExplFlowPrint + "}, {" + abstImplFlowPrint + "}"
    def abstExplFlowPrint = "<" + name + "{" + (uExpStm map print) + "}, {" + (oExpStm map print) + "}>" // explicit flow print function (abstract)
    def abstImplFlowPrint = "<" + name + "{" + (uImplStm map print) + "}, {" + (oImplStm map print) + "}>" // implicit flow print function (abstract)

    // implicit quantitative value print function (abstract)
    def abstImplQuantPrint = "<" + name + ", " + implQuant.oPrint + ">"

  }
  object ADExp {
    def empty = ADExp(Label.empty)
  }
}
