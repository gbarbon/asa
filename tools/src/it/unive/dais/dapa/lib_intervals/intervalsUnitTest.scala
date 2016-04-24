package it.unive.dais.dapa.lib_intervals
import intervals._

/**
  * @author esteffin
  */
object intervalsUnitTest {

  def otherTest(b: intervt, c: intervt): Unit = {
    printf("b=")
    intervPrint(b)
    printf(" c=")
    intervPrint(c)
    printf("\n")

    printf(s"intervalNeg($b)=")
    intervPrint(intervNeg(b))
    printf("\n")

    printf(s"intervalAbs($b)=")
    intervPrint(intervAbs(b))
    printf("\n")

    printf(s"$b + $c =")
    intervPrint(intervAdd(b, c))
    printf("\n")

    printf(s"$b - $c =")
    intervPrint(intervSub(b, c))
    printf("\n")

    printf(s"$b * $c =")
    intervPrint(intervMul(b, c))
    printf("\n")

    printf(s"$b / $c =")
    intervPrint(intervDiv(b, c))
    printf("\n")

    printf(s"$b mod $c =")
    intervPrint(intervMod(b, c))
    printf("\n")

    printf(s"$b JOIN $c =")
    intervPrint(intervJoin(b, c))
    printf("\n")

    printf(s"$b MEET $c =")
    intervPrint(intervMeet(b, c)._2)
    printf("\n")

    printf(s"$b WIDENING $c =")
    intervPrint(intervWidening(b, c))
    printf("\n")
  }

  def intervalMain(): Unit = {
    var b = intervt.bottom
    var c = intervt.bottom
    var x = intervt.point(0)
    var y = intervt.point(1)
    println(x)
    x = intervWidening(x, y)
    println(x)
    y = intervWidening(x, y)
    println(x)
    println(x==y)
    throw new RuntimeException("")

  }
  def main(args: Array[String]) {
    val a = intervt.interval(3, 6)
    val b = intervt.interval(5, 9)
    intervalMain()
    println(intervLeqat(a, b))
  }
}

