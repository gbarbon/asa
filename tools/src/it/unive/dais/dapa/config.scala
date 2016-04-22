package it.unive.dais.dapa



/**
 * @author esteffin
 */

import java.io.{PrintStream, File}
import scopt._

object config {

  object Credits {
    object Version {
      val major: Int = 1
      val minor: Int = 0
      val revision: Int = 0
      override def toString =
        "%d.%d.%d" format (major, minor, revision)
    }
    val date = {
      val now = java.util.Calendar.getInstance().getTime
      val str = new java.text.SimpleDateFormat("yyyy-MM-dd")
      str format now
      //"[2016-04-21]"
    }
    val name = this.getClass.getPackage.getName
    val description = "Degradation-Aware Privacy Analysis of Android Apps.\nDesigned and written by\n\tGianluca Barbon, Agostino Cortesi, Pietro Ferrara, Enrico Steffinlongo"
    val copyright = "Copyright © 2015-2017"
    val company = "Universita' Ca' Foscari di Venezia"
    val version = Version.toString
    override def toString =
      "%s v%s [%s]\n\n%s\n\n%s is %s %s.\n" format
        (name,
          version, date,
          description,
          name, copyright, company)
  }

  case class Config(
    sources: List[String],
    libs: List[String] = List[String]("libraries/readlib.java", "libraries/stdlib.java"),
    operators: String = "libraries/operators.csv",
    verbose: Boolean = false,
    quiet: Boolean = false,
    widening_threshold: Int = 15,
    max_string_length: Int = 30,
    profile: Boolean = false,
    out: Option[String] = None)

  private val empty = Config(List())

  private var _value: Config = empty

  private val parser: OptionParser[Config] = new OptionParser[Config](Credits.name) {
    val libs = config.value.libs.addString(new StringBuilder(), "", ",", "").toString()
    head(Credits.name, Credits.version)

    opt[String]("out") valueName "<file>" action { (x, c) =>
      c.copy(out = Some(x))
    } text "redirect the output of the analysis to the specified file"

    opt[Seq[String]]("libs") valueName "<lib1>,<lib1>..." action { (x, c) =>
      c.copy(libs = x toList)
    } text ("Path of the alternative lib definitions to include (defaults are %s)" format libs)

    opt[String]("operators") valueName "<operator>" action { (x, c) =>
      c.copy(operators = x)
    } text ("Path of the alternative file with the operators specifications (default = %s)" format config.value.operators)

    opt[Int]("widening-threshold") action {
      case (threshold, c) =>
        c.copy(widening_threshold = threshold)
    } text ("Set the widening threshold (default = %s)" format config._value.widening_threshold)

    opt[Int]("max-string-length (default = 18)") action {
      case (threshold, c) =>
        c.copy(max_string_length = threshold)
    } text ("Set the maximum length of the unbound strings (default = %s)" format config._value.max_string_length)

    opt[Unit]("verbose") action { (_, c) =>
      c.copy(verbose = true)
    } text "Set if the output is verbose (default = false)"

    opt[Unit]("profile") action { (_, c) =>
      c.copy(verbose = true)
    } text "Show the times needed for the analysis (default = false)"

    opt[Unit]("quiet") action { (_, c) =>
      c.copy(quiet = true)
    } text "Set if the output is quiet (default = false)"

    opt[Unit]("version") action { (_, c) =>
      println(Credits.toString)
      sys.exit()
      c
    }

    arg[String]("<file>") required () maxOccurs 1 action { (x, c) =>
      c.copy(sources = c.sources :+ x)
    } text "Source file to be analyzed"

    help("help") abbr "h" text "prints this usage text"

    checkConfig { c =>
      if (c.quiet && c.verbose)
        failure("Quiet and verbose are incompatible.")
      else success
    }
  }

  def value = _value

  def initialize(args: Seq[String]) = {
    // parser.parse returns Option[C]
    parser.parse(args, empty) match {
      case Some(config) =>
        _value = config
        config.out match {
          case None => ()
          case Some(x) =>
            Console.setOut(new PrintStream(x))
        }


      case None =>
        // arguments are bad, error message will have been displayed
        sys.exit()
    }
  }
}
