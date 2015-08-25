package it.unive.dais.yaasa

//import scala.collection.mutable._

/**
 * @author esteffin
 */
/*
object config {
  val sources = MutableList[String]()
  val libs = MutableList[String]()

}*/

import java.io.File
import scopt._

object config {

  val credits = {
    case class Version(Major: Int = 0, Minor: Int = 0, Build: Int = 100, Revision: Int = 0)
    val date =
      {
        val now = java.util.Calendar.getInstance().getTime()
        val str = new java.text.SimpleDateFormat("yyyy-MM-dd")
        str format now
      }
    //val asm = Assembly.GetExecutingAssembly()
    val name = this.getClass().getPackage().getName()
    val ver = Version()
    val title = "Title..."
    val description = "Description..."
    val product = "Product..."
    val copyright = "Copyright..."
    val company = "Company..."
    "%s v%d.%d.%d build %d [%s]\n\n%s\n\n%s & %s are %s %s.\n" format
      (title,
        ver.Major, ver.Minor, ver.Build, ver.Revision, date,
        description,
        product, title, copyright, company)
  }

  case class Config(
    libs: List[File],
    operators: File,
    sources: List[File],
    verbose: Boolean = false,
    warnLevel: Int = 0,
    out: Option[File] = None)

  private val empty = Config(List(), null, List())

  private var _value: Config = empty

  private val parser: OptionParser[Config] = new OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[File]('o', "out") valueName ("<file>") action { (x, c) =>
      c.copy(out = Some(x))
    } text ("redirect the output of the analysis to the file specified")
    opt[Seq[File]]('l', "libs") valueName ("<lib1>,<lib1>...") action { (x, c) =>
      c.copy(libs = x toList)
    } text ("Lib definitions to include")
    /*opt[Seq[File]]('s', "sources") valueName ("<src1>,<src2>...") action { (x, c) =>
      c.copy(sources = x toList)
    } text ("Sources to analyze")*/
    opt[File]('o', "operators") valueName ("<operator>") action { (x, c) =>
      c.copy(operators = x)
    } text ("The file with specifications of the operators")
    opt[Int]('w', "warn") action {
      case (warn, c) =>
        c.copy(warnLevel = warn)
    } text ("Set the verbosity of the analysis")
    opt[Unit]("verbose") action { (_, c) =>
      c.copy(verbose = true)
    } text ("Set if the output is verbose")
    opt[Unit]("debug") hidden () action { (_, c) =>
      c.copy(warnLevel = 3)
    } text ("this option is hidden in the usage text")
    opt[Unit]("version") action { (_, c) =>
      println(credits)
      c
    }
    //note("File.\n")
    help("help") text ("prints this usage text")
    arg[File]("<file>...") unbounded () required () action { (x, c) =>
      c.copy(sources = c.sources :+ x)
    } text ("Source file to be analyzed")
  }

  def value = _value

  def initialize(args: Seq[String]) = {
    // parser.parse returns Option[C]
    parser.parse(args, empty) match {
      case Some(config) =>
        _value = config

      case None =>
      // arguments are bad, error message will have been displayed
    }
  }
}

/*
case class ConfigDef(foo: Int = -1, out: File = new File("."), xyz: Boolean = false,
                     libName: String = "", maxCount: Int = -1, verbose: Boolean = false, debug: Boolean = false,
                     mode: String = "", files: Seq[File] = Seq(), keepalive: Boolean = false,
                     jars: Seq[File] = Seq(), kwargs: Map[String, String] = Map())

object args {

  val parser: OptionParser[ConfigDef] = new OptionParser[ConfigDef]("scopt") {
    head("scopt", "3.x")
    opt[Int]('f', "foo") action {
      case (jjj, c) =>
        c.copy(foo = jjj)
    } text ("foo is an integer property")
    opt[File]('o', "out") required () valueName ("<file>") action { (x, c) =>
      c.copy(out = x)
    } text ("out is a required file property")
    opt[(String, Int)]("max") action {
      case ((k, v), c) =>
        c.copy(libName = k, maxCount = v)
    } validate { x =>
      if (x._2 > 0) success else failure("Value <max> must be >0")
    } keyValueName ("<libname>", "<max>") text ("maximum count for <libname>")
    opt[Seq[File]]('j', "jars") valueName ("<jar1>,<jar2>...") action { (x, c) =>
      c.copy(jars = x)
    } text ("jars to include")
    opt[Map[String, String]]("kwargs") valueName ("k1=v1,k2=v2...") action { (x, c) =>
      c.copy(kwargs = x)
    } text ("other arguments")
    opt[Unit]("verbose") action { (_, c) =>
      c.copy(verbose = true)
    } text ("verbose is a flag")
    opt[Unit]("debug") hidden () action { (_, c) =>
      c.copy(debug = true)
    } text ("this option is hidden in the usage text")
    note("some notes.\n")
    help("help") text ("prints this usage text")
    arg[File]("<file>...") unbounded () optional () action { (x, c) =>
      c.copy(files = c.files :+ x)
    } text ("optional unbounded args")
    cmd("update") action { (_, c) =>
      c.copy(mode = "update")
    } text ("update is a command.") children (
      opt[Unit]("not-keepalive") abbr ("nk") action { (_, c) =>
        c.copy(keepalive = false)
      } text ("disable keepalive"),
      opt[Boolean]("xyz") action { (x, c) =>
        c.copy(xyz = x)
      } text ("xyz is a boolean property"),
      checkConfig { c =>
        if (c.keepalive && c.xyz) failure("xyz cannot keep alive") else success
      })
  }

  def foo(args: Seq[String]) = {
    // parser.parse returns Option[C]
    parser.parse(args, ConfigDef()) match {
      case Some(config) =>
      // do stuff

      case None         =>
      // arguments are bad, error message will have been displayed
    }
  }
}*/
