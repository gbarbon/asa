package it.unive.dais.yaasa.utils

import it.unive.dais.yaasa.utils.prelude.pretty

/**
 * @author esteffin
 */

object parsingUtils {
  object Location {
    def empty = new Location(-1, -1, -1, -1, "")
    def pretty_pair(a: Int, b: Int): String = {
      if (a == b)
        "%d" format a
      else
        "(%d,%d)".format(a, b)
    }

    def between(start_loc: Location, end_loc: Location) =
      new Location(start_loc.start_line, start_loc.start_col, end_loc.start_line, end_loc.start_col, start_loc.filename)
  }

  class Location(_line: Int, _col: Int, _end_line: Int, _end_col: Int, _filename: String) extends pretty {
    def start_line = _line
    def start_col = _col
    def end_line = _end_line
    def end_col = _end_col
    def filename = _filename

    private def pretty_linecol =
      if (this.start_line <= 0 || this.start_col <= 0)
        ""
      else
        "%s-%s".format(Location.pretty_pair(this.start_line, this.start_col),
          Location.pretty_pair(this.end_line, this.end_col))

    def pretty =
      if (this.filename == "") "%s" format pretty_linecol
      else "%s:%s" format (filename, pretty_linecol)

  }

  case class LocatedError(message: String, loc: Location)
      extends RuntimeException(message) {

    def location = loc
    //member self.header = self.location.pretty
    //override self.Message = sprintf "%s: %s" self.header message
  }

  case class ParseError(msg: String) extends RuntimeException(msg)
}
