package it.unive.dais.yaasa.utils

import prelude._
import pretty_print._

/**
 * @author esteffin
 */
object env {

  case class UnboundSymbolError(_message: String) extends MessageException(_message) {
  }
  case class BoundSymbolError(_message: String) extends MessageException(_message) {
  }

  case class Env[id <: { def toString: String }, a <: { def toString: String }](m: Map[id, a]) extends pretty {
    def this() = this(Map[id, a]())
    def this(l: List[(id, a)]) = this(l toMap)
    def fail_unbound_symbol(s: id) = throw UnboundSymbolError("Invalid lookup of field %s in environment." format s.toString())

    def pretty: String = {
      val self = m
      if (m.size == 0)
        "<empty>"
      else {
        def print_map(mp: Map[id, a]): Seq[String] =
          mp.foldLeft(List[string]()) {
            case (s, (k, v)) =>
              val d = ("%s" format k) <|> (":  " <+> ("%s" format v.toString()))
              //s@[d]
              s ++ List(d)
          }
        val content = print_map(self)
        prettyList(";")(content)
      }
    }

    def keys = m.keys

    def values = m.values

    def pairs =
      m.map { x => x }

    def search_by(p: (id, a) ⇒ Boolean): Option[a] =
      {
        val res = m.find { case (x, v) => p(x, v) }
        res match {
          case Some((_, v)) => Some(v)
          case None         => None
        }
      }

    def search_by_key(p: id ⇒ Boolean): Option[a] =
      {
        val res = m.find { case (x, _) => p(x) }
        res match {
          case Some((_, v)) => Some(v)
          case None         => None
        }
      }

    def search(x: id): Option[a] =
      search_by((x1: id, v: a) => x1 == x)

    def lookup(x: id): a =
      search(x) match {
        case Some(v) => v
        case None    => fail_unbound_symbol(x)
      }

    def bind_new(x: id, v: a) =
      if (m.keys.exists { x == _ })
        throw new BoundSymbolError("Variable %s is already present in the scope." format x)
      else
        Env(m + (x -> v))

    def binds_new(bs: List[(id, a)]) =
      bs.foldLeft(this) { case (env, (x, v)) => env.bind_new(x, v) }

    def bind(x: id, v: a) =
      Env(m + (x -> v))

    def binds(bs: List[(id, a)]) =
      bs.foldLeft(this) { case (env, (x, v)) => env.bind(x, v) }

    def replace(x: id, v: a) =
      Env(m + (x -> v))

    def append(other: Env[id, a]) =
      other match {
        case Env(k) =>
          Env(m.foldLeft(k) { case (m, (x, t)) => m + (x -> t) })
      }
    def update(x: id)(f: a => a) =
      bind(x, f(lookup(x)))

    def effect(x: id)(f: a => a) = f(lookup(x))

    def map(f: (id, a) => (id, a)) =
      Env(m.map { case (x, v) => f(x, v) })

    def fold(s: (id, a))(f: ((id, a), (id, a)) => (id, a)) =
      m.fold(s)(f)

    def foldLeft[c](s: c)(f: (c, (id, a)) => c) =
      m.foldLeft(s)(f)

    def foldRight[c](s: c)(f: ((id, a), c) => c) =
      m.foldRight(s)(f)

    def filter(f: ((id, a)) => Boolean) = Env(m.filter(f))

    def find(f: ((id, a)) => Boolean) =
      m.find(f) match {
        case Some((k, _)) => k
        case None         => None
      }

    def exists(f: ((id, a)) => Boolean) =
      m.exists(f)

    def existsKey(f: id => Boolean) =
      m.exists { case (k, _) => f(k) }

    def occurs(x: id) =
      m.contains(x)

    def toSeq =
      m toSeq

    def asMap = m

    def pretty_sep(ret: Boolean)(p: (id, a) => string)(sep: string): string =
      if (m.isEmpty)
        "<empty>"
      else {
        val ss = m.foldLeft(List[String]()) { case (ss, (x, v)) => p(x, v) :: ss }
        if (ret)
          prettyList(sep)(ss)
        else
          prettyList(sep)(ss)
      }

    def pretty(p: (id, a) => string) =
      pretty_sep(ret=true)(p)("")

    //let pretty_diffs pretty_existant pretty_new env2 env1 =
    //    let f ss = function
    //        | Existant (x, v1, v2) -> if v1 <> v2 then pretty_existant x v1 v2 :: ss else ss
    //        | New (x, v)           -> pretty_new x v :: ss
    //    in
    //        diff f [] env2 env1

    //let empty = Env Map.empty

    /*    def partial_compare(other: Env[id, a]) =
      {
        val m1 = m
        val m2 = other.m
        def cmp(op: (a, a) => Boolean)(m1: Map[id, a])(m2: Map[id, a]) =
          {
            m1.forall {
              case (x1, v1) =>
                m2.find { case (x2, _) => x1 == x2 } match {
                  case Some(v2) => op(v1, v2)
                  case None     => false
                }
            }
          }

        if ((m.Size == other.m.Size) && cmp { case (a, b) => a == b }(m)(other.m))
          Some(0)
        else if (cmp({ case (a, b) => a > b })(m2)(m1))
          Some(-1)
        else if (cmp(>) m1 m2)
          Some(1)
        else None
      }
    */
    def union(other: Env[id, a])(join: ((a, a) => a)): Env[id, a] =
      {
        val m1 = this
        val m2 = other
        val keys = this.keys ++ other.keys toSet
        def f(s: Env[id, a], k: id) =
          (m1.search(k), m2.search(k)) match {
            case (None, None)         => throw new Unexpected("Arguments cannot be both null")
            case (Some(x), None)      => s.bind(k, x)
            case (None, Some(x))      => s.bind(k, x)
            case (Some(x1), Some(x2)) => s.bind(k, join(x1, x2))
          }
        val n_env = keys.foldLeft(Env(Map[id, a]()))(f)
        n_env
      }
    def update_values(other: Env[id, a]) =
      {
        Env(
          for { (x, v) <- this.m }
            yield other.search(x) match {
            case Some(v1) => x -> v1
            case None     => x -> v
          })
      }
  }

  object Env {
    def empty = Env(Map.empty)
  }
}
