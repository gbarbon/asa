package it.unive.dais.dapa.utils

/**
  * @author esteffin
  * @author gbarbon
  */

import it.unive.dais.dapa.utils.prelude._

/**
 * @author esteffin
 */
object collection {

  object map {
    def join_map[A, B](join: (B, B) => B, m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
      val keys = m1.keySet ++ m2.keySet
      (for (k <- keys) yield {
        (m1.get(k), m2.get(k)) match {
          case (None, None)       => throw new Unexpected("Cannot exists a key without data.")
          case (None, Some(r))    => k -> r
          case (Some(l), None)    => k -> l
          case (Some(l), Some(r)) => k -> join(l, r)
        }
      }).toMap
    }

    def meet_map[A, B](meet: (B, B) => B, m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
      val keys = m1.keySet intersect m2.keySet
      (for (k <- keys) yield {
        (m1.get(k), m2.get(k)) match {
          case (None, None)       => throw new Unexpected("Cannot exists a key without data.")
          case (None, Some(r))    => throw new Unexpected("Cannot exists a key without data.")
          case (Some(l), None)    => throw new Unexpected("Cannot exists a key without data.")
          case (Some(l), Some(r)) => k -> meet(l, r)
        }
      }).toMap
    }

    def widening_map[A, B](widening: (B, B) => B, m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
      val keys = m1.keySet ++ m2.keySet
      (for (k <- keys) yield {
        (m1.get(k), m2.get(k)) match {
          case (None, None)       => throw new Unexpected("Cannot exists a key without data.")
          case (None, Some(r))    => {
            k -> r}
          case (Some(l), None)    => {
            k -> l}
          case (Some(l), Some(r)) => {
            k -> widening(l, r)
          }
          case (_, _)       => throw new Unexpected("We must have both values to perform the widening.")
        }
      }).toMap
    }

    def add_map[A, B](m: Map[A, Set[B]], k: A, v: B) =
      if (m contains k)
        m updated (k, m(k) + v)
      else
        m + (k -> Set(v))

    def from_relation_set[A, B, C](extractor: A => (B, C), in: Set[A]): Map[B, Set[C]] =
      {
        val tuples = for ((e1, e2) <- in map extractor) yield e1 -> e2
        tuples.foldLeft(Map.empty[B, Set[C]]) { case (acc, (e1, e2)) => add_map(acc, e1, e2) }

      }
  }

  //noinspection LanguageFeature
  object list {
    class MyList[A](l: List[A]) {
      def cast[B] = l map { _.asInstanceOf[B] }
    }

    implicit def myListWrapper[A](l: List[A]): MyList[A] = new MyList(l)
  }

  object set {
    class MySet[A](s1: Set[A]) {
      def pointwise_join[B, C](s2: Set[B], join: (A, B) => C): Set[C] =
        for (e1 <- s1; e2 <- s2) yield join(e1, e2)
    }

    implicit def set_wrapper[A](s: Set[A]): MySet[A] = new MySet[A](s)

    class MySetSet[A](s1: Set[Set[A]]) {
      def pointwise_union(s2: Set[Set[A]]): Set[Set[A]] =
        s1.pointwise_join(s2, { (e1, e2: Set[A]) => e1 union e2 })

      def mul_pointwise_join(join: (A, A) => A): Set[Set[A]] =
        if (s1.size == 1)
          s1
        else
          for (sl <- s1; sr <- s1 if sl != sr) yield sl.pointwise_join(sr, join)

      def mul_pairwise_union =
        for (sl <- s1; sr <- s1 if sl != sr) yield sl union sr
    }

    implicit def set_set_wrapper[A](s: Set[Set[A]]): MySetSet[A] = new MySetSet[A](s)

    class MySetSetSet[A](s1: Set[Set[Set[A]]]) {
      def mul_pointwise_union: Set[Set[A]] =
        if (s1.size == 1)
          s1.head
        else {
          (for (sl <- s1; sr <- s1 if sl != sr) yield sl pointwise_union sr) flatten
        }
    }

    implicit def set_set_set_wrapper[A](s: Set[Set[Set[A]]]): MySetSetSet[A] = new MySetSetSet[A](s)
  }

}
