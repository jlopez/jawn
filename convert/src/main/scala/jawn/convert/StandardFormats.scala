/*
 * Original implementation (C) 2009-2011 Debasish Ghosh
 * Adapted and extended in 2011 by Mathias Doenitz
 * Adapted in 2018 by Jesus Lopez
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package jawn.convert

import jawn.ast._

/**
  * Provides the JsonFormats for the non-collection standard types.
  */
trait  StandardFormats {
  this: AdditionalFormats =>

  private[convert] type JF[T] = JsonFormat[T] // simple alias for reduced verbosity

  implicit def optionFormat[T :JF]: JF[Option[T]] = new OptionFormat[T]

  class OptionFormat[T :JF] extends JF[Option[T]] {
    def write(option: Option[T]) = option match {
      case Some(x) => x.toJson
      case None => JNull
    }
    def read(value: JValue) = value match {
      case JNull => None
      case x => Some(x.convertTo[T])
    }
    // allows reading the JSON as a Some (useful in container formats)
    def readSome(value: JValue) = Some(value.convertTo[T])
  }

  implicit def eitherFormat[A :JF, B :JF] = new JF[Either[A, B]] {
    def write(either: Either[A, B]) = either match {
      case Right(a) => a.toJson
      case Left(b) => b.toJson
    }
    def read(value: JValue) = (value.convertTo(safeReader[A]), value.convertTo(safeReader[B])) match {
      case (Right(a), _: Left[_, _]) => Left(a)
      case (_: Left[_, _], Right(b)) => Right(b)
      case (_: Right[_, _], _: Right[_, _]) => deserializationError("Ambiguous Either value: can be read as both, Left and Right, values")
      case (Left(ea), Left(eb)) => deserializationError("Could not read Either value:\n" + ea + "---------- and ----------\n" + eb)
    }
  }

  implicit def tuple1Format[A :JF] = new JF[Tuple1[A]] {
    def write(t: Tuple1[A]) = t._1.toJson
    def read(value: JValue) = Tuple1(value.convertTo[A])
  }

  implicit def tuple2Format[A :JF, B :JF] = new RootJsonFormat[(A, B)] {
    def write(t: (A, B)) = JArray(Array(t._1.toJson, t._2.toJson))
    def read(value: JValue) = value match {
      case JArray(Array(a, b)) => (a.convertTo[A], b.convertTo[B])
      case x => deserializationError("Expected Tuple2 as JArray, but got " + x)
    }
  }

  implicit def tuple3Format[A :JF, B :JF, C :JF] = new RootJsonFormat[(A, B, C)] {
    def write(t: (A, B, C)) = JArray(Array(t._1.toJson, t._2.toJson, t._3.toJson))
    def read(value: JValue) = value match {
      case JArray(Array(a, b, c)) => (a.convertTo[A], b.convertTo[B], c.convertTo[C])
      case x => deserializationError("Expected Tuple3 as JArray, but got " + x)
    }
  }

  implicit def tuple4Format[A :JF, B :JF, C :JF, D :JF] = new RootJsonFormat[(A, B, C, D)] {
    def write(t: (A, B, C, D)) = JArray(Array(t._1.toJson, t._2.toJson, t._3.toJson, t._4.toJson))
    def read(value: JValue) = value match {
      case JArray(Array(a, b, c, d)) => (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D])
      case x => deserializationError("Expected Tuple4 as JArray, but got " + x)
    }
  }

  implicit def tuple5Format[A :JF, B :JF, C :JF, D :JF, E :JF] = {
    new RootJsonFormat[(A, B, C, D, E)] {
      def write(t: (A, B, C, D, E)) = JArray(Array(t._1.toJson, t._2.toJson, t._3.toJson, t._4.toJson, t._5.toJson))
      def read(value: JValue) = value match {
        case JArray(Array(a, b, c, d, e)) =>
          (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D], e.convertTo[E])
        case x => deserializationError("Expected Tuple5 as JArray, but got " + x)
      }
    }
  }

  implicit def tuple6Format[A :JF, B :JF, C :JF, D :JF, E :JF, F: JF] = {
    new RootJsonFormat[(A, B, C, D, E, F)] {
      def write(t: (A, B, C, D, E, F)) = JArray(Array(t._1.toJson, t._2.toJson, t._3.toJson, t._4.toJson, t._5.toJson, t._6.toJson))
      def read(value: JValue) = value match {
        case JArray(Array(a, b, c, d, e, f)) =>
          (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D], e.convertTo[E], f.convertTo[F])
        case x => deserializationError("Expected Tuple6 as JArray, but got " + x)
      }
    }
  }

  implicit def tuple7Format[A :JF, B :JF, C :JF, D :JF, E :JF, F: JF, G: JF] = {
    new RootJsonFormat[(A, B, C, D, E, F, G)] {
      def write(t: (A, B, C, D, E, F, G)) = JArray(Array(t._1.toJson, t._2.toJson, t._3.toJson, t._4.toJson, t._5.toJson, t._6.toJson, t._7.toJson))
      def read(value: JValue) = value match {
        case JArray(Array(a, b, c, d, e, f, g)) =>
          (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D], e.convertTo[E], f.convertTo[F], g.convertTo[G])
        case x => deserializationError("Expected Tuple7 as JArray, but got " + x)
      }
    }
  }

}
