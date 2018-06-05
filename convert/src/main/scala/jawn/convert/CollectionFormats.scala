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

import scala.reflect.ClassTag

import jawn.ast._

trait CollectionFormats {

  /**
    * Supplies the JsonFormat for Lists.
    */
  implicit def listFormat[T :JsonFormat] = new RootJsonFormat[List[T]] {
    def write(list: List[T]) = JArray(list.map(_.toJson).toArray)
    def read(value: JValue): List[T] = value match {
      case JArray(elements) => elements.map(_.convertTo[T])(collection.breakOut)
      case x => deserializationError("Expected List as JsArray, but got " + x)
    }
  }

  /**
    * Supplies the JsonFormat for Arrays.
    */
  implicit def arrayFormat[T :JsonFormat :ClassTag] = new RootJsonFormat[Array[T]] {
    def write(array: Array[T]) = JArray(array.map(_.toJson))
    def read(value: JValue) = value match {
      case JArray(elements) => elements.map(_.convertTo[T])
      case x => deserializationError("Expected Array as JsArray, but got " + x)
    }
  }

  /**
    * Supplies the JsonFormat for Maps. The implicitly available JsonFormat for the key type K must
    * always write JStrings, otherwise a [[jawn.convert.SerializationException]] will be thrown.
    */
  implicit def mapFormat[K :JsonFormat, V :JsonFormat] = new RootJsonFormat[Map[K, V]] {
    def write(m: Map[K, V]) = JObject {
      m.map { field =>
        field._1.toJson match {
          case JString(x) => x -> field._2.toJson
          case x => throw new SerializationException("Map key must be formatted as JsString, not '" + x + "'")
        }
      } (collection.breakOut)
    }
    def read(value: JValue) = value match {
      case x: JObject => x.vs.map { field =>
        (JString(field._1).convertTo[K], field._2.convertTo[V])
      } (collection.breakOut)
      case x => deserializationError("Expected Map as JsObject, but got " + x)
    }
  }

  import collection.{immutable => imm}

  implicit def immIterableFormat[T :JsonFormat]   = viaSeq[imm.Iterable[T], T](seq => imm.Iterable(seq :_*))
  implicit def immSeqFormat[T :JsonFormat]        = viaSeq[imm.Seq[T], T](seq => imm.Seq(seq :_*))
  implicit def immIndexedSeqFormat[T :JsonFormat] = viaSeq[imm.IndexedSeq[T], T](seq => imm.IndexedSeq(seq :_*))
  implicit def immLinearSeqFormat[T :JsonFormat]  = viaSeq[imm.LinearSeq[T], T](seq => imm.LinearSeq(seq :_*))
  implicit def immSetFormat[T :JsonFormat]        = viaSeq[imm.Set[T], T](seq => imm.Set(seq :_*))
  implicit def vectorFormat[T :JsonFormat]        = viaSeq[Vector[T], T](seq => Vector(seq :_*))

  import collection._

  implicit def iterableFormat[T :JsonFormat]   = viaSeq[Iterable[T], T](seq => Iterable(seq :_*))
  implicit def seqFormat[T :JsonFormat]        = viaSeq[Seq[T], T](seq => Seq(seq :_*))
  implicit def indexedSeqFormat[T :JsonFormat] = viaSeq[IndexedSeq[T], T](seq => IndexedSeq(seq :_*))
  implicit def linearSeqFormat[T :JsonFormat]  = viaSeq[LinearSeq[T], T](seq => LinearSeq(seq :_*))
  implicit def setFormat[T :JsonFormat]()      = viaSeq[Set[T], T](seq => Set(seq :_*))

  /**
    * A JsonFormat construction helper that creates a JsonFormat for an Iterable type I from a builder function
    * List => I.
    */
  def viaSeq[I <: Iterable[T], T :JsonFormat](f: imm.Seq[T] => I): RootJsonFormat[I] = new RootJsonFormat[I] {
    def write(iterable: I) = JArray(iterable.map(_.toJson).toArray)
    def read(value: JValue) = value match {
      case JArray(elements) => f(elements.map(_.convertTo[T])(collection.breakOut))
      case x => deserializationError("Expected Collection as JsArray, but got " + x)
    }
  }
}
