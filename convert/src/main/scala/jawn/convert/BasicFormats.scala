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
  * Provides the JsonFormats for the most important Scala types.
  */
trait BasicFormats {

  implicit object IntJsonFormat extends JsonFormat[Int] {
    def write(x: Int) = JNum(x)
    def read(value: JValue) = value match {
      case x: JValue => x.asInt
      case x         => deserializationError("Expected Int as JNum, but got " + x)
    }
  }

  implicit object LongJsonFormat extends JsonFormat[Long] {
    def write(x: Long) = JNum(x)
    def read(value: JValue) = value match {
      case x: JNum => x.asLong
      case x       => deserializationError("Expected Long as JNum, but got " + x)
    }
  }

  implicit object FloatJsonFormat extends JsonFormat[Float] {
    def write(x: Float) = JNum(x)
    def read(value: JValue) = value match {
      case x: JNum => x.asDouble.toFloat
      case JNull   => Float.NaN
      case x => deserializationError("Expected Float as JNum, but got " + x)
    }
  }

  implicit object DoubleJsonFormat extends JsonFormat[Double] {
    def write(x: Double) = JNum(x)
    def read(value: JValue) = value match {
      case x: JNum => x.asDouble
      case JNull   => Double.NaN
      case x => deserializationError("Expected Double as JNum, but got " + x)
    }
  }

  implicit object ByteJsonFormat extends JsonFormat[Byte] {
    def write(x: Byte) = JNum(x)
    def read(value: JValue) = value match {
      case x: JNum => x.asInt.toByte
      case x       => deserializationError("Expected Byte as JNum, but got " + x)
    }
  }

  implicit object ShortJsonFormat extends JsonFormat[Short] {
    def write(x: Short) = JNum(x)
    def read(value: JValue) = value match {
      case x: JNum => x.asInt.toShort
      case x       => deserializationError("Expected Short as JNum, but got " + x)
    }
  }

  implicit object BigDecimalJsonFormat extends JsonFormat[BigDecimal] {
    def write(x: BigDecimal) = {
      require(x ne null)
      DeferNum(x.toString())
    }
    def read(value: JValue) = value match {
      case x: JNum => x.asBigDecimal
      case x       => deserializationError("Expected BigDecimal as JNum, but got " + x)
    }
  }

  implicit object BigIntJsonFormat extends JsonFormat[BigInt] {
    def write(x: BigInt) = {
      require(x ne null)
      DeferLong(x.toString())
    }
    def read(value: JValue) = value match {
      case x: JNum => x.asBigInt
      case x => deserializationError("Expected BigInt as JNum, but got " + x)
    }
  }

  implicit object UnitJsonFormat extends JsonFormat[Unit] {
    def write(x: Unit) = JNum(1)
    def read(value: JValue) {}
  }

  implicit object BooleanJsonFormat extends JsonFormat[Boolean] {
    def write(x: Boolean) = JBool(x)
    def read(value: JValue) = value match {
      case JTrue => true
      case JFalse => false
      case x => deserializationError("Expected JsBoolean, but got " + x)
    }
  }

  implicit object CharJsonFormat extends JsonFormat[Char] {
    def write(x: Char) = JString(String.valueOf(x))
    def read(value: JValue) = value match {
      case JString(x) if x.length == 1 => x.charAt(0)
      case x => deserializationError("Expected Char as single-character JsString, but got " + x)
    }
  }

  implicit object StringJsonFormat extends JsonFormat[String] {
    def write(x: String) = {
      require(x ne null)
      JString(x)
    }
    def read(value: JValue) = value match {
      case JString(x) => x
      case x => deserializationError("Expected String as JsString, but got " + x)
    }
  }

  implicit object SymbolJsonFormat extends JsonFormat[Symbol] {
    def write(x: Symbol) = JString(x.name)
    def read(value: JValue) = value match {
      case JString(x) => Symbol(x)
      case x => deserializationError("Expected Symbol as JsString, but got " + x)
    }
  }
}
