/*
 * Copyright (C) 2009-2011 Mathias Doenitz
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

package jawn

import jawn.ast._

package object convert {

  type JsField = (String, JValue)

  def deserializationError(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) = throw DeserializationException(msg, cause, fieldNames)
  def serializationError(msg: String) = throw new SerializationException(msg)

  case class DeserializationException(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)

  def jsonReader[T](implicit reader: JsonReader[T]) = reader
  def jsonWriter[T](implicit writer: JsonWriter[T]) = writer

  implicit class RichAny[T : JsonWriter](value: T) {
    def toJson: JValue = implicitly[JsonWriter[T]].write(value)
  }

  implicit class RichString(val string: String) extends AnyVal {
    def parseJson: JValue = JParser.parseUnsafe(string)
  }

  implicit class JValueExtensions(val value: JValue) extends AnyVal {
    def convertTo[T : JsonReader]: T = implicitly[JsonReader[T]].read(value)
    def compactPrint = FastRenderer.render(value)
  }
}
