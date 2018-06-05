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
  * Provides additional JsonFormats and helpers
  */
trait AdditionalFormats {

  implicit object JsValueFormat extends JsonFormat[JValue] {
    def write(value: JValue) = value
    def read(value: JValue) = value
  }

  implicit object RootJsObjectFormat extends RootJsonFormat[JObject] {
    def write(value: JObject) = value
    def read(value: JValue) = value match {
      case x: JObject => x
      case _ => deserializationError("JSON Object expected")
    }
  }

  implicit object RootJsArrayFormat extends RootJsonFormat[JArray] {
    def write(value: JArray) = value
    def read(value: JValue) = value match {
      case x: JArray => x
      case _ => deserializationError("JSON array expected")
    }
  }

  /**
    * Constructs a JsonFormat from its two parts, JsonReader and JsonWriter.
    */
  def jsonFormat[T](reader: JsonReader[T], writer: JsonWriter[T]) = new JsonFormat[T] {
    def write(obj: T) = writer.write(obj)
    def read(json: JValue) = reader.read(json)
  }

  /**
    * Constructs a RootJsonFormat from its two parts, RootJsonReader and RootJsonWriter.
    */
  def rootJsonFormat[T](reader: RootJsonReader[T], writer: RootJsonWriter[T]) =
    rootFormat(jsonFormat(reader, writer))

  /**
    * Turns a JsonWriter into a JsonFormat that throws an UnsupportedOperationException for reads.
    */
  def lift[T](writer :JsonWriter[T]) = new JsonFormat[T] {
    def write(obj: T): JValue = writer.write(obj)
    def read(value: JValue) =
      throw new UnsupportedOperationException("JsonReader implementation missing")
  }

  /**
    * Turns a RootJsonWriter into a RootJsonFormat that throws an UnsupportedOperationException for reads.
    */
  def lift[T](writer :RootJsonWriter[T]): RootJsonFormat[T] =
    rootFormat(lift(writer :JsonWriter[T]))

  /**
    * Turns a JsonReader into a JsonFormat that throws an UnsupportedOperationException for writes.
    */
  def lift[T <: AnyRef](reader :JsonReader[T]) = new JsonFormat[T] {
    def write(obj: T): JValue =
      throw new UnsupportedOperationException("No JsonWriter[" + obj.getClass + "] available")
    def read(value: JValue) = reader.read(value)
  }

  /**
    * Turns a RootJsonReader into a RootJsonFormat that throws an UnsupportedOperationException for writes.
    */
  def lift[T <: AnyRef](reader :RootJsonReader[T]): RootJsonFormat[T] =
    rootFormat(lift(reader :JsonReader[T]))

  /**
    * Lazy wrapper around serialization. Useful when you want to serialize (mutually) recursive structures.
    */
  def lazyFormat[T](format: => JsonFormat[T]) = new JsonFormat[T] {
    lazy val delegate = format
    def write(x: T) = delegate.write(x)
    def read(value: JValue) = delegate.read(value)
  }

  /**
    * Explicitly turns a JsonFormat into a RootJsonFormat.
    */
  def rootFormat[T](format: JsonFormat[T]) = new RootJsonFormat[T] {
    def write(obj: T) = format.write(obj)
    def read(json: JValue) = format.read(json)
  }

  /**
    * Wraps an existing JsonReader with Exception protection.
    */
  def safeReader[A :JsonReader] = new JsonReader[Either[Exception, A]] {
    def read(json: JValue) = {
      try {
        Right(json.convertTo[A])
      } catch {
        case e: Exception => Left(e)
      }
    }
  }

}
