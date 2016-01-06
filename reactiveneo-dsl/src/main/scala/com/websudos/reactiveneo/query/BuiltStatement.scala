/*
 * Copyright 2014 websudos ltd.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.websudos.reactiveneo.query

/**
 * Expanded match query that is enriched with every DSL query function call.
 */
case class BuiltStatement(statement: String) {

  def this() = this("")

  def wrap(str: String,open: String = "(", close: String = ")"): BuiltStatement = pad.append(open).append(str).append(close)
  def wrap(query: BuiltStatement): BuiltStatement = wrap(query.statement)
  def wrapped(open: String = "(", close: String = ")"): BuiltStatement = BuiltStatement(s"$open$statement$close")

  def append(str: String): BuiltStatement = new BuiltStatement(statement + str)
  def append(query: BuiltStatement): BuiltStatement = new BuiltStatement(statement + query.statement)
  def append[T](value: T)(implicit formatter: ValueFormatter[T]): BuiltStatement = append(formatter.format(value))

  def appendSpaced(str: String): BuiltStatement = appendSpaced(new BuiltStatement(str))
  def appendSpaced(query: BuiltStatement): BuiltStatement = (if(spaced) this else append(" ")).append(query).append(" ")
  def appendSpaced[T](value: T)(implicit formatter: ValueFormatter[T]): BuiltStatement =
    appendSpaced(new BuiltStatement(formatter.format(value)))

  def space: BuiltStatement = append(" ")

  def spaced: Boolean = statement.endsWith(" ")
  def pad: BuiltStatement = if (spaced) this else BuiltStatement(statement + " ")
  def forcePad: BuiltStatement = BuiltStatement(statement + " ")
  def trim: BuiltStatement = BuiltStatement(statement.trim)

  override def toString: String = statement
}

/**
 * Type class used for formatting arbitrary values when building query string.
 * @tparam T Type of value.
 */
trait ValueFormatter[T] {

  def format(value: T): String

}

object DefaultFormatters {

  class IntegerFormatter extends ValueFormatter[Int] {
    override def format(value: Int): String = value.toString
  }

  class DoubleFormatter extends ValueFormatter[Double] {
    override def format(value: Double): String = value.toString
  }

  class BooleanFormatter extends ValueFormatter[Boolean] {
    override def format(value: Boolean): String = if(value) "TRUE" else "FALSE"
  }

  class StringFormatter extends ValueFormatter[String] {
    override def format(value: String): String = s"'$value'"
  }
}