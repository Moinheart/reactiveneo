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
package com.websudos.reactiveneo.dsl

import com.websudos.reactiveneo.attribute._
import com.websudos.reactiveneo.query.{ValueFormatter, CypherOperators, BuiltStatement}

/**
 * Criteria applied to a graph object. Cypher representation of a pattern is ```(n: Label { name = "Mark" })```
 */
private[reactiveneo] case class GraphObjectSelection[Owner <: GraphObject[Owner, _]](
   owner: Owner,
   predicates: Predicate[_]*) {

  @inline
  private def predicatesQuery: Option[BuiltStatement] = {
    if(predicates.nonEmpty) {
      Some(predicates.tail.foldLeft(predicates.head.colonClause)((agg, next) => agg.append(",").append(next.colonClause)))
    } else {
      None
    }
  }

/*  /*@TODO to refactor*/
  @inline
  private def predicatesSet(context: CypherBuilderContext): Option[BuiltStatement] = {
    if(predicates.nonEmpty) {
      Some(predicates.tail.foldLeft(predicates.head.equalClause(vonyrcy)) {
        (agg, next) =>
          agg.append(",").append(context.resolve(next.attribute.asInstanceOf[Attribute[Owner, _ , _]].owner.asInstanceOf[GraphObject[_, _]]) + CypherOperators.DOT + next.equalClause)
      })
    } else {
      None
    }
  }*/


/*  def setClause(context: CypherBuilderContext): BuiltStatement = {
    val alias = context.resolve(owner)
    BuiltStatement(s"$alias:${owner.objectName}")
      .append(predicatesSet.map(" {" + _.statement + "}").getOrElse(""))
  }*/

/*  def setClauseString(context: CypherBuilderContext): String = {
    predicatesSet(context).map(_.statement).get
  }*/

  /**
   * Builds a query string of alias, object name and criteria if some.
   */
  def queryClause(context: CypherBuilderContext): BuiltStatement = {
    val alias = context.resolve(owner)
    val (open:String, close:String) = this.owner match {
      case _:Relationship[_,_] => "[" -> "]"
      case _                   => "(" -> ")"
    }
    BuiltStatement(s"$alias:${owner.objectName}")
      .append(predicatesQuery.map(" {" + _.statement + "}").getOrElse(""))
      .wrapped(open, close)
  }

  override def toString: String = {
    owner.objectName + predicatesQuery.map(" {" + _.statement + "}")
  }
}

/**
 * Predicate filtering nodes by attribute value.
 * @param attribute Attribute to filter.
 * @param value Lookup value
 */
private[reactiveneo] case class Predicate[T](
  attribute: AbstractAttribute[T], value: T)(implicit formatter: ValueFormatter[T]) {

  val colonClause: BuiltStatement = {
    if(value == null)
      throw new IllegalArgumentException("NULL value is not allowed in predicate.")
    new BuiltStatement(attribute.name).append(CypherOperators.COLON).append(value)
  }

  def equalClause(context: CypherBuilderContext): BuiltStatement = {
    if(value == null)
      throw new IllegalArgumentException("NULL value is not allowed in predicate.")
    new BuiltStatement(context.resolve(attribute.asInstanceOf[Attribute[_, _ , _]].owner.asInstanceOf[GraphObject[_, _]]) + CypherOperators.DOT + attribute.name).append(CypherOperators.EQ).append(value)
  }
}

trait PredicateOps {

  implicit class PredicateFunctions[V](attr: AbstractAttribute[V])(implicit formatter: ValueFormatter[V]) {

    implicit def :=(value: V): Predicate[V] = {
      new Predicate[V](attr, value)
    }

  }
}