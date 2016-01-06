package com.websudos.reactiveneo.dsl

import com.websudos.reactiveneo.attribute.Attribute
import com.websudos.reactiveneo.query.{CypherOperators, BuiltStatement}
import play.api.libs.json._

abstract class SetExpression[S] {
  /**
    * Builds part of the query string corresponding to this expression.
    * @return Returns the built query.
    */
  def set(context: CypherBuilderContext): BuiltStatement

}


/**
  * Expression that defines return value being a graph object.
  * @param go Graph object being returned.
  * @tparam GO Class of the graph object definition
  * @tparam R Returned type - graph object record type in this case
  */
case class ObjectSetExpression[GO <: GraphObject[GO, R], R](go: GraphObject[GO, R]) extends SetExpression[R] {

  //@TODO
  override def set(context: CypherBuilderContext): BuiltStatement = {
    context.resolve(go)
  }


}


/**
  * Expression that defines return data being an attribute of a graph object.
  * @param predicates Attribute type-value to be set.
  * @tparam R Returned type - an attribute concrete type in this case.
  */
case class AttributeSetExpression[GO <: GraphObject[GO, R], R, T](
                                                                   predicates: Predicate[_]*
                                                                      ) extends SetExpression[T] {


  override def set(context: CypherBuilderContext): BuiltStatement = {
    if(predicates.nonEmpty) {
      Some(predicates.tail.foldLeft(predicates.head.equalClause(context)) {
        (agg, next) =>
          agg.append(",").append(next.equalClause(context))
      }).get
    } else {
      BuiltStatement("")
    }
  }

  /*@TODO*/
  def +++(predicate: => Predicate[_]): AttributeSetExpression[GO, R, T] = {
    AttributeSetExpression[GO, R, T](predicates :+ predicate: _*)
  }

}


/**
  * Implicits converting [[com.websudos.reactiveneo.dsl.GraphObject]] to [[com.websudos.reactiveneo.dsl.ObjectSetExpression]]
  * and [[com.websudos.reactiveneo.dsl.Predicate]] to [[com.websudos.reactiveneo.dsl.AttributeSetExpression]]
  */
trait SetImplicits {

  implicit def predicateToSetExpression[GO <: GraphObject[GO, R], R, T](predicate: Predicate[_]): AttributeSetExpression[GO, R, T] = {
    AttributeSetExpression(predicate)
  }

  implicit def graphObjectToSetExpression[GO <: GraphObject[GO, R], R](go: GraphObject[GO, R]): ObjectSetExpression[GO, R] = {
    ObjectSetExpression(go)
  }
}