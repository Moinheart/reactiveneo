package com.websudos.reactiveneo.dsl

import com.websudos.reactiveneo.attribute.Attribute
import com.websudos.reactiveneo.query.{CypherOperators, BuiltStatement}
import play.api.libs.json._

abstract class DeleteExpression[D] {
  /**
    * Builds part of the query string corresponding to this expression.
    * @return Returns the built query.
    */
  def delete(context: CypherBuilderContext): BuiltStatement

}


/**
  * Expression that defines return value being a graph object.
  * @param go Graph object being returned.
  * @tparam GO Class of the graph object definition
  * @tparam R Returned type - graph object record type in this case
  */
case class ObjectDeleteExpression[GO <: GraphObject[GO, R], R](go: GraphObject[GO, R]) extends DeleteExpression[R] {

  //@TODO
  override def delete(context: CypherBuilderContext): BuiltStatement = {
    context.resolve(go)
  }


}

/**
  * Implicits converting [[com.websudos.reactiveneo.dsl.GraphObject]] to [[com.websudos.reactiveneo.dsl.ObjectSetExpression]]
  * and [[com.websudos.reactiveneo.dsl.Predicate]] to [[com.websudos.reactiveneo.dsl.AttributeSetExpression]]
  */
trait DeleteImplicits {

  implicit def graphObjectToDeleteExpression[GO <: GraphObject[GO, R], R](go: GraphObject[GO, R]): ObjectDeleteExpression[GO, R] = {
    ObjectDeleteExpression(go)
  }
}