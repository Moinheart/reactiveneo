package com.websudos.reactiveneo.dsl

import com.websudos.reactiveneo.client.RestConnection
import com.websudos.reactiveneo.query.{CypherKeywords, BuiltStatement}
import play.api.libs.json._

import scala.concurrent.Future

import MatchSet._

class MatchSet [P <: Pattern, WB <: WhereBind, RB <: ReturnBind, OB <: OrderBind, LB <: LimitBind, S](
                                                                                                           pattern: P,
                                                                                                           builtStatement: BuiltStatement,
                                                                                                           context: CypherBuilderContext,
                                                                                                           ret: Option[ReturnExpression[DefaultResult]] = None) extends CypherBuilder(pattern, builtStatement, context, ret) {

  def statement: String = builtStatement.statement

  final def set[US](s: P => SetExpression[US]): MatchSet[P, WB, ReturnBound, OB, LB, US] = {
    new MatchSet[P, WB, ReturnBound, OB, LB, US](
      pattern,
      builtStatement.appendSpaced(CypherKeywords.SET).appendSpaced(s(pattern).set(context))
        .appendSpaced(CypherKeywords.RETURN).appendSpaced("id(a)"),
      context,
      ret
    )
  }

  /**
    * Execute the request against provided REST endpoint
    * @param connection REST endpoint calling service.
    * @return Asynchronous response
    */
  override def execute(implicit connection: RestConnection): Future[Seq[DefaultResult]] = {
    connection.makeRequest[DefaultResult](this, parser).execute
  }
}

private[reactiveneo] object MatchSet {

  case class DefaultResult(id: Int)
  implicit val parser: Reads[DefaultResult] = __.read[Int].map { arr =>
    DefaultResult(arr)
  }

  def createRootMatchSet[P <: Pattern](
     pattern: P,
     context: CypherBuilderContext): MatchSet[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, _] = {
    pattern.foreach(context.nextLabel(_))
    val query = new BuiltStatement(CypherKeywords.MATCH).appendSpaced(pattern.queryClause(context))
    new MatchSet[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, Any](
      pattern,
      query,
      context)
  }
}
