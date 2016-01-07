package com.websudos.reactiveneo.dsl

import com.websudos.reactiveneo.client.RestConnection
import com.websudos.reactiveneo.query.{CypherKeywords, BuiltStatement}
import play.api.libs.json._

import scala.concurrent.Future

import MatchDelete._

class MatchDelete [P <: Pattern, WB <: WhereBind, RB <: ReturnBind, OB <: OrderBind, LB <: LimitBind, D](
                                                                                                       pattern: P,
                                                                                                       builtStatement: BuiltStatement,
                                                                                                       context: CypherBuilderContext,
                                                                                                       ret: Option[ReturnExpression[DefaultResult]] = None) extends CypherBuilder(pattern, builtStatement, context, ret) {

  def statement: String = builtStatement.statement

  final def delete[UD](d: P => DeleteExpression[UD]): MatchDelete[P, WB, ReturnBound, OB, LB, UD] = {
    new MatchDelete[P, WB, ReturnBound, OB, LB, UD](
      pattern,
      builtStatement.appendSpaced(CypherKeywords.DELETE).appendSpaced(d(pattern).delete(context))
        .appendSpaced(CypherKeywords.RETURN).appendSpaced("id(a)"),
      context,
      ret
    )
  }
  /**
    * Execute the request against provided REST endpoint
    * @param connection REST endpoint calling service.
    * @return Asynchronous response
    **/
  override def execute(implicit connection: RestConnection): Future[Seq[DefaultResult]] = {
    connection.makeRequest[DefaultResult](this, parser).execute
  }
}

private[reactiveneo] object MatchDelete {

  case class DefaultResult(id: Int)
  implicit val parser: Reads[DefaultResult] = __.read[Int].map { arr =>
    DefaultResult(arr)
  }

  def createRootMatchDelete[P <: Pattern](
                                        pattern: P,
                                        context: CypherBuilderContext): MatchDelete[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, _] = {
    pattern.foreach(context.nextLabel(_))
    val query = new BuiltStatement(CypherKeywords.MATCH).appendSpaced(pattern.queryClause(context))
    new MatchDelete[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, Any](
      pattern,
      query,
      context)
  }
}
