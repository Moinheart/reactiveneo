package com.websudos.reactiveneo.dsl

import com.websudos.reactiveneo.query.{CypherKeywords, BuiltQuery}

/**
  * Created by w00292111 on 2015/12/31.
  */
class MatchSet [P <: Pattern, WB <: WhereBind, RB <: ReturnBind, OB <: OrderBind, LB <: LimitBind, RT](
    pattern: P,
    builtQuery: BuiltQuery,
    context: CypherBuilderContext,
    //set: Option[SetPattern] = None,
    ret: Option[ReturnExpression[RT]] = None) extends CypherBuilder(pattern, builtQuery, context, ret) {

  def query: String = builtQuery.queryString

  //final def set()

  final def returns[URT](ret: P => ReturnExpression[URT]): MatchSet[P, WB, ReturnBound, OB, LB, URT] = {
    new MatchSet[P, WB, ReturnBound, OB, LB, URT](
      pattern,
      builtQuery.appendSpaced(CypherKeywords.RETURN).appendSpaced(ret(pattern).query(context)),
      context,
      Some(ret(pattern)))
  }

}

private[reactiveneo] object MatchSet {

  def createRootQuery[P <: Pattern](
     pattern: P,
     context: CypherBuilderContext): MatchSet[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, _] = {
    pattern.foreach(context.nextLabel(_))
    val query = new BuiltQuery(CypherKeywords.MATCH).appendSpaced(pattern.queryClause(context))
    new MatchSet[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, Any](
      pattern,
      query,
      context)
  }
}
