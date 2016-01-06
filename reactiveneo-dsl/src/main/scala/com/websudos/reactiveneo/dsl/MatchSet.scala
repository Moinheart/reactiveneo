package com.websudos.reactiveneo.dsl

import com.websudos.reactiveneo.query.{CypherKeywords, BuiltStatement}

/**
  * Created by w00292111 on 2015/12/31.
  */
class MatchSet [P <: Pattern, WB <: WhereBind, RB <: ReturnBind, OB <: OrderBind, LB <: LimitBind, S, RT](
                                                                                                           pattern: P,
                                                                                                           builtStatement: BuiltStatement,
                                                                                                           context: CypherBuilderContext,
                                                                                                           ret: Option[ReturnExpression[RT]] = None) extends CypherBuilder(pattern, builtStatement, context, ret) {

  def statement: String = builtStatement.statement

  final def set[US](s: P => SetExpression[US]): MatchSet[P, WB, ReturnBound, OB, LB, US, RT] = {
    new MatchSet[P, WB, ReturnBound, OB, LB, US, RT](
      pattern,
      builtStatement.appendSpaced(CypherKeywords.SET).appendSpaced(s(pattern).set(context)),
      context,
      ret
    )
  }

/*  final def set[US, GO <: GraphObject[GO, _]](goSelection: GraphObjectSelection[GO])
                                               (implicit m: Manifest[GO]): MatchSet[P, WB, ReturnBound, OB, LB, US, RT] = {
    val obj = m.runtimeClass.newInstance().asInstanceOf[GO]
    new MatchSet[P, WB, ReturnBound, OB, LB, US, RT](
      pattern,
      builtStatement.appendSpaced(CypherKeywords.SET).appendSpaced(goSelection.setClauseString(context)),
      context,
      ret
    )
  }*/

/*  @inline
  private def predicatesSet(predicates: Predicate[_]*): Option[BuiltStatement] = {
    if(predicates.nonEmpty) {
      Some(predicates.tail.foldLeft(predicates.head.equalClause)((agg, next) => agg.append(",").append(next.equalClause)))
    } else {
      None
    }
  }*/
}

private[reactiveneo] object MatchSet {

  def createRootMatchSet[P <: Pattern](
     pattern: P,
     context: CypherBuilderContext): MatchSet[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, _, _] = {
    pattern.foreach(context.nextLabel(_))
    val query = new BuiltStatement(CypherKeywords.MATCH).appendSpaced(pattern.queryClause(context))
    new MatchSet[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, Any, Any](
      pattern,
      query,
      context)
  }
}
