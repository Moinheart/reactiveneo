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

import com.websudos.reactiveneo.query.{BuiltQuery, CypherKeywords}

/**
 * Query builder is responsible for encapsulating nodes information and selection criteria.
 * @param pattern Pattern this query is build against.
 * @param builtQuery Current query string.
 * @param context Map of added node types to corresponding alias value used in RETURN clause.
 */
private[reactiveneo] class MatchQuery[P <: Pattern, WB <: WhereBind, RB <: ReturnBind, OB <: OrderBind, LB <: LimitBind, RT](
                                                                                                                              pattern: P,
                                                                                                                              builtQuery: BuiltQuery,
                                                                                                                              context: CypherBuilderContext,
                                                                                                                              ret: Option[ReturnExpression[RT]] = None) extends CypherBuilder(pattern, builtQuery, context, ret) {

  def query: String = builtQuery.queryString

  final def returns[URT](ret: P => ReturnExpression[URT]): MatchQuery[P, WB, ReturnBound, OB, LB, URT] = {
    new MatchQuery[P, WB, ReturnBound, OB, LB, URT](
      pattern,
      builtQuery.appendSpaced(CypherKeywords.RETURN).appendSpaced(ret(pattern).query(context)),
      context,
      Some(ret(pattern)))
  }
}

private[reactiveneo] object MatchQuery {

  def createRootQuery[P <: Pattern](
    pattern: P,
    context: CypherBuilderContext): MatchQuery[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, _] = {
    pattern.foreach(context.nextLabel(_))
    val query = new BuiltQuery(CypherKeywords.MATCH).appendSpaced(pattern.queryClause(context))
    new MatchQuery[P, WhereUnbound, ReturnUnbound, OrderUnbound, LimitUnbound, Any](
      pattern,
      query,
      context)
  }
}
