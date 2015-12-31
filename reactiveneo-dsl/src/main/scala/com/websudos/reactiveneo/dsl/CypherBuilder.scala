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

import com.websudos.reactiveneo.client.RestConnection
import com.websudos.reactiveneo.query.BuiltQuery
import com.websudos.reactiveneo.query.CypherKeywords._

import scala.annotation.implicitNotFound
import scala.concurrent.Future

sealed trait RelationshipBind

private[reactiveneo] abstract class RelationshipBound extends RelationshipBind

private[reactiveneo] abstract class RelationshipUnbound extends RelationshipBind

sealed trait WhereBind

private[reactiveneo] abstract class WhereBound extends WhereBind

private[reactiveneo] abstract class WhereUnbound extends WhereBind

sealed trait ReturnBind

private[reactiveneo] abstract class ReturnBound extends ReturnBind

private[reactiveneo] abstract class ReturnUnbound extends ReturnBind

sealed trait OrderBind

private[reactiveneo] abstract class OrderBound extends OrderBind

private[reactiveneo] abstract class OrderUnbound extends OrderBind

sealed trait LimitBind

private[reactiveneo] abstract class LimitBound extends LimitBind

private[reactiveneo] abstract class LimitUnbound extends LimitBind

/**
 * Evaluates query blocks and outputs Cypher clauses.
 */
private[reactiveneo] class CypherBuilder[P <: Pattern, WB <: WhereBind, RB <: ReturnBind, OB <: OrderBind, LB <: LimitBind, RT](
                                                                                                                                 pattern: P,
                                                                                                                                 builtQuery: BuiltQuery,
                                                                                                                                 context: CypherBuilderContext,
                                                                                                                                 ret: Option[ReturnExpression[RT]] = None) {

  def where(qb: BuiltQuery, criteria: BuiltQuery) = {
    qb.appendSpaced(WHERE).append(criteria)
  }


  @implicitNotFound("You cannot use two where clauses on a single query")
  final def where(condition: P => Criteria[_])(implicit ev: WB =:= WhereUnbound): CypherBuilder[P, WhereBound, RB, OB, LB, _] = {
    new CypherBuilder[P, WhereBound, RB, OB, LB, Any](
      pattern,
      where(builtQuery, condition(pattern).clause),
      context)
  }

  /**
    * Generate final query and result type tuple.
    */
  @implicitNotFound("You need to add return clause to capture the type of result")
  final def finalCypher: (String, ReturnExpression[RT]) = {
    (builtQuery.queryString, ret.get)
  }

  /**
    * Execute the request against provided REST endpoint
    * @param connection REST endpoint calling service.
    * @return Asynchronous response
    */
  def execute(implicit connection: RestConnection): Future[Seq[RT]] = connection.makeRequest(this).execute

}
