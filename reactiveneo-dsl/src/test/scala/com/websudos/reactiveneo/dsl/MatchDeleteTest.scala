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

import org.scalatest.{FlatSpec, Matchers}

class MatchDeleteTest extends FlatSpec with Matchers {

  implicit val context: CypherBuilderContext = new CypherBuilderContext

  it should "build a simple set with a predicate" in {
    TestNode(_.name := "Tom").delete { case n ~~ _ => n }.statement shouldEqual
      "MATCH (a:TestNode {name:'Tom'})   DELETE a RETURN id(a) "
  }

}