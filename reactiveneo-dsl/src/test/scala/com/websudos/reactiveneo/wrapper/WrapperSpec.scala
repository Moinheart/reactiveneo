package com.websudos.reactiveneo.wrapper

import com.websudos.reactiveneo.client.RestConnection
import org.scalatest.FlatSpecLike

case class Test(s: String, i: Int, ji: java.lang.Integer, d: Double, l: Long, b: Boolean)

case class TestX(name: String)

class WrapperSpec extends Wrapper with FlatSpecLike {
  "Neo4jWrapper" must "CaseClass serialize" in {
    implicit val tx = Transaction()
    val node = createNode(Test("Something", 1, 2, 3.3, 10, true))

    assert(node("s").getOrElse("error") == "Something")
    assert(node("i").getOrElse(0) == 1)
    assert(node("b").getOrElse(false))

    assert(node.labels == Set("Test"))
  }

  "Neo4jWrapper" must "update node's property" in {
    implicit val tx = Transaction()
    val node = createNode(Test("Something", 1, 2, 3.3, 10, true))

    node("s") = "Nothing"
    assert(node("s").getOrElse("error") == "Nothing")
  }

  "Neo4jWrapper" must "get case class's name" in {
    implicit val tx = Transaction()
    val t = Test("Something", 1, 2, 3.3, 10, true)

    assert(t.getClass.getSimpleName == "Test")
  }

  "Neo4jWrapper" must "create a simple relationship" in {
    implicit val tx = Transaction()
    val start = createNode("Node")
    val end = createNode("Node")
    val relType = "rel"

    start --> relType --> end

    assert(start.getRelationships.head == Relationship(start, RelationshipType("rel"), end))
    assert(end.getRelationships.head == Relationship(start, RelationshipType("rel"), end))
  }

  "Neo4jWrapper" must "create a simple reversed relationship" in {
    implicit val tx = Transaction()
    val start = createNode("Node")
    val end = createNode("Node")
    val relType = "rel"

    end <-- relType <-- start

    assert(end.getRelationships.head == Relationship(start, RelationshipType("rel"), end))
    assert(start.getRelationships.head == Relationship(start, RelationshipType("rel"), end))
  }

  "Neo4jWrapper" must "create a CaseClass relationship" in {
    implicit val tx = Transaction()
    val start = createNode("Node")
    val end = createNode("Node")
    val relType = "rel"

    val rel = start --> relType --> end < Test("Something", 1, 2, 3.3, 10, true)

    assert(rel == Relationship(start, RelationshipType("rel"), end))
    assert(rel("s").getOrElse("error") == "Something")
    assert(rel("i").getOrElse(0) == 1)
    assert(rel("b").getOrElse(false))
  }

  "Neo4jWrapper" must "create a null String rel relationship" in {
    implicit val tx = Transaction()
    val start = createNode("Node")
    val end = createNode("Node")
    val relType = ""

    val rel = start --> relType --> end < Test("Something", 1, 2, 3.3, 10, true)

    assert(rel == Relationship(start, RelationshipType(""), end))
    assert(rel("s").getOrElse("error") == "Something")
    assert(rel("i").getOrElse(0) == 1)
    assert(rel("b").getOrElse(false))
  }

  "Neo4jWrapper" must "create relationships of the same direction" in {
    implicit val tx = Transaction()
    val start = createNode("Node")
    val middle = createNode("Node")
    val end = createNode("Node")

    start --> "rel1" --> middle --> "rel2" --> end

    import RelationshipDirection._
    assert(start.getRelationships(DIRECTION_OUTGOING).head == Relationship(start, RelationshipType("rel1"), middle))
    assert(middle.getRelationships(DIRECTION_INGOING).head == Relationship(start, RelationshipType("rel1"), middle))
    assert(middle.getRelationships(DIRECTION_OUTGOING).head == Relationship(middle, RelationshipType("rel2"), end))
    assert(end.getRelationships(DIRECTION_INGOING).head == Relationship(middle, RelationshipType("rel2"), end))
  }

  "Neo4jWrapper" must "test transaction" in {
    implicit val connection = RestConnection("localhost", 7474, "neo4j", "password")

    withTx {
      implicit tx =>
      val start = createNode(Test("start", 1, 2, 3.3, 10, true))
      val middle = createNode(Test("middle", 1, 2, 3.3, 10, true))
      val end = createNode(Test("end", 1, 2, 3.3, 10, true))

      start --> "rel1" --> middle --> "rel2" --> end

      import RelationshipDirection._
      assert(start.getRelationships(DIRECTION_OUTGOING).head == Relationship(start, RelationshipType("rel1"), middle))
      assert(middle.getRelationships(DIRECTION_INGOING).head == Relationship(start, RelationshipType("rel1"), middle))
      assert(middle.getRelationships(DIRECTION_OUTGOING).head == Relationship(middle, RelationshipType("rel2"), end))
      assert(end.getRelationships(DIRECTION_INGOING).head == Relationship(middle, RelationshipType("rel2"), end))
    }
  }

  "Neo4jWrapper" must "test case class relationship transaction" in {
    implicit val connection = RestConnection("localhost", 7474, "neo4j", "password")

    withTx {
      implicit tx =>
        val start = createNode(Test("startxxx", 1, 2, 3.3, 10, true))
        val middle = createNode(Test("middlexxx", 1, 2, 3.3, 10, true))
        val end = createNode(Test("endxxx", 1, 2, 3.3, 10, true))

        start --> "rel1" --> middle --> "rel2" --> end < Test("RELLL", 1, 2, 3.3, 10, true)

        import RelationshipDirection._
        assert(start.getRelationships(DIRECTION_OUTGOING).head == Relationship(start, RelationshipType("rel1"), middle))
        assert(middle.getRelationships(DIRECTION_INGOING).head == Relationship(start, RelationshipType("rel1"), middle))
        assert(middle.getRelationships(DIRECTION_OUTGOING).head == Relationship(middle, RelationshipType("rel2"), end))
        assert(end.getRelationships(DIRECTION_INGOING).head == Relationship(middle, RelationshipType("rel2"), end))
    }
  }

/*  "Neo4jWrapper" must "be serializable with Test" in {

    val o = Test("sowas", 1, 2, 3.3, 10, true)

    withTx {
      implicit tx =>
        val node = createNode(o)
        val oo1 = Wrapper.deSerialize[Test](node)
        assert(oo1 == o)

        val oo2 = node.toCC[Test]
        assert(oo2.get == o)
    }
  }*/

}
