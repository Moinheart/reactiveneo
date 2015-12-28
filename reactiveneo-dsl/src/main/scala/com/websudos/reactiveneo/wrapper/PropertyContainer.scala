package com.websudos.reactiveneo.wrapper

trait PropertyContainer {
  var props: Map[String, AnyRef] = Map()
  var labels: Set[String] = Set()

  def setProperty(key: String, value: AnyRef): Unit = props = props.updated(key, value)

  def getProperty(key: String): Any = props(key)

  def removeProperty(key: String): Any = props -= key

  def hasProperty(key: String): Boolean = props.contains(key)

  def addLabel(label: String): Unit = labels += label

  def getPropertyKeys: Iterable[String] = props.keys
}

class Node extends PropertyContainer {
  var inGoingRelationships: Set[Relationship] = Set()
  var outGoingRelationships: Set[Relationship] = Set()
  private var isExisting = false
  var id: String = _


  def createRelationshipTo(that: Node, relType: RelationshipType): Relationship = {
    val rel = Relationship(this, relType, that)
    outGoingRelationships += rel
    that.inGoingRelationships += rel
    rel
  }

  def getRelationships: Set[Relationship] = {
    inGoingRelationships ++ outGoingRelationships
  }

  import RelationshipDirection._
  def getRelationships(relDirection: RelationshipDirection): Set[Relationship] = {
    relDirection match {
      case DIRECTION_INGOING => inGoingRelationships
      case DIRECTION_OUTGOING => outGoingRelationships
    }
  }

  def hasCreated: Boolean = isExisting

  def createStatement: String = {
    val labelSection = labels.foldLeft("") {
      (str, label) =>
        str + (":" + label + " ")
    }

    val propertySection = props.foldLeft("") {
      (str, prop) =>
        str + prop._1 + ":'" + prop._2 + "',"
    }.dropRight(1)

    if (props.nonEmpty) {
      s" ($id $labelSection  {$propertySection}),"
    }
    else {
      s" ($id $labelSection),"
    }
  }

  def createRelationshipStatement: String = {
    inGoingRelationships.foldLeft[String]("") {
      (state, rel) =>
        state + rel.createStatement
    }
  }
}

case class Relationship(from: Node, relType: RelationshipType, to: Node) extends PropertyContainer {
  def createStatement: String = {
    val labelSection = labels.foldLeft("") {
      (str, label) =>
        str + (":" + label + " ")
    }

    val propertySection = props.foldLeft("") {
      (str, prop) =>
        str + prop._1 + ":'" + prop._2 + "',"
    }.dropRight(1)

    if (props.nonEmpty) {
      s" (${from.id})-[:${relType.name} {$propertySection}]->(${to.id}),"
    }
    else {
      s" (${from.id})-[:${relType.name}]->(${to.id}),"
    }
  }
}

case class RelationshipType(name: String)

object RelationshipDirection extends Enumeration {
  type RelationshipDirection = Value
  val DIRECTION_INGOING = Value(0)
  val DIRECTION_OUTGOING = Value(1)
}