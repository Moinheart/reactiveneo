package com.websudos.reactiveneo.wrapper

import com.websudos.reactiveneo.client.RestConnection
import play.api.libs.json._

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.currentMirror
import com.websudos.reactiveneo.util.CaseClassDeserializer
import com.websudos.reactiveneo.util.CaseClassDeserializer._

import scala.concurrent._
import scala.concurrent.duration._

trait Wrapper extends WrapperImplicits {
  private var index: Int = _

  def withTx[T <: Any](operation: Transaction => T)(implicit connection: RestConnection): Unit = {
    index = 0
    val tx = beginTx
    try {
      operation(tx)
      tx.execute()
    } finally {
      tx.close()
    }
  }

  //create a new Node and add labels
  def createNode(labels: String*)(implicit tx: Transaction): Node = {
    val node = new Node
    for (label <- labels) {
      node.addLabel(label)
    }
    tx.addNode(node)
    node.id = "n" + index
    index += 1
    node
  }

  //create and serialize a case class
  def createNode(cc: AnyRef, labels: String*)(implicit tx: Transaction): Node =
    Wrapper.serialize(cc,  createNode(labels: _*))

  private def beginTx: Transaction = {
    Transaction()
  }
}

/**
  * Neo4jWrapper Object
  */
object Wrapper extends WrapperImplicits {
  def serialize[T <: PropertyContainer](cc: AnyRef, pc: PropertyContainer)(implicit exclusions: Option[List[String]] = None): T = {
    CaseClassDeserializer.serialize(cc).foreach {
      //case (name, null) =>
      case (name: String, value: AnyRef) => {
        if (exclusions.isEmpty || (exclusions.isDefined && !exclusions.get.contains(name))){
          if (value != None){
            if (value.getClass().toString() == "class scala.Some"){
              val actualValue = value.asInstanceOf[Some[_]].get
              pc.setProperty(name, actualValue.asInstanceOf[AnyRef])
            }else{
              pc.setProperty(name, value)
            }
          }else{
            // Remove the property from node if it is there
            if (pc.hasProperty(name)){
              pc.removeProperty(name)
            }
          }
        }
      }
    }
    pc.addLabel(currentMirror.reflect(cc).symbol.name.toTypeName.decodedName.toString)
    pc.asInstanceOf[T]
  }

  def toCC[T: Manifest](pc: PropertyContainer)(implicit customConverters: Option[Map[String, AnyRef => AnyRef]] = None): Option[T] = {
    val kv = for (k <- pc.getPropertyKeys; v = pc.getProperty(k)) yield k -> v.asInstanceOf[AnyRef]
    deserializeManifest[T](manifest[T].runtimeClass, kv.toMap)
  }

  def deSerialize[T: Manifest](pc: PropertyContainer): T = {
    toCC[T](pc) match {
      case Some(t) => t
      case _ => throw new IllegalArgumentException("given Case Class: " +
        manifest[T].runtimeClass.getName + " does not fit to serialized properties")
    }
  }
}

/**
  * creates incoming and outgoing relationships
  */
case class NodeRelationshipMethods(node: Node, rel: Relationship = null) {
  def -->(relType: RelationshipType) = OutgoingRelationshipBuilder(node, relType)

  def <--(relType: RelationshipType) = IncomingRelationshipBuilder(node, relType)

  //use this to get the created relationship object
  //start --> "KNOWS" --> end <()
  def <() = rel

  //start --> "KNOWS" --> end <(MyCaseClass(...))
  def <(cc: AnyRef): Relationship = Wrapper.serialize(cc, rel)
}

/**
  * Half-way through building an outgoing relationship
  */
case class OutgoingRelationshipBuilder(fromNode: Node, relType: RelationshipType) {
  def -->(toNode: Node) = {
    val rel = fromNode.createRelationshipTo(toNode, relType)
    NodeRelationshipMethods(toNode, rel)
  }
}

/**
  * Half-way through building an incoming relationship
  */
case class IncomingRelationshipBuilder(toNode: Node, relType: RelationshipType) {
  def <--(fromNode: Node) = {
    val rel = fromNode.createRelationshipTo(toNode, relType)
    NodeRelationshipMethods(fromNode, rel)
  }
}

/**
  * convenience for handling properties
  */
class RichPropertyContainer(propertyContainer: PropertyContainer) {

  /**
    * type of properties is normally Object
    * use type identifier T to cast it
    */
  def apply[T](property: String): Option[T] =
    propertyContainer.hasProperty(property) match {
      case true => Some(propertyContainer.getProperty(property).asInstanceOf[T])
      case _ => None
    }

  /**
    * updates the property
    * <code>node("property") = value</code>
    */
  def update(property: String, value: AnyRef): Unit = value match {
    case null =>
    case None =>
    case _ => propertyContainer.setProperty(property, value)
  }
}

private[reactiveneo] case class Transaction() {
  var nodes: Set[Node] = Set()
  var relationships: Set[Relationship] = Set()


  def addNode(node: Node): Unit = {
    nodes += node
  }

  def execute()(implicit connection: RestConnection): Boolean = {
    val createNodeSec = nodes.foldLeft[String]("") {
      (state, node) =>
        state + node.createStatement
    }.dropRight(1)

    val createRelationSec = nodes.foldLeft[String]("") {
      (state, node) =>
        state + node.createRelationshipStatement
    }.dropRight(1)

    implicit val parser: Reads[InsertResult] = __.read[Int].map { arr =>
      InsertResult(arr)
    }

    val result = connection.makeRequest[InsertResult](s"create $createNodeSec, $createRelationSec return id(n0)").execute

    if (Await.result(result, 10.seconds).asInstanceOf[ListBuffer[InsertResult]].head.id > 0)
      true
    else false
  }

  def close(): Unit = {
    this.nodes = Set()
    this.relationships = Set()
  }
}

private[wrapper] case class InsertResult(id: Int)