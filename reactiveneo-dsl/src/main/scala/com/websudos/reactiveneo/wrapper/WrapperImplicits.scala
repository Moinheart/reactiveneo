package com.websudos.reactiveneo.wrapper

import scala.language.implicitConversions

/**
  * trait for implicits
  * used by Neo4j wrapper
  */
trait WrapperImplicits {
  implicit def node2relationshipBuilder(node: Node): NodeRelationshipMethods = NodeRelationshipMethods(node)

  implicit def string2RelationshipType(relType: String): RelationshipType = RelationshipType(relType)

  implicit def propertyContainer2RichPropertyContainer(propertyContainer: PropertyContainer): RichPropertyContainer = new RichPropertyContainer(propertyContainer)

  implicit class NodeToCaseClass(pc: PropertyContainer) {
    def toCC[T: Manifest]: Option[T] = Wrapper.toCC[T](pc)
  }
}
