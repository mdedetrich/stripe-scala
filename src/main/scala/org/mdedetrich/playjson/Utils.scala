package org.mdedetrich.playjson

import play.api.libs.json._

object Utils {

  implicit class JsPathExtensionMethods(self: JsPath) {

    /**
      * Reads a Option[T] search optional or nullable field at JsPath (field not found or null is None
      * and other cases are Error).
      *
      * It runs through JsValue following all JsPath nodes on JsValue except last node:
      * - If one node in JsPath is not found before last node => returns JsError( "missing-path" )
      * - If all nodes are found till last node, it runs through JsValue with last node =>
      * - If last node if not found => returns None
      * - If last node is found with value "null" => returns None
      * - If last node is found as a Object value that is 
      * empty OR last node is not a Object => returns None
      * - If last node is found => applies implicit Reads[T]
      */
    def readNullableOrEmptyJsObject[T](implicit r: Reads[T]): Reads[Option[T]] = {
      self.readNullable[JsValue].map {
        case Some(jsValue) =>
          jsValue match {
            case jsObject: JsObject =>
              if (jsObject.fields.isEmpty)
                None
              else
                Option(jsObject)
            case _ => None
          }
        case None => None
      }.flatMap { _ =>
        Reads.nullable[T](self)(r)
      }
    }

    /**
      * Reads a Option[T] search optional or nullable field at JsPath (field not found or null is None
      * and other cases are Error).
      *
      * It runs through JsValue following all JsPath nodes on JsValue except last node:
      * - If one node in JsPath is not found before last node => returns JsError( "missing-path" )
      * - If all nodes are found till last node, it runs through JsValue with last node =>
      * - If last node if not found => returns None
      * - If last node is found with value "null" => returns None
      * - If last node is found as a Array value that is 
      * empty OR last node is not a Array => returns None
      * - If last node is found => applies implicit Reads[T]
      */
    def readNullableOrEmptyJsArray[T](implicit r: Reads[T]): Reads[Option[T]] = {
      self.readNullable[JsValue].map {
        case Some(jsValue) =>
          jsValue match {
            case jsArray: JsArray =>
              if (jsArray.value.isEmpty)
                None
              else
                Option(jsArray)
            case _ => None
          }
        case None => None
      }.flatMap { _ =>
        Reads.nullable[T](self)(r)
      }
    }
  }

}
