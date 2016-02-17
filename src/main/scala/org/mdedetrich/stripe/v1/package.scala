package org.mdedetrich.stripe

/**
  * Created by matthewdedetrich on 16/02/2016.
  */
package object v1 {

  def mapToPostParams(optionalMap: Option[Map[String, String]], parentKey: String) = {
    optionalMap match {
      case Some(map) =>
        map.map { case (key, value) =>
          s"$parentKey[$key]" -> value
        }
      case None =>
        Map.empty
    }
  }

}
