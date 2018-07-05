package ch.timo_schmid.sbt.dockerRun

import play.api.libs.json._

class JsValueOps(val jsValue: JsValue) {

  def asArray: Array[JsValue] =
    jsValue.as[JsArray].value.toArray

  def asString: String =
    jsValue.as[JsString].value

  def field(field: String): JsValue =
    jsValue.as[JsObject].value(field)

}