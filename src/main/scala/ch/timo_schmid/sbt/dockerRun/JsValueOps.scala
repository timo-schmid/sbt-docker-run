package ch.timo_schmid.sbt.dockerRun

import play.api.libs.json._

class JsValueOps(val jsValue: JsValue) {

  def field(field: String): JsValue =
    jsValue.as[JsObject].value(field)

}
