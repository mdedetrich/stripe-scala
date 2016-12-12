package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.v1.FileUploads.FileUpload
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class FileUploadSpec extends WordSpec with Matchers {
  "File Upload" should {
    "parse JSON correctly" in {
      val in   = this.getClass.getResourceAsStream("/file-upload.json")
      val json = Json.parse(in)

      val JsSuccess(fileUpload, _) = json.validate[FileUpload]
      fileUpload.id should be("file_19211GJ6y4jvjvHhV7CGDHse")
    }
  }
}
