package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import org.mdedetrich.stripe.v1.FileUploads.{FileUpload, Purpose}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.JsValue

class FileUploadIT extends WordSpec with Matchers with ScalaFutures {

  implicit val defaultPatience = PatienceConfig(timeout = Span(10, Seconds), interval = Span(5, Millis))

  "Accounts" should {
    "upload jpg to Stripe" in {

      import org.mdedetrich.stripe.Config._
      import scala.concurrent.ExecutionContext.Implicits.global

      val is = this.getClass.getResourceAsStream("/id-card.jpg")
      val uploadF = handle(FileUploads.upload(Purpose.IdentityDocument, s"file-upload-${OffsetDateTime.now}.jpg", is))

      whenReady(uploadF) { u =>
        u shouldBe a[FileUpload]
      }
    }
  }

}
