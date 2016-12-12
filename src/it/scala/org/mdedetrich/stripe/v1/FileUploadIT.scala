package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.FileUploads.{FileUpload, Purpose}

class FileUploadIT extends IntegrationTest{

  "FileUpload" should {
    "upload jpg to Stripe" in {

      val is = getClass.getResourceAsStream("/id-card.jpg")
      val uploadF = handle(FileUploads.upload(Purpose.IdentityDocument, s"file-upload-${OffsetDateTime.now}.jpg", is))

      uploadF.map { u =>
        u shouldBe a[FileUpload]
      }
    }
  }

}
