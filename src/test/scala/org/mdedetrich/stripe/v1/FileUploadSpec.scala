package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.v1.FileUploads.FileUpload
import org.scalatest.{Matchers, WordSpec}

class FileUploadSpec extends WordSpec with Matchers {
  "File Upload" should {
    "parse JSON correctly" in {
      val in = this.getClass.getResourceAsStream("/file-upload.json")

      val string     = scala.io.Source.fromInputStream(in).mkString
      val json       = parse(string).toOption
      val fileUpload = json.flatMap(_.as[FileUpload].toOption).get

      fileUpload.id should be("file_19211GJ6y4jvjvHhV7CGDHse")
    }
  }
}
