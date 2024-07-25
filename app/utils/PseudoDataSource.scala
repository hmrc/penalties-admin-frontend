/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package utils

import play.api.libs.json.{JsArray, Json}

import java.time.Instant
import java.time.Instant.ofEpochMilli
import java.time.format.DateTimeFormatterBuilder
import scala.util.Random

object PseudoDataSource {
  private val random = new Random(0)

  private def from(chars: String): LazyList[Char] = LazyList continually (chars.charAt(random nextInt chars.length))

  private def hex: LazyList[Char] = from("abcdef0123456789")
  private def numeric: LazyList[Char] = from("0123456789")
  private def alpha: LazyList[Char] = from("abcdefghijklmnopqrstuvqwxyz")

  private def reference: LazyList[String] = LazyList continually Seq(8,4,4,4,12).map(hex.take(_).mkString).mkString("-")

  private def nino: LazyList[String] = LazyList continually Seq(from("ABCEGHJKLMNOPRSTWXYZ").take(1).mkString, from("ABCEGHJKLMNPRSTWXYZ").take(1).mkString, numeric.take(6).mkString, from("ABCD").take(1).mkString).mkString

  private def appealId: LazyList[String] = LazyList continually Seq("PR-", numeric.take(9).mkString).mkString

  private def penaltyId: LazyList[String] = LazyList continually Seq("X",alpha.take(1).mkString, numeric.take(12).mkString).mkString.toUpperCase

  val statii = Seq(
    "PENDING",
    "SENT",
    "FILE_RECEIVED_IN_SDES",
    "FILE_NOT_RECEIVED_IN_SDES_PENDING_RETRY",
    "FILE_PROCESSED_IN_SDES",
    "FAILED_PENDING_RETRY",
    "NOT_PROCESSED_PENDING_RETRY",
    "PERMANENT_FAILURE"
  )

  private def status: LazyList[String] = LazyList continually statii(random nextInt statii.length)

  private val minDate = Instant.parse("2024-03-01T00:00:00Z")
  private val maxDate = Instant.parse("2024-06-01T00:00:00Z")
  private def dateTime: LazyList[Instant] = LazyList continually ofEpochMilli(random.between(minDate.toEpochMilli, maxDate.toEpochMilli))

  val jsonDateFormatter = new DateTimeFormatterBuilder().parseCaseInsensitive().appendInstant(0).toFormatter

  private[utils] def submission = {
    def jsonFormat(instant: Instant) = s""""${jsonDateFormatter.format(instant)}""""
    def mongoFormat(instant: Instant) = s""" { "$$date": { "$$numberLong": "${instant.toEpochMilli}" } } """
    val createdAt = dateTime.head
    val updatedAt = createdAt.plusSeconds(random.between(1, 3600))
    val nextAttemptAt = if (random.nextBoolean) maxDate.plusSeconds(random.nextInt(180)) else updatedAt.plusSeconds(180)
    val uploadedAt = dateTime.head.minusSeconds(random.between(60, 600))
    s"""{
       |  "reference": "${reference.head}",
       |  "status": "${status.head}",
       |  "numberOfAttempts": ${random.nextInt(100)},
       |  "createdAt": ${mongoFormat(createdAt)},
       |  "updatedAt": ${mongoFormat(updatedAt)},
       |  "nextAttemptAt": ${mongoFormat(nextAttemptAt)},
       |  "notification": {
       |    "informationType": "foo",
       |    "file": {
       |      "recipientOrSender": "recipient1",
       |      "name": "file1.txt",
       |      "location": "http://example.com/file1.txt",
       |      "checksum": {
       |        "algorithm": "SHA-256",
       |        "value": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
       |      },
       |      "size": ${random.nextInt(100000)},
       |      "properties": [
       |        { "name": "CaseId", "value": "${appealId.head}" },
       |        { "name": "SourceFileUploadDate", "value": ${jsonFormat(uploadedAt)} },
       |        { "name": "PenaltyNumber", "value": "${penaltyId.head}" },
       |        { "name": "EnrolmentKey", "value": "HMRC-PT~NINO~${nino.head}" },
       |        { "name": "FileMimeType", "value": "text/plain;charset=UTF-8" }
       |      ]
       |    },
       |    "audit": {
       |      "correlationID": "${reference.head}"
       |    }
       |  }
       |}""".stripMargin
  }

  lazy val submissions: JsArray = JsArray((1 to 100).map { _ =>
    Json.parse(submission)
  })
}
