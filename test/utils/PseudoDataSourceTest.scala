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

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsDefined, JsString, Json}
import utils.JsPathUtil.JsPathEx

class PseudoDataSourceTest extends AnyFunSuiteLike with Matchers {

  private val aSubmission = Json.parse(PseudoDataSource.submission)

  test("testEnrolmentKey") {
    val result = (aSubmission \ "notification" \ "file" \ "properties").find(_.\("name")===JsDefined(JsString("EnrolmentKey"))) \ 0 \ "value"
    result shouldBe JsDefined(JsString("HMRC-PT~NINO~YX882716D"))
  }

  test("testCaseId") {
    val result = (aSubmission \ "notification" \ "file" \ "properties").find(_.\("name")===JsDefined(JsString("CaseId"))) \ 0 \ "value"
    result shouldBe JsDefined(JsString("PR-146229766"))
  }

  //println(Json.prettyPrint(PseudoDataSource.submissions))
}
