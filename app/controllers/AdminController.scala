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

package controllers

import config.FrontendAppConfig
import models.InternalTable
import models.InternalTable.{SimpleDataSource, TblHead}
import navigation.PageNavigation
import play.api.Logging
import play.api.libs.json.{JsNumber, JsObject, JsString}
import play.api.mvc._
import play.twirl.api.Html
import uk.gov.hmrc.internalauth.client.FrontendAuthComponents
import utils.ActionUtil.ActionBuilderPlus
import utils.PseudoDataSource
import views.html.{IndexPage, SubmissionPage, SubmissionsPage}

import java.lang.Math.abs
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future.successful
import scala.concurrent.{ExecutionContext, Future, Promise}

@Singleton
class AdminController @Inject()(
   messagesControllerComponents: MessagesControllerComponents,
   appConfig: FrontendAppConfig,
   auth: FrontendAuthComponents,
   indexPage: IndexPage,
   submissionPage: SubmissionPage,
   submissionsPage: SubmissionsPage
)(implicit ec: ExecutionContext)
  extends InternalFrontendController(messagesControllerComponents, appConfig, auth) with MessagesBaseController with Logging {
  import appConfig._

  private def dataSourceFactory[T<:Product](table: InternalTable[T]): SimpleDataSource[T] = new SimpleDataSource(table)(()=> PseudoDataSource.submissions.value.map{_.as[JsObject]}.toSeq)

  def index[A](): EssentialAction = canonicallyAuthorised().asIndex.async { implicit request =>
    val navigation: PageNavigation = service.index
    successful(Ok(indexPage(navigation)))
  }

  def submissions[A](sort: Seq[String], filter: Seq[String], page: Option[Int]): EssentialAction = canonicallyAuthorised().asIndex.async { implicit request =>
    // TODO: persist filter display
    // TODO: fix paginator
    // TODO: add penalty appeal reference # to the orchestrator database
    val route = routes.AdminController.index()
    val navigation: PageNavigation = service.child("Home", route) called "Submission Log"
    val referenceField = TblHead("Reference", _.\("reference").asOpt[String], markup = {ref: String => Html(s"<a href=$ref class=govuk-link--no-visited-state>$ref</a>")})
    //val linkField = TblHead("foo\uD83D\uDD17", _.\("reference").asOpt[String], markup = {ref: String => Html(s"<a href=$ref>&#x2197;</a>")})
    val penaltyIdField = TblHead("Penalty ID", { js => (js \ "notification" \ "file" \ "properties" \ 2 \ "value").asOpt[String] })
    val caseeIdField = TblHead("Case ID", { js => (js \ "notification" \ "file" \ "properties" \ 0 \ "value").asOpt[String] })
    val statusField = TblHead("Status", _.\("status").asOpt[String])
    val numberOfAttemnptsField = TblHead("Attempt #", _.\("numberOfAttempts").asOpt[Int])
    val createdAtField = TblHead("Created At", _.\("createdAt").asOpt[String], format = {ref: String => ref.replaceAll("[TZ]", " ").dropRight(4)})
    //val updatedAtField = TblHead("Updated At", _.\("updatedAt").asOpt[String], format = {ref: String => ref.replaceAll("[TZ]", " ").dropRight(4)})
    //val nextAttemptAtField = TblHead("Next Attempt At", _.\("nextAttemptAt").asOpt[String], format = {ref: String => ref.replaceAll("[TZ]", " ").dropRight(4)})
    val demoDataSource = dataSourceFactory(InternalTable(
      (referenceField, penaltyIdField, caseeIdField, statusField, numberOfAttemnptsField, createdAtField)
    ))
    demoDataSource.pageData(filter, sort, page.getOrElse(0)) map { data =>
      Ok(submissionsPage(navigation, data))
    }
  }

  def submission(reference: String): Action[AnyContent] = canonicallyAuthorised().async { implicit request =>
    val route = routes.AdminController.index()
    val route2 = routes.AdminController.submissions(Seq(), Seq(), None)
    val navigation: PageNavigation = appConfig.service.child("Home", route).child("Submission Log", route2) called s"Submission $reference"
    val demoDataSource = dataSourceFactory(InternalTable((
      TblHead("Reference", _.\("reference").asOpt[String]),
      TblHead("Uploaded At", { js => (js \ "notification" \ "file" \ "properties" \ 1 \ "value").asOpt[String] }),
      TblHead("Enrolment Key", { js => (js \ "notification" \ "file" \ "properties" \ 3 \ "value").asOpt[String] }),
      TblHead("Penalty Number", { js => (js \ "notification" \ "file" \ "properties" \ 2 \ "value").asOpt[String] }),
      TblHead("Case ID", { js => (js \ "notification" \ "file" \ "properties" \ 0 \ "value").asOpt[String] }),
      TblHead("Created At", _.\("createdAt").asOpt[String], format = {ref: String => ref.replaceAll("[TZ]", " ").dropRight(4)}),
      TblHead("Status", _.\("status").asOpt[String]),
      TblHead("Attempt #", _.\("numberOfAttempts").asOpt[Int]),
      TblHead("Updated At", _.\("updatedAt").asOpt[String], format = {ref: String => ref.replaceAll("[TZ]", " ").dropRight(4)}),
      TblHead("Next Attempt At", _.\("nextAttemptAt").asOpt[String], format = {ref: String => ref.replaceAll("[TZ]", " ").dropRight(4)}),
      TblHead("File name", { js => (js \ "notification" \ "file" \ "name").asOpt[String] }),
      TblHead("File size", { js => (js \ "notification" \ "file" \ "size").asOpt[Int] }, format = {v: Int=> f"$v%,1.0f" }, markup = {ref: String => Html(s"$ref bytes")}),
      TblHead("Correlation ID", { js => (js \ "notification" \ "audit" \ "correlationID").asOpt[String] })
    )))
    demoDataSource.find(reference) map {
      case Some(data: JsObject) =>
        Ok(submissionPage(navigation, reference, demoDataSource.table.headers.map(x=>(Html(x.name),x.html(data)))))
      case None =>
        NotFound
    }
  }

  def checkFileUrl(reference: String): Action[AnyContent] = Action.async { //implicit request =>
    val randomResponse = {
      abs(reference.hashCode) % 8 match {
        case 0 => Ok(JsObject(Seq("status" -> JsNumber(200), "statusText" -> JsString("Ok"), "type" -> JsString("application/msword"), "size" -> JsNumber(256789))))
        case 1 => Ok(JsObject(Seq("status" -> JsNumber(400), "statusText" -> JsString("Bad Request"))))
        case 2 => Ok(JsObject(Seq("status" -> JsNumber(401), "statusText" -> JsString("Unauthorised"))))
        case 3 => Ok(JsObject(Seq("status" -> JsNumber(403), "statusText" -> JsString("Forbidden"))))
        case 4 => Ok(JsObject(Seq("status" -> JsNumber(404), "statusText" -> JsString("Not Found"))))
        case 5 => Ok(JsObject(Seq("status" -> JsNumber(500), "statusText" -> JsString("Internal Server Error"))))
        case 6 => Ok(JsObject(Seq("status" -> JsNumber(503), "statusText" -> JsString("Service Unavailable"))))
        case 7 => RequestTimeout
      }
    }
    dataSourceFactory(
      InternalTable(Tuple1(TblHead("Reference", _.\("reference").asOpt[String])))
    ).find(reference) flatMap {
      case Some(_: JsObject) =>
        val promise: Promise[Result] = Promise()
        new Thread() {
          override def run(): Unit = {
            Thread.sleep(200 + abs(reference.hashCode) % 300)
            promise.success(randomResponse)
          }
        }.start()
        promise.future
      case None =>
        Future.successful(NotFound)
    }
  }

}