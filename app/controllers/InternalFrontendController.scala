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
import play.api.Logging
import play.api.mvc._
import uk.gov.hmrc.internalauth.client._
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
abstract class InternalFrontendController @Inject()(
                                 messagesControllerComponents: MessagesControllerComponents,
                                 appConfig: FrontendAppConfig,
                                 auth: FrontendAuthComponents,
                               )(implicit ec: ExecutionContext)
  extends FrontendController(messagesControllerComponents) with MessagesBaseController with Logging {

  protected val read = IAAction("READ")

//  protected def authorised(location: String, action: IAAction)/*(implicit request: Request[_])*/: ActionBuilder[MessagesRequest, AnyContent] = {
//    messagesControllerComponents.messagesActionBuilder.compose(
//      auth.authorizedAction(
//        continueUrl = routes.AdminController.index(Seq.empty, Seq.empty, None),
//        predicate = Permission(
//          Resource(
//            ResourceType(appName),
//            ResourceLocation(location)
//          ),
//          action
//        ),
//        retrieval = Retrieval.username
//      )
//    )
//    Action
//  }

  protected def canonicallyAuthorised()/*(implicit ec: ExecutionContext)*/: ActionBuilder[MessagesRequest, AnyContent] = {
    val underlyingAction = Action
    new ActionBuilder[MessagesRequest, AnyContent]() {
      override def parser: BodyParser[AnyContent] = underlyingAction.parser
      override def invokeBlock[A](request: Request[A], block: MessagesRequest[A] => Future[Result]): Future[Result] = {
        underlyingAction.invokeBlock(request, block)
      }
      override protected def executionContext: ExecutionContext = ec
    }
  }

}