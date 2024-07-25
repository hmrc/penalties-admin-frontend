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

import play.api.Logging
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionBuilder, BodyParser, Request, Result}

import java.net.URI
import scala.concurrent.{ExecutionContext, Future}

object ActionUtil extends Logging {
  implicit class ActionBuilderPlus[+R[_], B] (inner: ActionBuilder[R,B]) {
    def asIndex(implicit ec: ExecutionContext): ActionBuilder[R,B] = new ActionBuilder[R,B] {
      override def parser: BodyParser[B] = inner.parser

      override def invokeBlock[A](request: Request[A], block: R[A] => Future[Result]): Future[Result] = {
        if (request.path.endsWith("/")) {
          inner.invokeBlock(request, block)
        } else {
          Future.successful(Redirect(addTrailingSlash(request.uri)))
        }
      }

      override protected def executionContext: ExecutionContext = ec;
    }
  }

  def addTrailingSlash(uri: String): String = {
    val originalURI = URI.create(uri)
    if (originalURI.getRawPath.endsWith("/")) {
      uri
    } else {
      if (originalURI.isAbsolute) {
        val redirectTo = originalURI.getScheme + "://" + originalURI.getRawAuthority + originalURI.getRawPath + "/" +
          (if (Option(originalURI.getRawQuery).nonEmpty) "?" + originalURI.getRawQuery else "") +
          (if (Option(originalURI.getRawFragment).nonEmpty) "#" + originalURI.getRawFragment else "")
        logger.warn(s"### $uri => $redirectTo ###")
        redirectTo
      } else {
        val redirectTo = originalURI.getRawPath + "/" +
          (if (Option(originalURI.getRawQuery).nonEmpty) "?" + originalURI.getRawQuery else "") +
          (if (Option(originalURI.getRawFragment).nonEmpty) "#" + originalURI.getRawFragment else "")
        logger.warn(s"### $uri => $redirectTo ###")
        redirectTo
      }
    }
  }
}
