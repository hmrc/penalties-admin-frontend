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

package navigation

import play.api.mvc.Call
import uk.gov.hmrc.govukfrontend.views.viewmodels.breadcrumbs.BreadcrumbsItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text

case class PageService(serviceName: String, baseUrl: String) {
  def child(pageTitle: String, href: Call): PageNavigationPath = {
    PageNavigationPath(this, Seq((pageTitle,href)))
  }
  def index: PageNavigation = PageNavigation(PageNavigationPath(this, Seq.empty), serviceName)
  def called(pageTitle: String): PageNavigation = PageNavigation(PageNavigationPath(this, Seq.empty), pageTitle)
}

case class PageNavigationPath (
  pageService: PageService,
  breadcrumbs: Seq[(String,Call)]
) {
  def child(pageTitle: String, href: Call): PageNavigationPath = {
    this.copy(pageService = pageService, breadcrumbs = this.breadcrumbs :+ (pageTitle,href))
  }
  def called(pageTitle: String): PageNavigation = PageNavigation(this, pageTitle)
}

case class PageNavigation (
  path: PageNavigationPath,
  pageTitle: String
) {
  def serviceName = path.pageService.serviceName
  def breadcrumbTrail()/*(implicit request: RequestHeader)*/: Seq[BreadcrumbsItem] = {
    path.breadcrumbs.map{bc=>BreadcrumbsItem(content = Text(bc._1), href = Some(bc._2.url))}
  }
}
