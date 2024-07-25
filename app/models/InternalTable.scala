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

package models

import models.InternalTable.TblHead
import play.api.libs.json.JsObject
import play.twirl.api.Html

import java.lang.Math.min
import java.net.URLDecoder
import scala.concurrent.Future
import scala.concurrent.Future.successful
import scala.reflect.ClassTag

object InternalTable {
  case class TblHead[T: ClassTag](
    name: String,
    lookup: JsObject => Option[T],
    markup: String => Html = Html(_),
    format: T => String = { value:T => value.toString }
  ) {
    val symbol: String = name.toLowerCase.replaceAll("[^a-z]+","-").stripSuffix("-")

    final def text(js: JsObject): Option[String] = lookup(js).map(format)

    final def html(js: JsObject): Html = markup(text(js).getOrElse(""))

    override def toString: String = name

    val comparator: (JsObject, JsObject)=>Int = { case (l: JsObject, r: JsObject) =>
      (lookup(l), lookup(r)) match {
        case (Some(lV: Comparable[T] @unchecked), Some(rV: T)) => lV.compareTo(rV)
        case (Some(lV), Some(rV)) => lV.toString.compareTo(rV.toString) // fallback
        case (Some(_), None) => 1
        case (None, Some(_)) => -1
        case (None, None) => 0
      }
    }

    val matcher: (JsObject, String)=>Boolean = { case (js: JsObject, needle: String) =>
      lookup(js) match {
        case Some(v) => v.toString.contains(needle)
        case None => false
      }
    }
  }

  case class SortSpec(field: TblHead[_], descending: Boolean)

  sealed trait Operation { val symbol: String; }
  object Equals extends Operation{ override val symbol = "="; }
  object Contains extends Operation{ override val symbol = "~"; }

  case class FilterSpec(field: TblHead[_], operation: Operation, needle: String)

  trait TableData {
    val filters: Seq[FilterSpec]
    val numPages: Int
    val pageNumber: Int
    val rows: Seq[JsObject]
    val htmlHeader: Seq[String]
    val htmlBody: Seq[Seq[Html]]
  }

  trait DataSource[T<:Product] {
    val table: InternalTable[T]

    case class Data[U<:T](filters: Seq[FilterSpec], numPages: Int, pageNumber: Int, val rows: Seq[JsObject]) extends TableData {
      lazy val htmlHeader: Seq[String] = table.headers.map(_.name)
      lazy val htmlBody: Seq[Seq[Html]] = rows.map{ table.format }
    }

    private def parseSortParam(spec: String): SortSpec  = {
        spec.split("-", 2) match {
          case Array(direction, fieldName) =>
            val hdr = table.headers.find(_.symbol == fieldName)
            (direction, hdr) match {
              case ("asc", Some(header)) => SortSpec(header, descending = false)
              case ("desc", Some(header)) => SortSpec(header, descending = true)
              case (_, None) => throw new Exception(s"""Bad column name: $fieldName""");
              case (dir, _) => throw new Exception(s"""Bad direction: $dir""");
            }
          case x => throw new Exception(s"""$spec became ${x.mkString(" ")}""");
        }
    }

    private def parseFilterParam(spec: String): FilterSpec = {
      val decoded = URLDecoder.decode(spec, "UTF8")
      decoded.split("=", 2) match {
        case Array(fieldName, filterValue) =>
          table.headers.find(_.symbol == fieldName) match {
            case Some(header) => FilterSpec(header, Equals, filterValue)
            case None => throw new Exception(s"""Bad column name in filter spec: $decoded""");
          }
        case x => throw new Exception(s"""$spec becamse ${x.mkString(" ")}""")
      }
    }

    final def pageData(filter: Seq[String], sort: Seq[String], page: Int, pageSize: Int = 20): Future[Data[T]] = {
      val sortSpecs = sort.map { parseSortParam }
      val filters = filter.map { parseFilterParam }
      fetch(filters, sortSpecs, page, pageSize)
    }

    def fetch(filter: Seq[FilterSpec], sort: Seq[SortSpec], page: Int, pageSize: Int = 20): Future[Data[T]]
    def find(hdr: TblHead[_], fieldValue: String): Future[Option[JsObject]]

    final def find(fieldName: String, fieldValue: String): Future[Option[JsObject]] = {
      table.headers.find(_.symbol == fieldName) match {
        case Some(hdr: TblHead[_]) => find(hdr, fieldValue)
        case None => throw new Exception(s"No such field: $fieldName")
      }
    }

    final def find(fieldValue: String): Future[Option[JsObject]] = {
      find(table.primaryKey, fieldValue)
    }
  }

  class SimpleDataSource[T<:Product](val table: InternalTable[T])(val factory: () => Seq[JsObject]) extends DataSource[T] {
    def fetch(filter: Seq[FilterSpec], sortSpecs: Seq[SortSpec], page: Int, pageSize: Int = 20): Future[Data[T]] = {
      val comparators: Seq[(JsObject,JsObject) => Int] = sortSpecs.map {
        case spec if spec.descending => {  case (l,r) => -spec.field.comparator(l,r) }
        case spec                    => {  case (l,r) => spec.field.comparator(l,r) }
      }

      def gestaltComparator(l: JsObject, r: JsObject): Boolean = {
        val startOpt: Option[Int] = None
        comparators.foldLeft(startOpt){ case (a,f) => a.orElse(Some(f(l,r)).filterNot(_==0)) }.getOrElse(0) > 0
      }

      val matchers: Seq[JsObject => Boolean] = filter.map {
        case spec if spec.operation == Equals => { x: JsObject => spec.field.matcher(x, spec.needle) }
        case spec => throw new Exception(s"Unsupported operation: $spec")
      }
      def gestaltMatcher(js: JsObject): Boolean = matchers.foldLeft(true){ case (a,f) => a && f(js) }

      val rawData = factory()
      val rows = rawData.map(_.as[JsObject]).filter(gestaltMatcher).sortWith(gestaltComparator).grouped(pageSize).drop(page).nextOption().getOrElse(Seq.empty)

      val numPages = rawData.length/pageSize + min(1, rawData.length % pageSize)
      successful(Data(filter, numPages, page, rows))
    }

    def find(hdr: TblHead[_], fieldValue: String): Future[Option[JsObject]] = successful {
      factory().find { case js: JsObject =>
        val v: Option[String] = hdr.text(js)
        v.exists(_.contains(fieldValue))
      }
    }
  }
}

case class InternalTable[T<:Product](header: T) {
  val headers: Seq[TblHead[_]] = header.productIterator.toSeq.asInstanceOf[Seq[TblHead[Any]]]

  /** override this if the first field is not the primary key */
  val primaryKey: TblHead[_] = headers.head

  def format(js: JsObject): Seq[Html] = headers.map(_.html(js))
}