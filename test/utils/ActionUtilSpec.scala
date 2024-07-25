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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.ActionUtil.addTrailingSlash

class ActionUtilSpec extends AnyWordSpec with Matchers {

  "addTrailingSlash" should {
    "add a slash if not present in path" in {
      addTrailingSlash("http://x.y/foo") shouldBe "http://x.y/foo/"
      addTrailingSlash("/foo") shouldBe "/foo/"
    }

    "don't change the url if already present in path" in {
      addTrailingSlash("http://x.y/foo/") shouldBe "http://x.y/foo/"
    }

    "preseve parameters and fragment when adding trailing slash" in {
      addTrailingSlash("http://x.y/foo?a=b&b=c") shouldBe "http://x.y/foo/?a=b&b=c"
      addTrailingSlash("http://x.y/foo#bar") shouldBe "http://x.y/foo/#bar"
      addTrailingSlash("http://x.y/foo?a=b&b=c#bar") shouldBe "http://x.y/foo/?a=b&b=c#bar"
    }

    "preseve parameters and fragment when not adding trailing slash" in {
      addTrailingSlash("http://x.y/foo/?a=b&b=c") shouldBe "http://x.y/foo/?a=b&b=c"
      addTrailingSlash("http://x.y/foo/#bar") shouldBe "http://x.y/foo/#bar"
      addTrailingSlash("http://x.y/foo/?a=b&b=c#bar") shouldBe "http://x.y/foo/?a=b&b=c#bar"
    }

    "ignore trailing slash on parameters or fragment when determining if slash should be added" in {
      addTrailingSlash("http://x.y/foo?a=b&b=c/") shouldBe "http://x.y/foo/?a=b&b=c/"
      addTrailingSlash("http://x.y/foo#bar/") shouldBe "http://x.y/foo/#bar/"
      addTrailingSlash("http://x.y/foo?a=b&b=c#bar/") shouldBe "http://x.y/foo/?a=b&b=c#bar/"
    }
  }

}