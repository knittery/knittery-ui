import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "send 404 on a bad request" in new WithApplication {
      route(FakeRequest(GET, "/boum")) must beNone
    }

    "render the index page" in new WithApplication {
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/html")
    }

    "render the serial-simulator page" in new WithApplication {
      val page = route(FakeRequest(GET, "/simulator")).get

      status(page) must equalTo(OK)
      contentType(page) must beSome.which(_ == "text/html")
    }

    "render the display page" in new WithApplication {
      val page = route(FakeRequest(GET, "/display")).get

      status(page) must equalTo(OK)
      contentType(page) must beSome.which(_ == "text/html")
    }
  }
}
