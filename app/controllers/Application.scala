package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import com.github.nscala_time.time.Imports._

import java.util.Date

import models._

import LoghoWrites._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def queryHandler(query: Option[String])(ok:LocalDate => Result, ng: String => Result ): Result = {
    def toDate(q: Option[String]):Option[LocalDate] = {
      //変なフォーマットのqueryが来たらNoneを返す.
      def toDate(query: String): Option[LocalDate] = {
        lazy val date = (y:String, m:String, d:String) => Some(new LocalDate(y.toInt, m.toInt, d.toInt))
        lazy val slashDelimited = """(\d{4})/(\d{1,2})/(\d{1,2})""".r
        lazy val hyphenDelimited = """(\d{4})-(\d{1,2})-(\d{1,2})""".r
        query match {
          case slashDelimited(y, m, d) => date(y, m, d)
          case hyphenDelimited(y, m, d) => date(y, m, d)
          case x =>  None
        }
      }
      // queryの指定が無ければ今日の日付
      lazy val today = Some(new LocalDate())
      q.map(toDate).getOrElse(today)
    }
    toDate(query) match {
      case None => ng(query.getOrElse("empty"))
      case Some(date) => ok(date)
    }
  }

  def jsonResponseOrError[T](query: Option[String])(ok: LocalDate => T)(implicit tjs: Writes[T]): Result = {
    object OkJson {
      def apply(f: => JsValue): Result = {
        Ok(f).as("application/json; charset=utf-8")
      }
    }
    object BadRequestJson {
      def apply(f: => JsValue): Result = {
        BadRequest(f).as("application/json; charset=utf-8")
      }
    }

    def errorJson(query: String): JsValue = {
      val msg = "Invalid query => " + query
      Json.toJson(Map("msg" -> msg))
    }

    queryHandler(query)(
      date => OkJson( tjs.writes( ok(date) ) ),
      s => BadRequestJson(errorJson(s))
    )
  }

  def optionHotentrys(query: Option[String]) = Action {
    jsonResponseOrError(query)( EntrysTimestamp.findHotentrys )
  }

  def hotentrys(y: Int, m: Int, d:Int) = {
    optionHotentrys(Some("" + y + "-" + m + "-" + d))
  }

  def optionNewentrys(query: Option[String]) = Action {
    jsonResponseOrError(query)( EntrysTimestamp.findNewentrys )
  }

  def newentrys(y: Int, m: Int, d: Int) = {
    optionNewentrys(Some("" + y + "-" + m + "-" + d))
  }

  def allentrys(dir: String) = {
    optionAllentrys(Some(dir))
  }


  def optionAllentrys(query: Option[String]) = Action {
    jsonResponseOrError(query){ date =>
    val tss = Timestamp.findAllByDate(date)
    tss.isEmpty match {
      case true => Json.toJson(List[Entry]())
      case false => {
        val startId = tss.head.id
        val endId = tss.last.id
        Json.toJson( Entry.findByTimestampIdBetween(startId, endId) )
      }
    }
    }
  }
}
