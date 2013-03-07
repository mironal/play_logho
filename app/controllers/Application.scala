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

    object OkJson {
      def apply(f: => JsValue): Result = {
        Ok(f).as("application/json; charset=utf-8")
      }
      def apply[T](f : => T)(implicit tjs: Writes[T]): Result = {
        Ok(tjs.writes(f)).as("application/json; charset=utf-8")
      }
    }
    object BadRequestJson {
      def apply(f: => JsValue): Result = {
        BadRequest(f).as("application/json; charset=utf-8")
      }
      def apply[T](f : => T)(implicit tjs: Writes[T]): Result = {
        BadRequest(tjs.writes(f)).as("application/json; charset=utf-8")
      }
    }
  def jsonResponseOrError[T](query: Option[String])(ok: LocalDate => T)(implicit tjs: Writes[T]): Result = {

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

  import scala.util.control.Exception._

  sealed trait Error {
    def msg(): String
  }
  case class InvalidFieldValue(y: Int, m: Int, d: Int) extends Error {
    override def msg(): String = {
      s"Invalid field value $y-$m-$d"
    }
  }

  implicit object ErrorWrites extends Writes[Error] {
    def writes(e: Error): JsValue = {
      Json.obj(
        "msg" -> e.msg()
      )
    }
  }

  def dateToResponse(y: Int, m: Int, d: Int)(dateWithError: InvalidFieldValue => Result, date: LocalDate => Result ): Result = {
    allCatch either new LocalDate(y, m, d) match {
      case Left(e) => e.printStackTrace; dateWithError( InvalidFieldValue(y, m, d))
      case Right(ld) => date(ld)
    }
  }

  def hotentrys(y: Int, m: Int, d:Int) = Action {
    dateToResponse(y, m, d)(
      dateWithError => BadRequestJson( dateWithError),
      date => OkJson(EntrysTimestamp.findHotentrys(date))
    )
  }

  def optionNewentrys(query: Option[String]) = Action {
    jsonResponseOrError(query)( EntrysTimestamp.findNewentrys )
  }

  def newentrys(y: Int, m: Int, d: Int) = Action {
    dateToResponse(y, m, d)(
      dateWithError => BadRequestJson( dateWithError),
      date => OkJson(EntrysTimestamp.findNewentrys(date))
    )
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
