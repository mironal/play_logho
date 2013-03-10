package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import com.github.nscala_time.time.Imports._

import java.util.Date

import models._

import LoghoWrites._
import Error._

import scala.util.control.Exception._

object Application extends Controller {

  implicit object ErrorWrites extends Writes[Error] {
    def writes(e: Error): JsValue = {
      Json.obj(
        "msg" -> e.msg()
      )
    }
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
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

  def hotentrys(y: Int, m: Int, d:Int) = Action {
    dateToResponse(y, m, d)(
      dateWithError => BadRequestJson( dateWithError),
      date => OkJson(EntrysTimestamp.findHotentrys(date))
    )
  }

  def hotentrysByQuery(query: Option[String]) = Action {
    splitQuery(query) match {
      case Left(e) => BadRequestJson(e)
      case Right((y, m, d)) =>
        dateToResponse(y, m, d)(
          dateWithError => BadRequestJson( dateWithError),
          date => OkJson(EntrysTimestamp.findHotentrys(date))
        )
    }
  }

  def newentrys(y: Int, m: Int, d: Int) = Action {
    dateToResponse(y, m, d)(
      dateWithError => BadRequestJson( dateWithError),
      date => OkJson(EntrysTimestamp.findNewentrys(date))
    )
  }

  def newentrysByQuery(query: Option[String]) = Action {
    splitQuery(query) match {
      case Left(e) => BadRequestJson(e)
      case Right((y, m, d)) =>
        dateToResponse(y, m, d)(
          dateWithError => BadRequestJson( dateWithError),
          date => OkJson(EntrysTimestamp.findNewentrys(date))
        )
    }
  }

  def allentrys(y: Int, m: Int, d: Int) = Action {
    dateToResponse(y, m, d)(
      dateWithError => BadRequestJson( dateWithError),
      date => OkJson{
        dateToAllentrys(date)
      }
    )
  }

  def allentrysByQuery(query: Option[String]) = Action {
    splitQuery(query) match {
      case Left(e) => BadRequestJson(e)
      case Right((y, m, d)) =>
      dateToResponse(y, m, d)(
        dateWithError => BadRequestJson( dateWithError),
        date => OkJson{
          dateToAllentrys(date)
        }
      )
    }
  }


  private def dateToAllentrys(date: LocalDate): List[Entry] = {
    val tss = Timestamp.findAllByDate(date)
    tss.isEmpty match {
      case true => List[Entry]()
      case false =>
        val startId = tss.head.id
        val endId = tss.last.id
        Entry.findByTimestampIdBetween(startId, endId)
    }
  }

  private def dateToResponse(y: Int, m: Int, d: Int)(dateWithError: InvalidFieldValue => Result, date: LocalDate => Result ): Result = {
    allCatch either new LocalDate(y, m, d) match {
      case Left(e) => e.printStackTrace; dateWithError( InvalidFieldValue(y, m, d)(e))
      case Right(ld) => date(ld)
    }
  }

  private def splitQuery(query: Option[String]): Either[InvalidFormat, (Int, Int, Int)] = {
    lazy val right = (y: String, m: String, d: String) => Right((y.toInt, m.toInt, d.toInt))
      def split(query: String): Either[InvalidFormat, (Int, Int, Int)] = {
        lazy val slashDelimited = """(\d{4})/(\d{1,2})/(\d{1,2})""".r
        lazy val hyphenDelimited = """(\d{4})-(\d{1,2})-(\d{1,2})""".r
        query match {
          case slashDelimited(y, m, d) => right(y, m, d)
          case hyphenDelimited(y, m, d) => right(y, m, d)
          // あとで"で囲むように治す.
          case x =>  Left(InvalidFormat(s"Invalid format: ${x}"))
        }
      }
    //(y, m, d)に変換. errorメッセージを適切に出すために冗長なLocalDateの生成が行われている.
    query.map(split).getOrElse(split(new LocalDate().toString))
  }

}
