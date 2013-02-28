package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import com.github.nscala_time.time.Imports._

import java.util.Date

import models._

object Application extends Controller {

  case class EntrysTimestamp(tweet: String, timestamp: Date, entrys: List[Entry])


  // EntryからJsonへの変換方法の定義
  implicit val entryWrites = new Writes[Entry] {
    def writes(e: Entry): JsValue = {
      Json.obj(
        "title" -> e.title,
        "url" -> e.url,
        "cnt" -> e.cnt
      )
    }
  }

  // EntrysTimestampからJsonへの変換方法の定義
  implicit val entrysTimestampWrites = new Writes[EntrysTimestamp] {
    import java.text.SimpleDateFormat
    // timestampのフォーマット
    def format(date: Date) = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date)
    def writes(ts: EntrysTimestamp): JsValue = {
      Json.obj(
        "tweet" -> ts.tweet,
        "timestamp" -> format(ts.timestamp),
        "entrys" -> Json.toJson(ts.entrys)
      )
    }
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


  lazy val returnHotentryJson = returnEntryJson("hot")_
  lazy val returnNewentryJson = returnEntryJson("new")_

  def hotentrys(dir: String) = {
    optionHotentrys(Some(dir))
  }


  def optionHotentrys(query: Option[String]) = Action{
    toDate(query) match {
      case Some(date) => Ok(returnHotentryJson(date)).as("application/json; charset=utf-8")
      case None => BadRequest(errorJson(query)).as("application/json; charset=utf-8")
    }
  }

  def newentrys(dir: String) = {
    optionNewentrys(Some(dir))
  }

  def optionNewentrys(query: Option[String]) = Action{
    toDate(query) match {
      case Some(date) => Ok(returnNewentryJson(date)).as("application/json; charset=utf-8")
      case None => BadRequest(errorJson(query)).as("application/json; charset=utf-8")
    }
  }

  def allentrys(dir: String) = {
    optionAllentrys(Some(dir))
  }

  def optionAllentrys(query: Option[String]) = {
    TODO
  }

  private def errorJson(query: Option[String]) = {
    val msg = "Invalid query => " + query.getOrElse("empty")
    Json.toJson(Map("msg" -> msg))
  }

  private def timestamps[A](entryType: String, date: LocalDate)( f: List[Timestamp] => A) = {
    entryType match {
      case "hot" => f(Timestamp.findHotentrysByDate(date))
      case "new" => f(Timestamp.findNewentrysByDate(date))
      case x => throw new IllegalStateException(x)
    }
  }

  private def returnEntryJson(entryType: String)(date: LocalDate): JsValue = {
    val results = timestamps(entryType, date){ ets =>
      ets.map{ et => EntrysTimestamp(et.tweet, et.created, Entry.findByTimestampId(et.id))}
    }
    Json.toJson(results)
  }

  private def toDate(q: Option[String]):Option[LocalDate] = {
    def toDate(query: String): Option[LocalDate] = {
      lazy val date = (y:String, m:String, d:String) => Some(new LocalDate(y.toInt, m.toInt, d.toInt))
      lazy val slashDelimited = """(\d{4})/(\d{1,2})/(\d{1,2})""".r
      lazy val hyphenDelimited = """(\d{4})-(\d{1,2})-(\d{1,2})""".r
      query match {
        case slashDelimited(y, m, d) => date(y, m, d)
        case hyphenDelimited(y, m, d) => date(y, m, d)
        case x => {
          println(x)
          None
        }
      }
    }
    lazy val today = Some(new LocalDate())
    q match {
      case None => today
      case Some(str) => toDate(str)
    }
  }
}
