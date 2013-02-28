package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import com.github.nscala_time.time.Imports._

import java.util.Date
import java.text.SimpleDateFormat

import models._

object Application extends Controller {

  case class EntryTimestamp(tweet: String, timestamp: Date, entrys: List[Entry])

  def format(date: Date) = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date)


  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


  val returnHotentryJson = returnEntryJson("hot")_
  val returnNewentryJson = returnEntryJson("new")_

  def hotentrys(dir: String) = {
    optionHotentrys(Some(dir))
  }

  private def errorJson(query: Option[String]) = {
    val msg = "Invalid query => " + query.getOrElse("empty")
    Json.toJson(Map("msg" -> msg))
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

  private def timestamps[A](entryType: String, date: LocalDate)( f: List[Timestamp] => A) = {
    entryType match {
      case "hot" => f(Timestamp.findHotentrysByDate(date))
      case "new" => f(Timestamp.findNewentrysByDate(date))
      case x => throw new IllegalStateException(x)
    }
  }

  private def returnEntryJson(entryType: String)(date: LocalDate): JsValue = {

    def toEntryTimestamps(tss: List[Timestamp]) =
      tss.map(ts => EntryTimestamp(ts.tweet, ts.created, Entry.findByTimestampId(ts.id)))

    //EntryTimestampのリストをJsValueに変換する
    def resultsToJson(results: List[EntryTimestamp]): JsValue = {
      //EntryTimestampをJsValuに変換する
      def toJson(result: EntryTimestamp): JsValue = {
        Json.toJson(
        Map(
          "tweet" -> Json.toJson(result.tweet),
          "timestamp" -> Json.toJson(format(result.timestamp)),
          "entrys" -> entrysToJson(result.entrys)
        ))
      }

      //EntryのリストをJsValueに変換する
      def entrysToJson(entrys: List[Entry]): JsValue = {
        //EntryをJsValueに変換する
        def toJson(entry: Entry): JsValue = {
          Json.toJson(Map(
            "title" -> Json.toJson(entry.title),
            "url"   -> Json.toJson(entry.url),
            "cnt"   -> Json.toJson(entry.cnt)
          ))
        }
        Json.toJson(entrys.map(toJson))
      }
      Json.toJson(results.map(toJson))
    }

    lazy val today = new LocalDate()
    val results = timestamps(entryType, date)(toEntryTimestamps)
    resultsToJson(results)
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
