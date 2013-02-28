package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import com.github.nscala_time.time.Imports._

import java.util.Date

case class Timestamp(id: Long, tweet: String, created: Date)

object Timestamp {

    val simple = {
        get[Long]("id") ~
        get[String]("tweet") ~
        get[Date]("created") map {
            case id~tweet~created => Timestamp(id, tweet, created)
        }
    }

    def findEntryByDate(entryType: String)(date: LocalDate) = findEntrys(entryType, date)

    def findHotentrysByDate(date: LocalDate): List[Timestamp] =  findEntrys("hot", date)
    def findNewentrysByDate(date: LocalDate): List[Timestamp] = findEntrys("new", date)

    private def findEntrys(entryType:String, date: LocalDate): List[Timestamp] = {
      DB.withConnection{ implicit c =>
      SQL("select id, tweet, created from timestamps where date(created)={date} and type={type}")
      .on("date" -> date.toString, "type" -> entryType)
      .as(Timestamp.simple *)
    }
  }

}
