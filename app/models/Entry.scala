package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import com.github.nscala_time.time.Imports._


case class Entry(title: String, url: String, cnt: Int)

object Entry {

  val simple = {
      get[String]("title") ~
      get[String]("url") ~
      get[Int]("cnt") map {
          case title~url~cnt => Entry(title, url, cnt)
      }
  }
  def findByTimestampId(timestampId: Long): List[Entry] = {
      DB.withConnection{ implicit c =>
        SQL("""select contents.title,  contents.url,  entrys.cnt from entrys
               inner join contents on entrys.content_id = contents.id where timestamp_id={id}""")
           .on("id" -> timestampId)
           .as(Entry.simple *)
      }
  }

}
