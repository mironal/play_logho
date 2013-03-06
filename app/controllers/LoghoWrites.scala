package controllers

import play.api.libs.json._
import models._

object LoghoWrites {

  // EntryからJsonへの変換方法の定義
  implicit object EntryWrites extends Writes[Entry] {
    def writes(e: Entry): JsValue = {
      Json.obj(
        "title" -> e.title,
        "url" -> e.url,
        "cnt" -> e.cnt
      )
    }
  }

  // EntrysTimestampからJsonへの変換方法の定義
  implicit object EntrysTimestampWrites extends Writes[EntrysTimestamp] {
    import java.text.SimpleDateFormat
    import java.util.Date
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
}
