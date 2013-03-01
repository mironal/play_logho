package models

import com.github.nscala_time.time.Imports._

import java.util.Date

case class EntrysTimestamp(tweet: String, timestamp: Date, entrys: List[Entry])

object EntrysTimestamp {
    val HOT = "hot"
    val NEW = "new"

    def findHotentrys(date: LocalDate): List[EntrysTimestamp] = {
        timestamps(HOT, date).map(timestampToEntrysTimestamp)
    }

    def findNewentrys(date: LocalDate): List[EntrysTimestamp] = {
        timestamps(NEW, date).map(timestampToEntrysTimestamp)
    }



    private def timestamps(entryType: String, date: LocalDate): List[Timestamp] = {
      entryType match {
        case HOT=> Timestamp.findHotentrysByDate(date)
        case NEW => Timestamp.findNewentrysByDate(date)
        case x => throw new IllegalStateException(x)
      }
    }

    private def timestampToEntrysTimestamp(ts: Timestamp) =
      EntrysTimestamp(ts.tweet, ts.created, Entry.findByTimestampId(ts.id))
}
