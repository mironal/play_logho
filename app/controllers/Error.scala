package controllers

import play.api.libs.json._

sealed trait Error {
    def msg(): String
}

// new LocalDate()した結果が、 org.joda.time.IllegalFieldValueException
//org.joda.time.IllegalFieldValueException: Cannot parse "2000-2-30": Value 30 for dayOfMonth must be in the range [1, 29]
//あとで頑張って適切にエラーメッセージ出すといいかも.
case class InvalidFieldValue(y: Int, m: Int, d: Int)(implicit e: Throwable) extends Error {
  override def msg(): String = {
    s"Invalid field value $y-$m-$d"
  }
}

// java.lang.IllegalArgumentException: Invalid format: "aaaaa"
case class InvalidFormat(str: String) extends Error {
  override def msg(): String = {
    s"Invalid format: $str"
  }
}


