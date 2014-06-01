package com.github.taise.csv

object Csv {
  def parse(line: String) : Seq[String] = {
    val quote: Char = '"'
    val quoteS: String = quote.toString
    val colSep: Char = ','

    val stray_quote = ".*[^\"]+\"[^\"]+.*".r

    var csv = Seq[String]()
    var inExtendedCol: Boolean = false

    val parts: Seq[String] = line.replaceAll("[\r\n]\\z","").split(",", -1)
    parts.foreach(part => {
      if (inExtendedCol) {
        if (part.last == quote && part.count(_.toString == "\"") % 2 != 0) {
          csv = lastUpdate(csv, part.init)
          inExtendedCol = false
        } else {
          val last: Int = lastIndex(csv)
          csv = lastUpdate(csv, part)
          csv = lastUpdate(csv, colSep)
        }
      } else if (part.isEmpty) {
        csv = csv :+ ""
      } else if(part(0) == quote) {
        if (part.last != quote || part.count(_.toString == "\"") % 2 != 0) {
          csv = csv :+ part.tail.replaceAll(quoteS * 2, quoteS)
          csv = lastUpdate(csv, colSep)
          inExtendedCol = true
        } else {
          val noQuote: String = part.init.tail
          noQuote match {
            case stray_quote() => throw new MalformedException("Missing or stray quote in line")
            case _ => csv = csv :+ noQuote.replaceAll(quoteS * 2, quoteS)
          }
        }
      } else if(part.startsWith("\n") || part.startsWith("\r")) {
          csv = csv :+ ""
      } else {
        csv = csv :+ part.replaceAll(quoteS * 2, quoteS)
      }
    })
    csv
  }

  def lastIndex(xs: Seq[Any]): Int = {
    xs.size match {
      case h if h == 0 => 0
      case h => h - 1
    }
  }

  def lastUpdate(xs: Seq[String], s:Any): Seq[String] = {
    xs.updated(lastIndex(xs), xs.last + s)
  }
}
