package com.github.taise.csv

object Csv {
  def parse(line: String) : Seq[String] = {
    val quote: Char = '"'
    val quoteS: String = quote.toString
    val colSep: Char = ','
    var csv = Seq[String]()
    var inExtendedCol: Boolean = false

    val parts: Seq[String] = line.replaceAll("[\r\n]\\z","").split(",", -1)
    // pattern
    //   "st,ri,ng"
    //   "string"
    //   string
    //   ""(blank)
    parts.foreach(part => {
      if (inExtendedCol) {
        // cases: ri, ng"
        if (part.last == quote && part.count(_.toString == "\"") % 2 != 0) {
          // case: ng"
          csv = lastUpdate(csv, part.init)
          inExtendedCol = false
        } else {
          // case: ri
          val last: Int = lastIndex(csv)
          csv = lastUpdate(csv, part)
          csv = lastUpdate(csv, colSep)
        }
      } else if (part.isEmpty) {
        csv = csv :+ ""
      } else if(part(0) == quote) {
        // cases: "st, "string"
        if (part.last != quote || part.count(_.toString == "\"") % 2 != 0) {
          // case: "st
          csv = csv :+ part.tail.replaceAll(quoteS * 2, quoteS)
          csv = lastUpdate(csv, colSep)
          inExtendedCol = true
        } else {
          // case: "string"
          csv = csv :+ part.init.tail.replaceAll(quoteS * 2, quoteS)
        }
      } else if(part.startsWith("\n") || part.startsWith("\r")) {
          csv = csv :+ ""
      } else {
        // case: string
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
