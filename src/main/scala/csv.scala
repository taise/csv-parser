object CSV {
  def main(args: Array[String]) {
    val parsed: Seq[String] = parse("""test,"st,ri,ng",123""")
  }

  def parse(line: String) : Seq[String] = {
    val quote: Char = '"'
    val colSep: Char = ','
    var csv = Seq[String]()
    var inExtendedCol: Boolean = false

    val parts: Seq[String] = line.split(',')


    // pattern
    //   "st,ri,ng"
    //   "string"
    //   string
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
      } else if(part(0) == quote) {
        // cases: "st, "string"
        if (part.last != quote || part.count(_.toString == "\"") % 2 != 0) {
          // case: "st
          csv = csv :+ part.tail
          csv = lastUpdate(csv, colSep)
          inExtendedCol = true
        } else {
          // case: "string"
          csv = csv :+ part.tail
          csv = lastUpdate(csv, colSep)
        }
      } else {
        // case: string
        csv = csv :+ (if (part.isEmpty) "" else part)
      }
    })
    println(csv)
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
