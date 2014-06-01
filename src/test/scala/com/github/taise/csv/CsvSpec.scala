import com.github.taise.csv._
import org.scalatest._

class CsvSpec extends FlatSpec with ShouldMatchers {

  "CSV" should "master regex example" in {
    val ex:String = """Ten Thousand,10000, 2710 ,,"10,000","It's ""10 Grand"", baby",10K"""
    val expect:Seq[String]
      =  Seq("Ten Thousand", "10000", " 2710 ", "",
             "10,000", "It's \"10 Grand\", baby", "10K")
    Csv.parse(ex) should be(expect)
  }

  it should "be able to parse" in {
    Seq(
      Map("abcdef"                  -> Seq("abcdef")),
      Map("\t"                      -> Seq("\t")),
      Map("foo,\"\"\"\"\"\",baz"    -> Seq("foo", "\"\"", "baz")),
      Map("foo,\"\"\"bar\"\"\",baz" -> Seq("foo", "\"bar\"", "baz")),
      Map("\"\"\"\n\",\"\"\"\n\""   -> Seq("\"\n", "\"\n")),
      Map("foo,\"\r\n\",baz"        -> Seq("foo", "\r\n", "baz")),
      Map("\"\""                    -> Seq("")),
      Map("foo,\"\"\"\",baz"        -> Seq("foo", "\"", "baz")),
      Map("abc,def"                 -> Seq("abc", "def")),
      Map(""""abc","def""""         -> Seq("abc", "def")),
      Map("""a,",",b"""             -> Seq("a", ",", "b")),
      Map("""a""b"""                -> Seq("""a"b""")),
      Map("""a\r\nb"""              -> Seq("""a\r\nb""")),
      Map("abc,,def"                -> Seq("abc", "", "def")),
      Map("foo,\"\r.\n\",baz"       -> Seq("foo", "\r.\n", "baz")),
      Map("foo,\"\r\",baz"          -> Seq("foo", "\r", "baz")),
      Map("foo,\"\",baz"            -> Seq("foo", "", "baz")),
      Map("\",\""                   -> Seq(",")),
      Map("foo"                     -> Seq("foo")),
      Map(",,"                      -> Seq("", "", "")),
      Map(","                       -> Seq("", "")),
      Map("foo,\"\n\",baz"          -> Seq("foo", "\n", "baz")),
      Map("foo,,baz"                -> Seq("foo", "", "baz")),
      Map("\"\"\"\r\",\"\"\"\r\""   -> Seq("\"\r", "\"\r")),
      Map("\",\",\",\""             -> Seq(",", ",")),
      Map("foo,bar,"                -> Seq("foo", "bar", "")),
      Map(",foo,bar"                -> Seq("", "foo", "bar")),
      Map("foo,bar"                 -> Seq("foo", "bar")),
      Map(";"                       -> Seq(";")),
      Map("	,	"                     -> Seq("\t", "\t")),
      Map("foo,\"\r\n\r\",baz"      -> Seq("foo", "\r\n\r", "baz")),
      Map("foo,\"\r\n\n\",baz"      -> Seq("foo", "\r\n\n", "baz")),
      Map("foo,\"foo,bar\",baz"     -> Seq("foo", "foo,bar", "baz")),
      Map(";,;"                     -> Seq(";", ";"))
    ).foreach { xs =>
      xs.foreach { kv =>
        val (k, v) = kv
        Csv.parse(k) should be(v)
      }
    }

    Seq(
      Map("foo,\"\"\"\"\"\",baz"    -> Seq("foo", "\"\"", "baz")),
      Map("foo,\"\"\"bar\"\"\",baz" -> Seq("foo", "\"bar\"", "baz")),
      Map("foo,\"\r\n\",baz"        -> Seq("foo", "\r\n", "baz")),
      Map("\"\""                    -> Seq("")),
      Map("foo,\"\"\"\",baz"        -> Seq("foo", "\"", "baz")),
      Map("foo,\"\r.\n\",baz"       -> Seq("foo", "\r.\n", "baz")),
      Map("foo,\"\r\",baz"          -> Seq("foo", "\r", "baz")),
      Map("foo,\"\",baz"            -> Seq("foo", "", "baz")),
      Map("foo"                     -> Seq("foo")),
      Map(",,"                      -> Seq("", "", "")),
      Map(","                       -> Seq("", "")),
      Map("foo,\"\n\",baz"          -> Seq("foo", "\n", "baz")),
      Map("foo,,baz"                -> Seq("foo", "", "baz")),
      Map("foo,bar"                 -> Seq("foo", "bar")),
      Map("foo,\"\r\n\n\",baz"      -> Seq("foo", "\r\n\n", "baz")),
      Map("foo,\"foo,bar\",baz"     -> Seq("foo", "foo,bar", "baz")) 
    ).foreach { xs =>
      xs.foreach { kv =>
        val (k, v) = kv
        Csv.parse(k) should be(v)
      }
    }
  }

  it should "be able to parse aras edge cases" in {
    Seq(
      Map("a,b"                     -> Seq("a", "b")),
      Map("a,\"\"\"b\"\"\""         -> Seq("a", "\"b\"")),
      Map("a,\"\"\"b\""             -> Seq("a", "\"b")),
      Map("a,\"b\"\"\""             -> Seq("a", "b\"")),
      Map("a,\"\nb\"\"\""           -> Seq("a", "\nb\"")),
      Map("a,\"\"\"\nb\""           -> Seq("a", "\"\nb")),
      Map("a,\"\"\"\nb\n\"\"\""     -> Seq("a", "\"\nb\n\"")),
      Map("a,\"\"\"\nb\n\"\"\",\nc" -> Seq("a", "\"\nb\n\"", "")),
      Map("a,,,"                    -> Seq("a", "", "", "")),
      Map(","                       -> Seq("", "")),
      Map("\"\",\"\""               -> Seq("", "")),
      Map("\"\"\"\""                -> Seq("\"")),
      Map("\"\"\"\",\"\""           -> Seq("\"","")),
      Map(",\"\""                   -> Seq("","")),
      Map(",\"\r\""                 -> Seq("","\r")),
      Map("\"\r\n,\""               -> Seq("\r\n,")),
      Map("\"\r\n,\","              -> Seq("\r\n,", ""))
    ).foreach { xs =>
      xs.foreach { kv =>
        val (k, v) = kv
        Csv.parse(k) should be(v)
      }
    }
  }

  it should "be able to parse james edge case" in {
    Csv.parse("") should be(Seq(""))
    Csv.parse("\n123\n") should be(Seq(""))
  }

  it should "be able to parse rob edge case" in {
    Seq(
      Map("\"a\nb\""                         -> Seq("a\nb")),
      Map("\"\n\n\n\""                       -> Seq("\n\n\n")),
      Map("a,\"b\n\nc\""                     -> Seq("a", "b\n\nc")),
      Map(",\"\r\n\""                        -> Seq("","\r\n")),
      Map(",\"\r\n.\""                       -> Seq("","\r\n.")),
      Map("\"a\na\",\"one newline\""         -> Seq("a\na", "one newline")),
      Map("\"a\n\na\",\"two newlines\""      -> Seq("a\n\na", "two newlines")),
      Map("\"a\r\na\",\"one CRLF\""          -> Seq("a\r\na", "one CRLF")),
      Map("\"a\r\n\r\na\",\"two CRLFs\""     -> Seq("a\r\n\r\na", "two CRLFs")),
      Map("with blank,\"start\n\nfinish\"\n" -> Seq("with blank", "start\n\nfinish"))
    ).foreach { xs =>
      xs.foreach { kv =>
        val (k, v) = kv
        Csv.parse(k) should be(v)
      }
    }
  }

  it should "be able to parse non regex edge cases" in {
    val k = "foo,\"foo,bar,baz,foo\",\"foo\""
    val v = Seq("foo", "foo,bar,baz,foo", "foo")
    Csv.parse(k) should be(v)
    intercept[MalformedException] {
      Csv.parse("1,\"23\"4\"5\", 6")
    }
  }
}
