import com.github.taise.csv._
import org.scalatest._

class CsvSpec extends FlatSpec with ShouldMatchers {

  "Csv" should "be able to parse" in {
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
  }
}
