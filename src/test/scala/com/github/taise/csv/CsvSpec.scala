import com.github.taise.csv._
import org.scalatest._

class CsvSpec extends FlatSpec with ShouldMatchers {

  "Csv" should "be able to parse" in {
    Seq(
      Map("abcdef"               -> Seq("abcdef")),             // a single string
      Map("\t"                   -> Seq("\t")),                 // a tab
      Map("foo,\"\"\"\"\"\",baz" -> Seq("foo", "\"\"", "baz")), // quotations
      Map("abc,def"              -> Seq("abc", "def")),         // simple csv text
      Map(""""abc","def""""      -> Seq("abc", "def")),         // put quotations
      Map("""a,",",b"""          -> Seq("a", ",", "b")),        // include comma between quote
      Map("""a""b"""             -> Seq("""a"b""")),            // escaped quote
      Map("""a\r\nb"""           -> Seq("""a\r\nb""")),         // include newline
      Map("abc,,def"             -> Seq("abc", "", "def"))      // include blank
    ).foreach { xs =>
      xs.foreach { kv =>
        val (k, v) = kv
        Csv.parse(k) should be(v)
      }
    }
  }
}
