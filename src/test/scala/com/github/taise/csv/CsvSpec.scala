import com.github.taise.csv._
import org.scalatest._

class CsvSpec extends FlatSpec with ShouldMatchers {

  "Csv" should "parse a single string" in {
    Csv.parse("abcdef").mkString should be("abcdef")
  }

  it should "parse simple csv text" in {
    val csvText: String = "abc,def"
    val res: Seq[String] = Csv.parse(csvText)
    res.size should be(2)
    res(0) should be("abc")
    res(1) should be("def")
  }

  it should "parse csv text include quotations" in {
    val csvText: String = """"abc","def""""
    val res: Seq[String] = Csv.parse(csvText)
    res.size should be(2)
    res(0) should be("abc")
    res(1) should be("def")
  }

  it should "parse csv text include blank" in {
    val csvText: String = "abc,,def"
    val res: Seq[String] = Csv.parse(csvText)
    res.size should be(3)
    res(0) should be("abc")
    res(1) should be("")
    res(2) should be("def")
  }

  it should "parse csv text include comma between quote" in {
    val csvText: String = """abc,"include,comma",def"""
    val res: Seq[String] = Csv.parse(csvText)
    res.size should be(3)
    res(0) should be("abc")
    res(1) should be("include,comma")
    res(2) should be("def")
  }
}
