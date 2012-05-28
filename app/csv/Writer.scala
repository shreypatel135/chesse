package lila
package csv

import com.github.tototoshi.csv.CSVWriter
import play.api.Play
import play.api.Play.current
import scalaz.effects._

object Writer {

  // returns the web path
  def apply(filename: String)(lines: List[List[Any]]): IO[String] = {

    val path = "export/" + filename + ".csv"
    val file = Play.getFile("public/" + path)
    val webPath = "/assets/" + path
    val writer = new CSVWriter(file)

    io {
      writer writeAll lines.map(_ map (_.toString))
      writer.close()
    } map (_ => webPath)
  }
}
