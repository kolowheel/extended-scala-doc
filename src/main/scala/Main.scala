import java.io.{PrintWriter, OutputStreamWriter, FileOutputStream, File}

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{CompilerCommand, _}


/**
 * @author yaroslav.gryniuk
 */

object Main {

  var reporter: ConsoleReporter = _

  def scalaFiles(base: File, name: String): Seq[String] = {
    val b = new ArrayBuffer[String]
    def collect(f: File, s: String) {
      //println("Scanning "+f.getPath)
      val fn = f.getName
      if (f.isDirectory) {
        if (fn == "." || !fn.startsWith(".")) {
          val files = f.listFiles
          if (files ne null) files foreach { ch => collect(ch, null) }
        }
      } else if (s ne null) b += s
      else if (fn endsWith ".scala") b += f.getPath
    }
    collect(base, name)
    b
  }


  def process(args: Array[String]): Unit = {

    val errorResolver = (x: String) => println(x)
    val docSettings = new doc.Settings(errorResolver)
    docSettings.processArguments(args.toList, true) // without this line has "fatal error: object scala in compiler mirror not found."
    reporter = new ConsoleReporter(docSettings)
    val command = new CompilerCommand(args.toList, errorResolver)

    if (!reporter.hasErrors) {
      try {
        val sourcePath = docSettings.sourcepath.value
        val expFiles = scalaFiles(new File(sourcePath), "").toList
        val docProcessor = new DocFactory(reporter, docSettings)
        val file = new PrintWriter(new OutputStreamWriter(new FileOutputStream("test", false)), true)
        docProcessor.document(expFiles,(x: AnyRef) => file.println(x))
      }
      catch {
        case ex@FatalError(msg) =>
          if (docSettings.debug.value) ex.printStackTrace();
          reporter.error(null, "fatal error: " + msg)
      }
      finally {
        reporter.printSummary()
      }
    }

  }

  def main(args: Array[String]): Unit = {
    process(args)
  }


}
