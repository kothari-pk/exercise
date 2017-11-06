package file

import java.io.File
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

object FileIO {
  val sleep = List(1000, 2000, 3000, 4000, 5000)

  def getMessage(source: String): String = {
    Thread.sleep(Random.shuffle(sleep).head)
    s"Hello from $source"
  }

  def printToFile(file: File, data: String): Unit = {
    // Create the instance of the PrintWriter
    val pw = new java.io.PrintWriter(file)
    try {
      pw.write(data)
    } finally {
      pw.close()
    }
  }

  def consumeMessages(fileName: String, s1: String, s2: String): Unit = {
    val message1: Future[String] = Future { getMessage(s1) }
    val message2: Future[String] = Future { getMessage(s2) }

    val file: File = new java.io.File(fileName)

    message1.onComplete {
      case Success(data) => printToFile(file, data)
      case Failure(t) => println(s"Error ${t.getMessage}")
    }

    message2.onComplete {
      case Success(data) => printToFile(file, data)
      case Failure(t) => println(s"Error ${t.getMessage}")
    }
  }

}
