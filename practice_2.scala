
import scala.io.StdIn._

object StudentRecordManager {

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }

  def getStudentInfo: (String, Int, Int, Double, Char) = {
    println("Enter student's name:")
    val name = readLine()
    println("Enter student's marks:")
    val marks = readInt()
    println("Enter total possible marks:")
    val totalMarks = readInt()

    validateInput(name, marks, totalMarks) match {
      case (true, None) =>
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        (name, marks, totalMarks, percentage, grade)

      case (false, Some(errorMessage)) =>
        println(s"Error: $errorMessage")
        getStudentInfoWithRetry()
    }
  }

  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(s"Percentage: ${"%.2f".format(percentage)}%")
    println(s"Grade: $grade")
  }

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty)
      (false, Some("Name cannot be empty."))
    else if (marks < 0 || marks > totalMarks)
      (false, Some("Marks should be a positive integer not exceeding the total possible marks."))
    else
      (true, None)
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    def getInput: (String, Int, Int) = {
      println("Enter student's name:")
      val name = readLine()
      println("Enter student's marks:")
      val marks = readInt()
      println("Enter total possible marks:")
      val totalMarks = readInt()
      (name, marks, totalMarks)
    }

    var validInput = false
    var studentTuple: (String, Int, Int, Double, Char) = null

    while (!validInput) {
      val (name, marks, totalMarks) = getInput
      validateInput(name, marks, totalMarks) match {
        case (true, None) =>
          val percentage = (marks.toDouble / totalMarks) * 100
          val grade = percentage match {
            case p if p >= 90 => 'A'
            case p if p >= 75 => 'B'
            case p if p >= 50 => 'C'
            case _ => 'D'
          }
          studentTuple = (name, marks, totalMarks, percentage, grade)
          validInput = true

        case (false, Some(errorMessage)) =>
          println(s"Error: $errorMessage")
      }
    }

    studentTuple
  }
}
