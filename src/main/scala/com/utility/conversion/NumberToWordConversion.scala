package com.utility.conversion

class NumberToWordConversion extends EnglishWordsInNumberConversion {

  def convertNumberToWord(numberInput: Int): String = {
    var number = numberInput
    var word: String = ""
    var index: Int = 0
    do {
      val num = number % 1000
      if (num != 0) {
        val str = convertNumber_InPartsOfThreeOrLessDigit_ToWord(num, index)
        word = str + hundreds(index) + word
      }
      index += 1
      number = number / 1000
    } while (number > 0);

    word
  }

  def convertNumber_InPartsOfThreeOrLessDigit_ToWord(number: Int, index: Int): String = {
    var word = ""
    val num = number % 100
    var and = ""
    index match {
      case 0 if (number / 100 > 0) => and = " and"
      case 0 => and = ""
      case _ => and = ""
    }

    num match {
      case num if (num < 10) => word = and + units(num)
      case num if (num < 20) => word = and + tenToNineteen(num % 10)
      case _ => word = and + tens(num / 10) + units(num % 10)
    }
    word = if (number / 100 > 0) units(number / 100) + " hundred " + word else word
    word
  }

  def readNumberToWords(numberEntered: Int): Unit = {
    numberEntered match {
      case 0 => println("0 = zero")
      case numberEntered if (numberEntered < 0) => println(numberEntered + " = minus" + convertNumberToWord(Math.abs(numberEntered)))
      case _ => println(numberEntered + " = " + convertNumberToWord(numberEntered))
    }
  }
}

object NumberToWordConversion extends App {
  val numberToWord = new NumberToWordConversion
  print("Enter  number : ")
  try {
    val numberEntered = scala.io.StdIn.readInt()
    numberToWord.readNumberToWords(numberEntered)
  }
  catch {
    case e: NumberFormatException =>
      println("Please enter valid whole numbers only...!!")
    case e: Exception =>
      e.printStackTrace()
  }
}
