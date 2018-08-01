package com.utility.conversion

import org.scalatest.FunSuite

class NumberToWordConversionTest extends FunSuite {

  test("Read 2 digit number in words"){
    val readNumberToWord = new NumberToWordConversion
    assert(readNumberToWord.convertNumberToWord(23) === " twenty three")
  }

  test("Read 9 digit number in words"){
    val readNumberToWord = new NumberToWordConversion
    assert(readNumberToWord.convertNumberToWord(999999999) === " nine hundred  ninety nine million nine hundred  ninety nine thousand nine hundred  and ninety nine")
  }

  test("Read 1 digit number in words"){
    val readNumberToWord = new NumberToWordConversion
    assert(readNumberToWord.convertNumberToWord(1) === " one")
  }


}
