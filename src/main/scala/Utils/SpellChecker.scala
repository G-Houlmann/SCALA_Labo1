package Utils

import Dictionary.dictionary

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  // TODO - Step 2
  def stringDistance(s1: String, s2: String): Int = {
    if (Math.min(s1.length, s2.length) == 0) {
      Math.max(s1.length, s2.length)
    } else if (s1(0) == s2(0)) {
      stringDistance(s1.tail, s2.tail)
    }else {
      1 + Seq(stringDistance(s1.tail, s2), stringDistance(s1, s2.tail), stringDistance(s1.tail, s2.tail)).min
    }
  }


  def minWord(word1: (String, Int), word2: (String, Int)): (String, Int) = {
    word1._2 match {
      case equal if word1._2 == word2._2 => if(word1._1 < word2._1) word1 else word2
      case lower if word1._2 < word2._2 => word1
      case _ => word2
    }

    //With conditional operators
//    if(word1._2 == word2._2) if(word1._1 < word2._1) word1 else word2
//    else if(word1._2 < word2._2) word1 else word2
  }

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String = {
    misspelledWord match {
      case name if misspelledWord.charAt(0) == '_' => misspelledWord
      case number if misspelledWord forall Character.isDigit => misspelledWord
      case _ =>
        dictionary getOrElse(dictionary.keys.map(k => (k, stringDistance(k, misspelledWord))).reduceLeft(minWord)._1,
          throw new Error("Unexpected error when searching for the best match"))
    }
  }
}
