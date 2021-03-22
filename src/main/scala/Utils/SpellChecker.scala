package Utils

import Dictionary.dictionary

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int = {
    if (Math.min(s1.length, s2.length) == 0) {
      Math.max(s1.length, s2.length)
    } else if (s1(0) == s2(0)) {
      stringDistance(s1.tail, s2.tail)
    } else {
      1 + Seq(stringDistance(s1.tail, s2), stringDistance(s1, s2.tail), stringDistance(s1.tail, s2.tail)).min
    }
  }

  /**
    * Return the closest word (in the Levenshtein's distance sense) or the first in alphabetical order
    * if the distances are equal
    * @param s1 the first word
    * @param s2 the second word
    * @return the closest word between s1 and s2
    */
  private def minWord(s1: (String, Int), s2: (String, Int)): (String, Int) = s1._2 match {
      case _ if s1._2 < s2._2 => s1
      case _ if s1._2 > s2._2 => s2
      case _ if s1._1 < s2._1 => s1
      case _ => s2
    }

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String = misspelledWord match {
      case _ if misspelledWord.charAt(0) == '_' => misspelledWord
      case _ if misspelledWord forall Character.isDigit => misspelledWord
      case _ =>
        dictionary getOrElse(dictionary.keys.map(k => (k, stringDistance(k, misspelledWord))).reduceLeft(minWord)._1,
          throw new Error("Unexpected error when searching for the best match"))
    }
}
