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

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = {
    misspelledWord match {
      case name if (misspelledWord[0] == '_') => misspelledWord
      case number if (misspelledWord forall Character.isDigit) => misspelledWord
      case _ => {
        //dictionary.keys.map(k => (k, stringDistance(k, misspelledWord))). //Je vois pas trop comment faire ça en fait
        "to be implemented"
      }
    }
  }
}
