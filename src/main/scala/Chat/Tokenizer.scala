package Chat

import Tokens._
import Utils.SpellChecker

class Tokenizer(input: String) {

  private var tokens : Array[(String, Token)] = Array()
  private var tokensIterator = tokens.iterator

  /**
    * Separate the user's input into tokens.
    */
  private def getAsToken(word: String): (String, Token) = {
    word match {
      case "bonjour" => (word, Tokens.BONJOUR)
      case "je" => (word, Tokens.JE)
      case "svp" => (word, Tokens.SVP)
      case "etre" => (word, Tokens.ETRE)
      case "vouloir" => (word, Tokens.VOULOIR)
      case "et" => (word, Tokens.ET)
      case "ou" => (word, Tokens.OU)
      case "biere" => (word, Tokens.BIERE)
      case "croissant" => (word, Tokens.CROISSANT)
      case name if word.charAt(0) == '_' => (word, Tokens.PSEUDO)
      case number if word forall Character.isDigit => (word, Tokens.NUM)
      case _ => (word, Tokens.UNKNOWN)

    }
  }

  def tokenize(): Unit = {
    tokens = input.filter(c => !List('.',',','!','?','*').contains(c))
      .replace("'", " ")
      .replaceAll(" +"," ")
      .split(" ")
      .map(t => getAsToken(SpellChecker.getClosestWordInDictionary(t)))

    tokensIterator = tokens.iterator
  }

  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token) = {
    if(tokensIterator.hasNext){
      tokensIterator.next()
    }else{
      ("EOL", Tokens.EOL)
    }
  }
}
