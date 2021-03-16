package Chat

import Tokens._
import Utils.Dictionary.dictionary
import Utils.SpellChecker._

class Tokenizer(input: String) {

  private var tokens : Array[String] = Array()
  private var index = 0

  /**
    * Separate the user's input into tokens.
    */
  // TODO - Step 3
  def tokenize(): Unit = {
    //not tested
    tokens = input.filter(c => !List('.',',','!','?','*').contains(c)).replace("'", " ").replace("  "," ").split(" ")
  }

  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  // TODO - Step 3
  def nextToken(): (String, Token) = ???
}
