package nak.util

/**
 * A very simple tokenizer that pulls most puncuation off the characters.
 * Given a raw string, tokenize it with a simple regular expression, returning
 * an IndexedSeq[String] with one token per element.
 */
object SimpleTokenizer {
  def apply(text: String): IndexedSeq[String] = text
    .replaceAll("""([\?!()\";\|\[\].,'])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
}

/**
  * A tokenizer that replaces all non-word characters with whitespace
  * and then returns a StringTokenizer.
  */
object CleanStringTokenizer {
  import java.util.StringTokenizer

  def apply (raw: String, doLowerCase: Boolean = true) =
    StringCleaner(raw, doLowerCase).split("\\s+").toIndexedSeq

}

/**
  * Cleans up a string by ripping out punctuation, turning all digit
  * sequences into a single numeric symbol, and getting rid of tokens
  * that contain mixtures of alphabetic and numeric characters.
  */
object StringCleaner {

  def apply (raw: String, doLowerCase: Boolean = true): String = {
    val cleaned = raw
      .replaceAll("[^\\p{L}\\p{N}]", " ")
      .replaceAll("""\b\d+\b""", "[-numeric-]")
      .replaceAll("""\b[^\s]{10,}\b""","")
      .replaceAll("""\b[^\s]*\w+\d+[^\s]*\b""","")
      .replaceAll("""\b[^\s]*\d+\w+[^\s]*\b""","")
      .trim()
    if (doLowerCase) cleaned.toLowerCase else cleaned
  }
    
}

