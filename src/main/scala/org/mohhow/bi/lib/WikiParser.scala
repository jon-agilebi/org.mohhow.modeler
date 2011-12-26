package org.mohhow.bi.lib

import scala.util.parsing.combinator._

object WikiParser {
 
 def transformWikiText(text: String) = {
  val parser = new Wiki
  val rawString = parser.parseAll(parser.wiki, text).get.toString
  rawString.substring(1, rawString.length - 9)
 }
 
 def getElements(text: String) = {
  val parser = new Wiki
  parser.parseAll(parser.richText, text)
  parser.elms	 
 }
 
 def parseTerm(text: String): String = {
  val parser = new Term
  parser.parseAll(parser.expr, text) match {
	  case parser.Success(result, _) => result.toString 
	  case parser.NoSuccess(msg, next) => msg
  }
 }
 
 def checkTerm(text: String): (Boolean, String) = {
  val parser = new Term
  parser.parseAll(parser.expr, text) match {
	  case parser.Success(result, _) => (false, "") 
	  case parser.NoSuccess(msg, next) => (true, msg)
  }
 }
 
}

class Term extends JavaTokenParsers {
 
 def number: Parser[Any] = floatingPointNumber^^{ case x => "<mn>" + x.toString + "</mn>"} 
 def measure: Parser[Any] = """m\d+""".r^^(x => prettyMeasure(x))
 def dim: Parser[Any] = """d\d+""".r^^(x => prettyDimension(x))
 def naturalNumber: Parser[Any] = """\d+""".r
 def measureDim: Parser[Any] = "<"~(measure | dim)~">"^^{case "<"~expr~">" => "<mi>" + expr + "</mi>"}
 def prettyDimension(x: String) = "[myDimension]"
 def prettyMeasure(x: String) = "[myMeasure]"
 def factor: Parser[Any] = number | measureDim | brackets
 def power: Parser[Any] = "power("~factor~","~naturalNumber~")"^^{case "power("~factor~","~naturalNumber~")" => "<msup><mrow>" + factor + "</mrow><mn>" + naturalNumber +"</mn></msup>"}
 def brackets: Parser[Any] = "("~expr~")"^^{ case "("~expr~")" => "<mo>(</mo>" + expr +"<mo>)</mo>"}
 def expr: Parser[Any] = term~rep("+"~term | "-"~term)^^{
		case t1~t2 => t1 + ce(t2)
		case t1 => t1
 	}
 def term: Parser[Any] = (factor|power)~rep("*"~(factor|power) | "/"~(factor|power))^^{
	 	case f1~f2 => f1 + ce(f2)
	 	case f1 => f1
	 }
 
 val RightTermPattern = """(\()([+,\-,/,*])(~)([^\)]+)(\))""".r
 
 def manageRightTerm(rightTerm: Any) = rightTerm.toString match {
	 case RightTermPattern(lbracket, operand, tilde, remainder, rbracket) => "<mo>" + operand + "</mo>" + remainder
	 case _ => println(rightTerm.toString); rightTerm.toString
 }
 
 def concAndEnriche(left: Any, right: Any): String = manageRightTerm(left) + manageRightTerm(right)
 
 def ce(l: List[Any]) = ("" /: l) (concAndEnriche)

}

class Wiki extends JavaTokenParsers {
 var elms = List()
 
 def plainText: Parser[Any] = """[^#'\*<>]*""".r
 def italic: Parser[Any] = "''"~plainText~"''" ^^{ case "''"~text~"''" => "<em>" + text + "</em>"}
 def bold: Parser[Any] = "'''"~plainText~"'''" ^^{ case "'''"~text~"'''" => "<b>" + text + "</b>"}
 def boldItalic: Parser[Any] = "''''"~plainText~"''''" ^^{ case "''''"~text~"''''" => "<b><em>" + text + "</em></b>"}
 def wiki: Parser[Any] = plainText~rep(italic | bold | boldItalic)
 def tagStart: Parser[Any] = "<"~plainText~">"
 def tagEnd: Parser[Any] = "</" ~plainText~">"
 def element: Parser[Any] = tagStart~wiki~tagEnd^^{case "<"~start~">"~text~"</"~end~">" => 
 														if (start.toString == end.toString){
 															//elms = (start.toString, text.toString) :: elms
 															text
 														} else "" 
 													}
 def richText: Parser[Any] = rep(wiki~element)
 
}