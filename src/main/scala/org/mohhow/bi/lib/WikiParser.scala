package org.mohhow.bi.lib

import org.mohhow._
import model._
import snippet._
 
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.Parsers
import java.math.BigDecimal

import net.liftweb._
import http._
import SHtml._
import S._

import mapper._
import util._
import Helpers._
import org.mohhow.bi.util.{Utility => MyUtil}

object WikiParser {
	
 // methods for wiki transformation

 def replaceWithTags(wikiSign: String, tag: String, text: String) = {
  def createTag(tag: String, isStartTag: Boolean) = if(isStartTag) "<" + tag + ">" else "</" + tag + ">"
  def replaceContextDependent(wikiSign: String, tag: String, text: String, isStartTag: Boolean): String = {
   val pattern = wikiSign.r
   val firstMatch = pattern findFirstIn text
   
   firstMatch match {
	   case None => text
	   case Some(m) => replaceContextDependent(wikiSign, tag, pattern replaceFirstIn(text, createTag(tag, isStartTag)), !isStartTag)  	   
   }
  }
	 
  replaceContextDependent(wikiSign, tag, text, true)
 }
 
 def transformEnumeration(text: String): String = {

	 def transformLine(line: String, level: Int, isNumbered: Boolean) = {
		 val pattern = if(isNumbered) """\*+""".r  else """#+""".r 
		 val firstMatch = pattern findFirstIn line
		 val startTag = if(isNumbered) "<ol>" else "<ul>" 
		 val endTag = if(isNumbered) "</ol>" else "</ul>" 
		 
		 firstMatch match {
		 	case None => if(level > 0)  (0, MyUtil.repeat(endTag, level) + line) else (0, line) 
		 	case Some(index) => {
		 		if(level == 0) (index.length, MyUtil.repeat(startTag, index.length) + "<li>" + line.substring(index.length) + "</li>")
		 		else if(level == index.length) (index.length, "<li>" + line.substring(index.length) + "</li>")
		 		else if(level < index.length) (index.length, MyUtil.repeat(startTag, index.length - level) + "<li>" + line.substring(index.length) + "</li>")
		 		else if(level > index.length) (index.length, MyUtil.repeat(endTag, level - index.length) + "<li>" + line.substring(index.length) + "</li>")
		 		else (0, line)
		 	}
		 }
	 }
	 
	 def transformLines(lines: List[String], level: Int, isNumbered: Boolean, resultLines:List[String]):List[String] = lines  match {
		 case Nil => resultLines.reverse
		 case l :: ls => {
			 val newLine = transformLine(l, level, isNumbered)
			 transformLines(ls, newLine._1, isNumbered, newLine._2 :: resultLines)
		 } 
	 }
	 
	 val lines = text.split("\n").toList ::: List("")
	 val transformedLines = transformLines(transformLines(lines,0,true, Nil),0,false, Nil)
	 
	 ("" /: transformedLines) (_ + _)
 }
 
 def transformWikiText(text: String) = transformEnumeration(replaceWithTags("''", "em", replaceWithTags("'''", "b", replaceWithTags("''''", "em", text))))
 def transformVisionText(text: String) = transformWikiText("""</subject>|</domain>""".r replaceAllIn("""<subject>|<domain>""".r replaceAllIn(text, "<em>"),"</em>"))
 
 // parse a term; in case of successful parsing, a Math XML expression will be returned 
 
 def replaceOneAtom(text: String, isMeasure: Boolean, badAtoms:List[String]): (String, List[String]) = {
  def getId(name: String, isMeasure: Boolean): Long = {
	  if(isMeasure) {
	 	  val msrs = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is), By(Measure.shortName, name))
	 	  if(msrs.isEmpty) -1 else msrs.apply(0).id
	  }
	  else {
	 	  val attrs = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is), By(ModelVertex.elementName, name), By(ModelVertex.elementType, "attribute"))
	 	  if(attrs.isEmpty) -1 else attrs.apply(0).id
	  }
  }
  def findPattern(isM: Boolean) = if(isM) """\$[^\$]+\$""".r else """\[[^\[\]]+\]""".r
  val pattern = findPattern(isMeasure)
  val firstMatch = pattern findFirstIn text
   
   firstMatch match {
	   case None => (text, badAtoms)
	   case Some(atom) => {
	  	   val name = atom.substring(1, atom.length - 1) 
	  	   
	  	   val id = getId(name,isMeasure)
	  	   if(id < 0) (text, name :: badAtoms)
	  	   else if(isMeasure) replaceOneAtom(pattern replaceFirstIn(text, "<m" + id.toString + ">"), isMeasure, badAtoms)
	  	   else replaceOneAtom(pattern replaceFirstIn(text, "<d" + id.toString + ">"), isMeasure, badAtoms)
	   }
   }
 }

 def makeReplacements(text: String) = {
  val replacedText = replaceOneAtom(text, true, Nil)
  replaceOneAtom(replacedText._1, false, replacedText._2)
 }
 
 def parseTerm(text: String): String = {
  val parser = new Term
  
  parser.parseAll(parser.measureTerm, text) match {
	 case parser.Success(result, _) => result.toString 
	 case parser.NoSuccess(msg, next) => msg
  }
 }
 
 def parseFilter(text: String): String = {
  val parser = new Term
  parser.parseAll(parser.filter, text) match {
	  case parser.Success(result, _) => result.toString 
	  case parser.NoSuccess(msg, next) => msg
  }
 }
 
 def substituteMeasure(text: String, vals:List[BigDecimal]): String = vals match {
	 case Nil => text
	 case someValue :: tailVals => {
		 val substitutedText = """<m\d+>""".r replaceFirstIn(text, someValue.toString)
		 substituteMeasure(substitutedText, tailVals)
	 }
 }
 
 def evaluateTerm(text: String, vals: List[BigDecimal]): Option[BigDecimal] = {
  val parser = new Term
  val evaluatedText = substituteMeasure(text, vals)
  parser.parseAll(parser.evalExpr, evaluatedText) match {
	  case parser.Success(result, _) => Some(result) 
	  case parser.NoSuccess(msg, next) => None
  }
 }
 
 def evaluateIndicator(text: String, vals: List[BigDecimal]): String = {
  val parser = new Term
  val evaluatedText = substituteMeasure(text, vals)

  parser.parseAll(parser.evalIndicator, evaluatedText) match {
	  case parser.Success(result, _) => result 
	  case parser.NoSuccess(msg, next) =>  "1"  
  }
 }
 
 def checkTerm(text: String): (Boolean, String) = {
  val parser = new Term
  val replacement = makeReplacements(text)
  
  if(replacement._2.isEmpty) {
	  println(replacement._1)
	  parser.parseAll(parser.measureTerm, replacement._1) match {
	  	case parser.Success(result, _) => (false, "") 
	  	case parser.NoSuccess(msg, next) => (true, msg)
	  }
  }
  else (true, replacement._2.toString)
 }
 
 def checkFilter(text: String): (Boolean, String) = {
  val parser = new Term
  parser.parseAll(parser.filter, text) match {
	  case parser.Success(result, _) => (false, text) 
	  case parser.NoSuccess(msg, next) => (true, msg)
  }
 }
 
 def parseBusinessQuestion(text: String): (Boolean, String) = {
  val parser = new BusinessQuestion
  parser.parseAll(parser.question, text) match {
	case parser.Success(result, _) => (false, result.toString) 
	case parser.NoSuccess(msg, next) => (true, msg)
  } 
 }
 
 def contentOfQuestion(text: String): (String, List[(String, List[(String, String)])]) = {
  val parser = new BusinessQuestion
  parser.parseAll(parser.metadata, text) match {
	case parser.Success(result, _) => result 
	case parser.NoSuccess(msg, next) => (msg.toString, Nil)
  } 
 }
 
 def findTaggedText(text: String, tagName: String) = {
  val TagPattern = """(<)(\w+)(>)([^<>]*)(</)(\w+)(>)""".r
  
  val result = for(TagPattern(_, openingTag, _, tagContent, _, closingTag, _) <- TagPattern findAllIn text)
	yield (openingTag == tagName && closingTag == tagName, tagContent)
  
  result.filter(_._1).map(_._2).toList
 }   
}

class Term extends JavaTokenParsers {

 // parse a natural number
	
 def naturalNumber: Parser[Any] = """\d+""".r
 def evalNatural: Parser[Int] = """\d+""".r^^{case n => n.toInt}
 
 // parse a decimal
 
 def number: Parser[Any] = floatingPointNumber^^{ case x => "<mn>" + x.toString + "</mn>"}
 def evalNumber: Parser[BigDecimal] = floatingPointNumber^^{ case x => new BigDecimal(x.toString)}

 // parse  measure or dimension element
 
 def measureDim: Parser[Any] = """<[d|m]\d+>""".r^^{ case x => prettyAtom(x)}
  
 def evalHelper(text: String, mode: String) = {
  val pattern = """(<)([d|m])(\d+)(>)""".r
  val firstMatch = pattern findFirstIn text
  
  firstMatch match {
	  case Some(dimMeasure) => {
	 	  
	 	val dm = dimMeasure.toString 
	 	  
	 	if(dm.substring(1,2) == "m") {
	 		val msrs = Measure.findAll(By(Measure.id, dm.substring(2,dm.length - 1).toLong))
	 		
	 		if(mode == "measure") {
	 			if(msrs.isEmpty) "<mi>[" + dm.substring(1,dm.length - 1) + "]</mi>" else "<mi>[" + msrs.apply(0).shortName + "]</mi>"
	 		}
	 		else if (mode == "id") {
	 			if(msrs.isEmpty) "0" else msrs(0).id.toString
	 		}
	 	}
	 	else {
	 		val elms = ModelVertex.findAll(By(ModelVertex.id, dm.substring(2,dm.length - 1).toLong))
	 		if(elms.isEmpty) "<mi>[" + dm.substring(1,dm.length - 1) + "]</mi>" else "<mi>[" + elms.apply(0).elementName + "]</mi>"
	 	}
	  }
	  case _ => "<mi>[--]</mi>"
  }
 }
 
 def prettyAtom(text: String) = evalHelper(text, "measure")
 
 // parse bracket expressions
 
 def brackets: Parser[Any] = "("~expr~")"^^{ case "("~expr~")" => "<mo>(</mo>" + expr +"<mo>)</mo>"}
 def evalBrackets: Parser[BigDecimal] = "("~evalNumber~")"^^{ case "("~evalNumber~")"=> evalNumber}
 
 // meaning, range, plain, random
 
 def meanIt(value: BigDecimal, reference: String) = {
  def triple(l:List[String]) = (new BigDecimal(l(0)), new BigDecimal(l(1)), l(2))
  def compare(d: BigDecimal, l: List[(BigDecimal, BigDecimal, String)]): String = l match {
	  case Nil => ""
	  case head :: tail => if(head._1.floatValue() < d.floatValue() && head._2.floatValue() >= d.floatValue()) head._3 else compare(d, tail)
  }
  
  val l = reference.split(";").map(item => triple(item.split("_").toList)).toList 
  compare(value, l)
 }
 
 def meaning: Parser[Any] = "meaning("~measureDim~")"^^{ case "meaning("~m~")" => "<mi>meaning</mi><mfenced>" + m +  "</mfenced>"}
 def valueList: Parser[String] = """[\d,\_,;,\.,-]+""".r
 def evalMeaning: Parser[String] = "meaning("~evalNumber~","~valueList~")"^^{ case "meaning("~evalNumber~","~list~")" => meanIt(evalNumber, list)}
  
 def range: Parser[Any] = "range("~measureDim~")"^^{ case "range("~m~")" => "<mi>range</mi><mfenced>" + m +  "</mfenced>"}
 def evalRange: Parser[String] = "range("~evalNumber~","~valueList~")"^^{ case "range("~evalNumber~","~list~")" => meanIt(evalNumber, list)}
 
 def unit: Parser[Any] = """[^,()]*""".r
 def plain: Parser[Any] = "plain("~measureDim~")"^^{ case "plain("~m~")" => "<mi>plain</mi><mfenced>" + m +  "</mfenced>"}
 def evalPlain: Parser[String] = "plain("~evalNumber~","~unit~")"^^{ case "plain("~evalNumber~","~unit~")" => evalNumber.toString + " " + unit.toString}

 def rnd: Parser[Any] = "rnd("~number~","~number~";"~repsep(naturalNumber~","~number,",")~")"^^{ case "rnd("~min~","~max~";"~appendix~")" => "<mi>rnd</mi><mfenced>" + min + max + ";" + "..." + "</mfenced>"}
	
 // a factor is either a number expression, a measure or dimension literal or an expression in brackets	 
 
 def factor: Parser[Any] = number | measureDim | brackets
 def evalFactor = evalNumber | evalBrackets
 
 // parse the power of a decimal with a natural number
 
 def power: Parser[Any] = "power("~expr~","~naturalNumber~")"^^{case "power("~x~","~naturalNumber~")" => "<msup><mrow>" + x + "</mrow><mn>" + naturalNumber +"</mn></msup>"}
 def evalPower: Parser[BigDecimal] = "power("~evalFactor~","~evalNatural~")"^^{case "power("~factor~","~n~")" => factor.pow(n)}
 
 // parse the fractions
 
 def fraction: Parser[Any] = "frac("~expr~","~expr~")"^^{case "frac("~nominator~","~denominator~")" => "<mfrac><mrow>" + nominator +"</mrow><mrow>" + denominator + "</mrow></mfrac>"}
 def evalFraction: Parser[BigDecimal] = "frac("~evalFactor~","~evalFactor~")"^^{case "frac("~n~","~d~")" => n.divide(d)}
 
 def sqrt: Parser[Any] = "sqrt("~expr~")"^^{case "sqrt("~x~")" => "<msqrt>" + x + "</msqrt>"}
 def evalSqrt: Parser[BigDecimal] = "sqrt("~evalFactor~")"^^{case "sqrt("~x~")" => new BigDecimal(java.lang.Math.sqrt(x.doubleValue()).toString)}
 
 // parse terms
 def term: Parser[Any] = (factor|power|fraction|sqrt)~rep("*"~(factor|power|fraction|sqrt) | "/"~(factor|power|fraction|sqrt))^^{
  case f1~f2 => f1 + ce(f2)
  case f1 => f1
 }
 
 def evalPart(x: ~[String, BigDecimal]) = x match {
  case "*"~d => d
  case "/"~d => (new BigDecimal("1")).divide(d)
  case "+"~d => d
  case "-"~d => d.multiply(new BigDecimal("-1"))
 }
 
 def evalTerm: Parser[BigDecimal] = (evalFactor|evalPower|evalFraction)~rep("*"~(evalFactor|evalPower|evalFraction) | "/"~(evalFactor|evalPower|evalFraction))^^{
  case f1~f2 => (f1 /: f2.map(evalPart)) (_.multiply(_))
 }
 
 // parse expressions
 
 def expr: Parser[Any] = term~rep("+"~term | "-"~term)^^{
  case t1~t2 => t1 + ce(t2)
  case t1 => t1
 }
 
 def plus(x: BigDecimal, y: BigDecimal) = x.add(y)
 
 def evalExpr: Parser[BigDecimal] = evalTerm~rep("+"~evalTerm | "-"~evalTerm)^^{
	 case t1~t2 => t1.add((new BigDecimal("0") /: t2.map(evalPart)) (plus))
 }
 
 def aggregate: Parser[Any] = ("sum"|"min"|"max"|"avg")~"("~expr~")"^^{
  case op~"("~expr~")" => "<mo>" + op + "</mo><mo>(</mo>" + expr +  "<mo>)</mo>"
 }
 
 def measureTerm = expr | aggregate | meaning | range | plain | rnd
 def evalIndicator = evalPlain | evalRange | evalMeaning
 
 val RightTermPattern = """(\()([+,\-,/,*])(~)([^\)]+)(\))""".r
 
 def manageRightTerm(rightTerm: Any) = rightTerm.toString match {
	 case RightTermPattern(lbracket, operand, tilde, remainder, rbracket) => "<mo>" + operand + "</mo>" + remainder
	 case _ => println(rightTerm.toString); rightTerm.toString
 }
 
 def concAndEnriche(left: Any, right: Any): String = manageRightTerm(left) + manageRightTerm(right)
 
 def ce(l: List[Any]) = ("" /: l) (concAndEnriche)
 
 def literal: Parser[Any] = "'"~"""[^']*""".r~"'"^^{case "'"~someText~"'" => "<mi>'" + someText + "'</mi>"}
 def timePattern: Parser[Any] = "?"~("today"|"yesterday"|"tomorrow" |"actual_week"|"actual_month"|"actual_quarter"|"actual_year"
		                                    |"previous_week"|"previous_month"|"previous_quarter"|"previous_year")~"?"^^{case "?"~pattern~"?" => "?" + pattern + "?"}
 
 def parameter: Parser[Any] = "?"|"'?'"
 
 def separate (x: ~[String, Any]) = x match {
  case s~d => s + d
 }
 
 def filterAtom = expr|literal|timePattern|parameter
 
 def relation: Parser[Any] = filterAtom~("="|"<"|">"|"<="|">=")~filterAtom^^{case expr1~r~expr2 => expr1 + "<mo>" + r + "</mo>" + expr2}
 def between: Parser[Any] = filterAtom~"between"~expr~"and"~expr^^{case atom~"between"~expr1~"and"~expr2 => atom + "<mo>between</mo>" + expr1 + "<mo>and</mo>" + expr2}
 
 def in: Parser[Any] = expr~"in"~"("~filterAtom~rep(","~filterAtom)~")"^^{case expr1~"in" ~"("~item~moreItems~")" => 
 																	   val serializedItems = ("" /: moreItems.map(separate)) (_ + _)
 																	   expr1 + "in(" + item + serializedItems + ")"
 }
 
 def filterItem = relation | between | in
 
 def filter: Parser[Any] = filterItem~rep(("and"|"AND"|"or"|"OR")~filterItem)^^{case cond~conds => 
 																				val condsString = ("" /: conds.map(separate)) (_ + _)
 																				cond + condsString}
 /*
 def filterBrackets: Parser[Any] = "("~(filterItem | filterCombination )~")"^^{case "("~inner~")" => "<mo>(</mo>" + inner + "<mo>)</mo>" }
 
 def filter: Parser[Any] = filterBrackets~rep(("and"|"AND"|"or"|"OR")~filterBrackets)^^{case cond2~conds2 => 
 																				val conds2String = ("" /: conds2.map(separate)) (_ + _)
 																				cond2 + conds2String} */
}

class Wiki extends JavaTokenParsers {
 var elms = List()
 
 def plainText: Parser[Any] = """[^#'\*\.<>]*""".r
 def italic: Parser[Any] = "''"~plainText~"''" ^^{ case "''"~text~"''" => "<em>" + text + "</em>"}
 def bold: Parser[Any] = "'''"~plainText~"'''" ^^{ case "'''"~text~"'''" => "<b>" + text + "</b>"}
 def boldItalic: Parser[Any] = "''''"~plainText~"''''" ^^{ case "''''"~text~"''''" => "<b><em>" + text + "</em></b>"}
 def wiki: Parser[Any] = plainText~rep(italic | bold | boldItalic)^^{
		case t1~t2 => t1 + ("" /: t2) (_ + _)
		case t1 => t1
 	}
 def tagStart: Parser[Any] = "<"~plainText~">"
 def tagEnd: Parser[Any] = "</" ~plainText~">"
 def element: Parser[Any] = tagStart~wiki~tagEnd^^{case "<"~start~">"~text~"</"~end~">" => if (start.toString == end.toString) text else ""}
 def richText: Parser[Any] = rep(wiki~element)
	
}

class Vision extends Wiki {
 def domain: Parser[Any] = "<domain>"~plainText~"</domain>"^^{ case "<domain>"~text~"</domain>" => "<em>" + text + "</em>"}
 def subject: Parser[Any] = "<subject>"~plainText~"</subject>"^^{ case "<subject>"~text~"</subject>" => "<em>" + text + "</em>"}
  
 def visionText: Parser[Any] = plainText~rep(italic | bold | boldItalic | domain | subject | plainText)^^{
		case t1~t2 => t1 + ("" /: t2) (_ + _)
		case t1 => t1
 	}
}

class BusinessQuestion extends JavaTokenParsers {
	
 def plainText: Parser[Any] = """[^#'\*\.\[\]\$\?<>]*""".r
 def untaggedText: Parser[Any] = """[^<>]*""".r
 def role: Parser[Any] = "<role>"~plainText~"</role>"^^{ case "<role>"~plainText~"</role>" => "<em>" + plainText + "</em>"}
 def roleName: Parser[Any] = "<role>"~plainText~"</role>"^^{ case "<role>"~plainText~"</role>" => plainText}
 def whoWants: Parser[Any] = plainText~role~plainText^^{case leftText~role~rightText => leftText + role.toString + " " + rightText}
 def getRole: Parser[Any] = plainText~roleName~plainText^^{case leftText~roleName~rightText => roleName}
  
 def context: Parser[Any] = "<context>"~plainText~"</context>"^^{ case "<context>"~plainText~"</context>" => plainText}
 def comparison: Parser[Any] = "<comparison>"~untaggedText~"</comparison>"^^{case "<comparison>"~untaggedText~"</comparison>" => simplify(untaggedText.toString)}
 def comparisonContent: Parser[(String, List[(String, String)])] = "<comparison>"~untaggedText~"</comparison>"^^{case "<comparison>"~untaggedText~"</comparison>" => ("plain", getMetadata(untaggedText.toString))}
 def comparisons: Parser[Any] = repsep(comparison, ",")^^{case l => ("" /: l) (_ + " " + _ + ",")}
 def comparisonsContent: Parser[List[(String, List[(String, String)])]] = repsep(comparisonContent, ",")
 def filter: Parser[Any] = opt("<filter>"~untaggedText~"</filter>")^^{
	 case None => ""
	 case Some("<filter>"~untaggedText~"</filter>") => untaggedText
	 case _ => ""
 }
 
 def question: Parser[Any] = whoWants~comparisons~plainText~filter~"."^^{
	 case w~c~p~f~"." => w.toString + c.toString + p.toString + f.toString + "."
 }
 
 def metadata: Parser[(String, List[(String, List[(String, String)])])] = getRole~comparisonsContent~plainText~filter~"."^^{
	 case r~c~p~f~"." => (r.toString,  c)
 }
 
 def simplify(text: String) = {
  val pattern = """[\[\]\$\?]""".r
  pattern replaceAllIn(text, "")
 }
  
 def getMetadata(text: String): List[(String, String)] = {
  def getPattern(kind: String) = kind match {
   case "measure" => """\$[^\$]*\$""".r 
   case "context" => """\[[^\[\]]*\]""".r
   case "parameter" => """\?[^\?]*\?""".r	  
  }
  
  def find(text: String, l: List[(String, String)], kind: String): List[(String, String)] = {
   val pattern = getPattern(kind)
   val firstMatch = pattern findFirstIn text
   
   firstMatch match {
	   case None => l
	   case Some(m) => { 
	  	   val inner = m.substring(1, m.length - 1)
	  	   val newText = pattern replaceFirstIn(text, "")
	  	   find(newText, (kind, inner ) :: l, kind)  	   
	   }
   }
  }
  
  find(text, Nil, "measure") ::: find(text, Nil, "context") ::: find(text, Nil, "parameter")
    
 } 
}