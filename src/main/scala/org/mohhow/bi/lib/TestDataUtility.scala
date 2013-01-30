package org.mohhow.bi.lib

import org.mohhow.bi.util.{Utility => MyUtil}
import scala.xml._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.Parsers
import java.math.BigDecimal
import java.math.MathContext
import scala.collection.mutable._

object TestDataUtility {
	
 def nullify(key: Long): String = "null"
 def seq(key:Long): String = key.toString
 def ref(key:Long, min: Long, max:Long) = (min + Math.ceil(Math.random * (max - min))).toLong.toString
 def choice(key:Long, opts: List[String]):String = {
	 if(opts.size == 0) "" 
	 else if(opts.size == 1) opts.apply(0) 
	 else {
		 val help = Math.random * (opts.size - 1)
		 if(help.toFloat < 1/opts.size.toFloat) opts.apply(0) else opts.apply(Math.ceil(help).toInt)
	 }
 }
 
 def rnd(min: BigDecimal, max: BigDecimal, precision: Int, favourites: List[(Double, String)]): String = {
  def choose(reference: Double, l: List[(Double, String)]): (Boolean, String) = l match {
	  case Nil => (false, "")
	  case head :: tail => if(head._1 > reference) (true, head._2) else choose(reference, tail)
  }
  
  val firstChoice = choose(Math.random.toDouble, favourites)
  
  if(firstChoice._1) firstChoice._2 
  else {
	  val ctxt = new MathContext(precision)
	  val random = new BigDecimal(Math.random)
	  min.add(max.subtract(min).multiply(random)).round(ctxt).toPlainString
  }
 }
 /*
 def selection(key:Long, n: Node, identifier: String) = {
  val choice = n \\ identifier
  if(choice.isEmpty) ""
  else if(choice.size == 1) choice.apply(0)
  else {
	val help = Math.random * (choice.size - 1)
	if(help < 1/choice.size)  MyUtil.getSeqHeadText(choice.apply(0))
	else MyUtil.getSeqHeadText(choice.apply(Math.ceil(help).toInt))
  }
 }
 
 def selectionWithConstraint(key:Long, n: Node, identifier: String, constraint: String) = {
  val choice = (n \\ identifier).filter(aNode => MyUtil.getSeqHeadText(aNode \ "constraint") == constraint)
  if(choice.isEmpty) "" 
  else MyUtil.getSeqHeadText(choice.apply(Math.ceil(Math.random * (choice.size - 1)).toInt))
 }
 */
 def defineOperator(attrInformation: (String, Boolean, String), maxPk: Map[String, Int]) = {
  def r(key: Long) = ref(key, 1, 1000) //maxPk(attrInformation._3))
  def p(key: Long) = parsePattern(attrInformation._3)
  def c(key: Long) = choice(key, (attrInformation._3).split(";").toList)
  
  if(attrInformation._1 == "pk") seq _ // primary key
  else if(attrInformation._1 == "fk") r _ // foreign key
  else if(attrInformation._1 == "pattern") p _ // pattern
  else if(attrInformation._1 == "choice") c _ // choice of ;-separated strings
  // measure value
  // selection
  // selectionWithConstraint
  else nullify _ // otherwise set a null value
 }
 
 def parsePattern(pattern: String) = {
  val parser = new RegConstructor
  val refinedPattern = """\+""".r replaceAllIn("""\*""".r replaceAllIn(("""\?""".r replaceAllIn(pattern, "{0,1}")), "{0,5}"), "{1,5}")
  parser.parseAll(parser.symbolParser, refinedPattern) match {
	 case parser.Success(result, _) => result 
	 case parser.NoSuccess(msg, next) => nullify _
  }
 }
}

class RegConstructor extends JavaTokenParsers {
 
 val letters = List('a', 'b', 'c')
 
 def printSymbols(symbols: String): String = symbols
 def symbolParser: Parser[Any] = """[^\*\?\+\{\}]*""".r^^(x => printSymbols(x))
 
 def printDigit(): String = {
  val rnd = Math.random * 9
  if(rnd < 0.5) "0" else Math.ceil(rnd).toString
 }
 
 def digitParser: Parser[Any] = """\d""".r^^(x => printDigit())
 
 def printLetter(letters: List[Char]): String = if(letters.length == 0) "" else letters.apply(Math.ceil(Math.random * (letters.length - 1)).toInt).toString
 def letterParser: Parser[Any] = """\w""".r^^(x => printLetter(letters))
 
 def atomParser: Parser[Any] = (symbolParser | digitParser | letterParser)~rep(symbolParser | digitParser | letterParser)
 
 def repeat(text: String, lower: Int, upper: Int) = {
  def r(txt: String, n: Int): String = if(n == 0) "" else txt + r(txt, n - 1)
  
  val n = lower + Math.ceil((upper - lower) * Math.random).toInt
  r(text,n)
 }
 
 def cardinalityParser: Parser[Any] = atomParser~"""\{\d+,\d+\}""".r^^{case atom~"{"~from~","~to~"}" => repeat(atom.toString, from.toString.toInt, to.toString.toInt)}
 
 def regexParser: Parser[Any] = (atomParser|cardinalityParser)~rep(atomParser|cardinalityParser)^^{
	 case t1~t2 => t1 + ("" /: t2) (_ + _)
	 case t1 => t1
 }
}