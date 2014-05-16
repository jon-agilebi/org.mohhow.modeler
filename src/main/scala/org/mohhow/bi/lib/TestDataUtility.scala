package org.mohhow.bi.lib

import org.mohhow.bi.util.{Utility => MyUtil}
import scala.xml._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.Parsers
import java.math.BigDecimal
import java.math.MathContext
import scala.collection.mutable._

object TestDataUtility {
	
 val dbs = Repository.read("configuration", 0, "metadata", "conf_db", -1) \\ "database"
 
 def nullify(key: Long): String = "null"
 def seq(key:Long): String = key.toString
 def ref(key:Long, min: Long, max:Long) = (min + Math.ceil(Math.random * (max - min))).toLong.toString
 def refDate(key:Long, timeSpan: List[Long]) = timeSpan(Math.floor(Math.random * timeSpan.size).toInt)
 def choice(key:Long, opts: List[String]):String = {
	 if(opts.size == 0) "" 
	 else if(opts.size == 1) opts.apply(0) 
	 else {
		 val help = Math.random * (opts.size - 1)
		 if(help.toFloat < 1/opts.size.toFloat) opts.apply(0) else opts.apply(Math.ceil(help).toInt)
	 }
 }
 
 def rnd(min: BigDecimal, max: BigDecimal, favourites: List[(Double, String)]): String = {
  def choose(reference: Double, l: List[(Double, String)]): (Boolean, String) = l match {
	  case Nil => (false, "")
	  case head :: tail => if(head._1 > reference) (true, head._2) else choose(reference, tail)
  }
  
  val firstChoice = choose(Math.random.toDouble * 100, favourites)
  
  if(firstChoice._1) firstChoice._2 
  else {
	  val ctxt = new MathContext(min.precision())
	  val random = new BigDecimal(Math.random)
	  min.add(max.subtract(min).multiply(random)).round(ctxt).toPlainString
  }
 }
 
 def toRnd(key: Long, information: String) = {
  def extract(l: List[String]): List[(Double, String)] = l match {
	  case Nil => Nil
	  case first :: second :: tail => (first.toDouble, second) :: extract(tail)
	  case List(singleton) => Nil // shall not happen
  }
  
  try {
  
	  val parts = information.split(";")
	  if(parts.size > 0) {
		  val left = parts(0).split(",")
		  val right = if(parts.size == 2) extract(parts(1).split(",").toList) else Nil
		  rnd(new BigDecimal(left(0)), new BigDecimal(left(1)), right)
	  }
  else "0"
  }
  catch {
	  case e: Exception => {
	 	  println("The information was " + information)
	 	  "0"
	  }
  }
 }
 
 def time(key: Long, pattern: String) = {
  if(pattern == "year") MyUtil.year(key.toInt).toString
  else if(pattern == "quarter") MyUtil.quarter(key.toInt).toString
  else if(pattern == "month") MyUtil.month(key.toInt).toString
  else if(pattern == "dayInMonth") MyUtil.dayInMonth(key.toInt).toString
  else if(pattern == "dayInWeek") MyUtil.dayInWeek(key.toInt).toString
  else if(pattern == "dayInYear") MyUtil.dayInYear(key.toInt).toString
  else if(pattern == "calendarWeek") MyUtil.weekInYear(key.toInt).toString
  else {
	  try MyUtil.formatDate(MyUtil.dateFromNumber(key.toInt), pattern)
	  catch {
	 	  case ex: Exception => MyUtil.formatDate(MyUtil.dateFromNumber(key.toInt), "dd.MM.yyyy")
	  }
  }
 }
 
 def defineOperator(attrInformation: (String, Boolean, String), measureNames: List[String], upperBounds: Map[String, Int], sysdatePattern: String, initialLoad: Int) = {
  
  def r(key: Long) = ref(key, 1, upperBounds(attrInformation._3) - 1)
  def rDate(key: Long) = refDate(key,  MyUtil.allDays(initialLoad, upperBounds(attrInformation._3) - 1, Nil))
  def p(key: Long) = regex(attrInformation._3)
  def c(key: Long) = choice(key, (attrInformation._3).split(";").toList)
  def t(key: Long) = time(key, attrInformation._3)
  def rn(key: Long) = toRnd(key, attrInformation._3)
  def m(key: Long) = if(key.toInt >= 0 && key.toInt < measureNames.size) measureNames(key.toInt) else ""
  def meta(key: Long) = if(attrInformation._3 == "NUMBER") "1" 
	  					else if(attrInformation._3 == "DATE") sysdatePattern
	  					else "undefined"
 
  if(attrInformation._1 == "pk") seq _ // primary key
  else if(attrInformation._1 == "fk") r _ // foreign key
  else if(attrInformation._1 == "fkDate") rDate _ // foreign key to date dimension
  else if(attrInformation._1 == "pattern") p _ // pattern
  else if(attrInformation._1 == "choice") c _ // choice of ;-separated strings
  else if(attrInformation._1 == "time") t _ // time pattern
  else if(attrInformation._1 == "rnd") rn _ // rnd function for measure
  else if(attrInformation._1 == "measure") m _ // measure name for measure dimension
  else if(attrInformation._1 == "metadata") meta _ // meta data
  else nullify _ // otherwise set a null value
 }
 
 def parsePattern(item: (String, String)) = {
  val parser = new RegConstructor
  val response = if(item._2 == "plain") parser.parseAll(parser.plain, item._1)
  				 else if(item._2 == "bracket") parser.parseAll(parser.brackets, item._1)
  				 else if(item._2 == "cardinal") parser.parseAll(parser.cardinalityPlain, item._1)
  				 else parser.parseAll(parser.cardinalityBracket, item._1)
  
  
  response match {
	 case parser.Success(result, _) => result.toString 
	 case parser.NoSuccess(msg, next) => "llun"
  }
 }
 
 def replacePattern(text: String) = {
	 
	 def repl(item: (String, String)): String = {
		 
		 val parser = new RegConstructor
		 
		 val response = if(item._2 == "digit") parser.parseAll(parser.digitParser, item._1)
  				 else if(item._2 == "letter") parser.parseAll(parser.letterParser, item._1)
  				 else parser.parseAll(parser.plain, item._1)
  				 
  		 response match {
  		  	case parser.Success(result, _) => result.toString 
  		  	case parser.NoSuccess(msg, next) => text
  		 }
	 }
	 
	 val allToken = tokenizerRepl(text, Nil, "")
	 val tokenAndType = allToken.map(t => (t, typeTokenRepl(t)))

	 ("" /: tokenAndType.map(repl)) (_ + _)
 }
 
 def tokenizer(remainder: String, alreadyParsed: List[String], actualToken: String): List[String] = remainder match {
	 case "" => alreadyParsed
	 case w => {
		 if(w.length == 1) alreadyParsed ::: List(actualToken + w)
		 else {
			val w0 = w.substring(0,1)
			val w_rem = w.substring(1)
			
			if(w0 == "}" && actualToken.length > 0) tokenizer(w_rem, alreadyParsed ::: List(actualToken), "}")
			else if(w0 == "}") tokenizer(w_rem, alreadyParsed, "}")
			else if(w0 == "(") tokenizer(w_rem,  alreadyParsed ::: List(actualToken + w0), "")
			else tokenizer(w_rem, alreadyParsed, actualToken + w0)
		 }
	 }
 }
 
 def typeToken(token: String) = {
	 if(token.containsSlice("{)")) "bracketCardinal"
	 else if(token.contains('{')) "cardinal"
	 else if(token.contains('(')) "bracket"
	 else "plain"
 }
 
 def tokenizerRepl(remainder: String, alreadyParsed: List[String], actualToken: String): List[String] = remainder match {
	 case "" => alreadyParsed
	 case w => {
		 if(w.length == 1) alreadyParsed ::: List(actualToken + w)
		 else {
			val w0 = w.substring(0,1)
			val w1 = w.substring(1,2)
			val w_rem = w.substring(2)
			
			if(w0 == "d" && w1 == "\\" && actualToken.length > 0) tokenizerRepl(w_rem, alreadyParsed ::: List(actualToken, "d\\"), "")
			else if(w0 == "d" && w1 == "\\") tokenizerRepl(w_rem, alreadyParsed ::: List("d\\"), "")
			else if(w0 == "w" && w1 == "\\" && actualToken.length > 0) tokenizerRepl(w_rem, alreadyParsed ::: List(actualToken, "w\\"), "")
			else if(w0 == "w" && w1 == "\\") tokenizerRepl(w_rem, alreadyParsed ::: List("w\\"), "")
			else tokenizerRepl(w.substring(1), alreadyParsed, actualToken + w0)
		 }
	 }
 }
 
 def typeTokenRepl(token: String): String = {
  if(token == "d\\") "digit"
  else if(token == "w\\") "letter"
  else "plain"
 }
 
 def regex(pattern: String) = replacePattern(("" /: tokenizer(refinePattern(pattern).reverse, Nil, "").map(t => (t, typeToken(t))).map(parsePattern)) (_ + _)).reverse
 
 def refinePattern(pattern: String) = {
	 """\+""".r replaceAllIn("""\*""".r replaceAllIn(("""\?""".r replaceAllIn(pattern, "{0,1}")), "{0,5}"), "{1,5}")
 }
}
class RegConstructor extends JavaTokenParsers {
	
 def letters(word: String): List[String] = word match {
	 case "" => Nil
	 case w => w.substring(0,1) :: letters(w.substring(1))
 }	
 
 def repeat(text: List[String], lower: Int, upper: Int) = {
  def r(txt: String, n: Int): String = if(n == 0) "" else txt + r(txt, n - 1)
  
  if(text.length > 0) {
	  val n = lower + Math.ceil((upper - lower) * Math.random).toInt
	  r(text.head,n) + (("" /: text.tail) (_ + _))
  }
  else ""
  
 }
 
 def plain: Parser[Any] = """[^\(\)\{\}]*""".r^^{case text => text.toString}
 def plainAsList: Parser[List[String]] = """[^\(\)\{\}]*""".r^^{case text => letters(text)}
 def brackets: Parser[List[String]] = ")"~plain~"("^^{case ")"~text~"(" => List(text.toString)}
 def atom: Parser[Any] = cardinalityBracket | plain
 
 def naturalNumber: Parser[Any] = """\d+""".r
 
 def cardinalityPlain: Parser[Any] = "}"~naturalNumber~","~naturalNumber~"{"~plainAsList^^{case "}"~to~","~from~"{"~text => repeat(text, from.toString.toInt, to.toString.toInt)}
 def cardinalityBracket: Parser[Any] = "}"~naturalNumber~","~naturalNumber~"{"~brackets^^{case "}"~to~","~from~"{"~text => repeat(text, from.toString.toInt, to.toString.toInt)}
 
 // construct a single digit	
	
 def printDigit(): String = {
  val rnd = Math.random * 9
  if(rnd < 0.5) "0" else Math.ceil(rnd).toInt.toString
 }
 
 def digitParser: Parser[Any] = """d\\""".r^^(x => printDigit())	
 
 val smallLetters = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
 val letters = smallLetters ::: smallLetters.map(_.toUpperCase)
 
 def printLetter(letters: List[String]): String = if(letters.length == 0) "" else letters.apply(Math.ceil(Math.random * (letters.length - 1)).toInt)
 def letterParser: Parser[Any] = """w\\""".r^^(x => printLetter(letters))
 
}