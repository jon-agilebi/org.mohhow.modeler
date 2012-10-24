package org.mohhow.bi.util

import net.liftweb._

import mapper._
import util._
import java.text.SimpleDateFormat
import java.util.Date
import java.text.ParsePosition
import scala.xml._
import org.mohhow.model._

object Utility {
	
 /*
  *  XML helper methods
  */
	
	
 /**
   * utility function used to convert a list of NodeSeq objects into a single NodeSeq object
   */
	
 def flattenNodeSeq(l: List[NodeSeq]) : NodeSeq = List.flatten(l.map(_.toList)).toSeq
 
 def toNode(seq: NodeSeq): Option[Node] = if(seq.length == 1) Some(seq(0)) else None
 
 def findNode(node: Node, path: List[(String,Option[(String, String)])]): Option[Node] =  path match  {
 
	 case List((tag, None)) => toNode(node \ tag)
	 case List((tag, Some((attr, attrValue)))) => {
		 for(child <- (node \ tag)) {
			 val pattern1 = "@" + attr
			 if((child \ pattern1).text == attrValue) return Some(child)
		 }
		 
		 None
	 }
	 case (tag, None) :: tail => toNode(node \ tag) match {
		 case Some(child) => findNode(child, tail)
		 case None => None
	 }
	 case (tag, Some((attr, attrValue))) :: tail => {
		 for(child <- (node \ tag)) {
			 val pattern2 = "@" + attr
			 if((child \ pattern2).text == attrValue) return findNode(child, tail)
		 }  
		 
		 None
	 }
	 case _ => None
 }
 
 def getNodeText(node: Node): String = node match {
  case Elem(_, _, _, _, Text(myText)) => myText
  case _ => ""
 }
 
 def getSeqHeadText(seq: NodeSeq) = if(seq.isEmpty) "" else getNodeText(seq(0))
 
 def makeUnique(text: String, list: List[String], n: Int): String = {
	if (!list.contains(text)) text 
	else {
	  val newText = text + "_" + n.toString
	  if (!list.contains(newText)) newText else makeUnique(text, list, n + 1)
	 }
 }
 
 def tagIt(text: String): NodeSeq = {
   val purgedText = """&gt;""".r replaceAllIn("""&lt;""".r replaceAllIn(text, "<"), ">")
   
   try {
	   XML.loadString("<span>" + purgedText + "</span>") \\ "span"
   }
   catch{
   		case e: Exception => {
	  	   println("Exception during parsing " + text)
	  	   <span>{text}</span>
   		}
   }
 }
	
/*
 * Methods for date formatting
 */
	
 def asDate(dayOfYear: String, timeInDay: String) : Date = {
  val day = new SimpleDateFormat("dd.MM.yyyy HH:mm")
  val pattern = dayOfYear + " " + timeInDay
  val pos = new ParsePosition (0)
  day.parse(pattern, pos)
 }
 
 def asDate(dayOfYear: String) : Date = {
  val day = new SimpleDateFormat("dd.MM.yyyy")
  val pos = new ParsePosition (0)
  day.parse(dayOfYear, pos)
 }
 
 def timeInDay(d : Date) : String = {
  if(d == null) "" else {
	val timeInDay = new SimpleDateFormat("HH:mm")
    timeInDay.format(d)
  }
 }
 
 def formatDate(d: Date) : String = {
  if(d == null) "" else {
   val dayInYear = new SimpleDateFormat("dd.MM.yyyy")
   dayInYear.format(d) 
  }
 }
 
 def dateFromNumber(n: Long) : Date = {
  if(n == 0) null 
  else {
	  val fromNumber = new SimpleDateFormat("yyyyMMdd")
	  val pos = new ParsePosition (0)
	  fromNumber.parse(n.toString, pos)
  }
 }
 
 def formatDate(d: Date, format: String) : String = {
  if(d == null) "" else {
   val sdf = new SimpleDateFormat(format)
   sdf.format(d) 
  }
 }
 
 def dateAsNumber(d: Date): Long = {
  if(d == null) 19000101
  else {
	  val dateNumberFormat = new SimpleDateFormat("yyyyMMdd")
	  dateNumberFormat.format(d).toLong
  }
 }
 
 def succDate(d: Long): Long = {
  val day = d % 100
  val m = (d/100) % 100
  val y = d/10000
  
  if (day < 28 || (day == 28 && m == 2 && y % 4 == 0) ) d + 1
  else if (day <= 29 && m == 2) y * 10000 + 301
  else if (day <= 29) d + 1
  else if (day == 30 && (m == 4 || m == 6 || m == 9 || m == 11)) y*10000 + (m+1) * 100 + 1
  else if (day ==30) d + 1
  else if (day == 31 && m == 12) (y + 1) * 10000 + 101
  else y * 10000 + (m+1)* 100 + 1
 }
 
 def duration(begin:Long, end:Long):Long = {
  def intervall(b: Long, e: Long, i: Long):Long = if(b >= e) i else intervall(succDate(b),e,i+1)
  
  if(begin > end) 0 else intervall(begin, end, 1)
 }
 
 def month(dateAsNumber: Int) = dateAsNumber/100 % 100 
 def year(dateAsNumber: Int) = dateAsNumber/10000

 def quarter(dateAsNumber: Int) = { 
  val m = month(dateAsNumber)  
  if(m % 3 == 0) m/3 else m/3 + 1 
 }

 def dayInMonth(dateAsNumber: Int) = dateAsNumber % 100


 def dayInWeek(dateAsNumber: Int) = { 
  val d = dayInMonth(dateAsNumber) 
  val mo = month(dateAsNumber)
  val m = if (mo < 3) 10 + mo else mo - 2 
  val theYear = year(dateAsNumber) 
  val y = if(mo < 3) (theYear - 1) % 100 else theYear % 100 
  val c = if(theYear % 400 == 0) (theYear - 100)/100 else theYear / 100 
  
  (d + Math.floor(2.6 * m - 0.2) + y + Math.floor(y.toLong/4) + Math.floor(c.toLong/4) - 2 * c) % 7 
}

def dayInYear(dateAsNumber: Int) = { 
 val y = year(dateAsNumber) 
 val m = month(dateAsNumber) 
 val d = dayInMonth(dateAsNumber) 
 val feb = if((y % 4) == 0 && (y % 400) != 0) 29 else 20

 m match { 
        case 1 => d 
        case 2 => d + 31 
        case 3 => d + 31 + feb 
        case 4 => d + 31 + feb + 31 
        case 5 => d + 31 + feb + 31 + 30 
        case 6 => d + 31 + feb + 31 + 30 + 31 
        case 7 => d + 31 + feb + 31 + 30 + 31 + 30 
        case 8 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 
        case 9 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 
        case 10 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 
        case 11 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 
        case 12 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30

 }      
}

def weekInYear(dateAsNumber: Int) = {
 val d = dayInYear(dateAsNumber) 
 val y = year(dateAsNumber) 
 val first = dayInWeek(y * 10000 + 101).toInt 
 if(first > 4) (d - first + 2) / 7 + 1 else  (d + first - 2) /  7 + 1
} 
 
 /**
  * generic methods for the user management
  * 
  */
 
 def getUser(userId: Long) = {
  val usrs = User.findAll(By(User.id, userId))
  if(usrs.isEmpty) null else usrs(0)
 }
 
 def getUserName(userId: Long) = {
  val usrs = User.findAll(By(User.id, userId))
  if(usrs.isEmpty) "" else usrs(0).firstName + " " + usrs(0).lastName 
 }
 
 def findRoles (scenarioId: Long, roleName: String): List[User] = {
  def getUser(sr: ScenarioRole) = {
	  val usrs = User.findAll(By(User.id, sr.fkUser))
	  if(usrs.isEmpty) null else usrs(0)
  }
  ScenarioRole.findAll(By(ScenarioRole.fkScenario, scenarioId), By(ScenarioRole.role, roleName)).map(sr => getUser(sr)).filter(_ != null).toList
 }
 
 def filterScenarioRoles(soFar: List[ScenarioRole], remaining: List[ScenarioRole]): List[ScenarioRole] = remaining match {
  case Nil => soFar
  case head :: tail => if(soFar.map(_.fkUser).contains(head.fkUser)) filterScenarioRoles(soFar, tail) else filterScenarioRoles(head :: soFar, tail)
 }
 
 /**
  * Helper methods for dimensional modelling
  * 
  */
 
 def isConnected(v1: Long, v2: Long) = !ModelEdge.findAll(By(ModelEdge.head, v1), By(ModelEdge.tail, v2)).isEmpty || !ModelEdge.findAll(By(ModelEdge.head, v2), By(ModelEdge.tail, v1)).isEmpty
  
 def makeSemicolonList(items: List[String]): String = items match {
	case Nil => ""
	case head :: tail => (head /:  tail) (_ + ";" + _)
 }
 
 def makeSeparatedList(items: List[String], separator: String): String = items match {
	case Nil => ""
	case head :: tail => (head /:  tail) (_ + separator + _)
 }
 
 val tableTypes = List("dimension", "dateDimension", "measureDimension", "level", "fact", "snapshotFact", "transactionFact", "accountFact", "accountSnapshotFact", "accountTransactionFact")
	
 /*
  * LDAP Utilities
  */
 
 def createConfiguration(p: Provider): Map[String, String] = {
	 Map("ldap.url" -> p.url.toString, 
		 "ldap.base" -> p.base.toString, 
		 "ldap.userName" -> p.userName.toString, 
		 "ldap.password" -> p.pwd.toString, 
		 "ldap.authType" -> p.authType.toString,
		 "ldap.initial_context_factory" -> p.initialContextFactory.toString,
		 "ldap.testLookup" -> p.testLookup.toString,
		 "ldap.retryInterval" -> p.retryIntervall.toString,
	 	 "ldap.maxRetries" -> p.maxRetries.toString)
 }
 
 def attr2List(attrs: javax.naming.directory.Attributes, memberAttribute: String, displayAttribute: String): List[String] = {
   val iterator = attrs.getIDs()
   getDisplayList(attrs.get(memberAttribute).toString, displayAttribute)
  }
  
  def rOfEqual(eqn: String) = {
	 val twoTerms= eqn.split("=").toList
	 if(twoTerms.size == 2) twoTerms(1) else eqn
  }
 
  def getDisplayList(attr: String, displayAttribute: String): List[String] = {
 
   val pair = attr.split(":").toList
   if(pair.size == 2) pair(1).split(",").toList.map(_.trim).filter(_.startsWith(displayAttribute)).map(rOfEqual).toList  
   else Nil
  }
  
  def getDisplayItem(attr: String, displayAttribute: String) = {
   val l = attr.split(",").toList.map(_.trim).filter(_.startsWith(displayAttribute)).map(rOfEqual)
   if(l.isEmpty) attr else l(0)
  }
  
  def prettyTerm(text: String, isFilter: Boolean): String = {
   if(text== null || text.length == 0) text
   else {
	  
	  
	  val pattern = """<(d|m)\d+>""".r
	  val firstMatch = pattern findFirstIn text
	  
	  
	  firstMatch match {
	 	case Some(content) => {
	 	
		 	if(content.substring(1,2) == "m") {
		 		val bracket = if(isFilter) "*" else "$"
		 		val msrs = Measure.findAll(By(Measure.id, content.substring(2,content.length - 1).toLong))
		 		val replaceMsr = (pattern replaceFirstIn(text, bracket + msrs.apply(0).shortName + bracket)).toString
		 		if(!msrs.isEmpty) prettyTerm(replaceMsr, isFilter)
		 		else text 
		 	}
		 	else {
		 		val openBracket = if(isFilter) "*" else "["
		 		val closingBracket = if(isFilter) "*" else "]"
		 		val elms = ModelVertex.findAll(By(ModelVertex.id, content.substring(2,content.length - 1).toLong))
		 		if(!elms.isEmpty) prettyTerm(pattern replaceFirstIn(text, openBracket + elms.apply(0).elementName  + closingBracket), isFilter)
		 		else text
		 	}
	 	}
	 	case _ => text
	 	 
	  }
   }
  }
}