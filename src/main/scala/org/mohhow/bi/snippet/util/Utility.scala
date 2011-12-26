package org.mohhow.bi.util

import util._
import java.text.SimpleDateFormat
import java.util.Date
import java.text.ParsePosition
import scala.xml._

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
 
 def replaceNode(complete: Node, coordinates: (String,Option[(String, String)]), replacement: Node): Node = complete match {
  case Elem(prefix, label, attributes, scope, childNodes) => {
	  coordinates match {
	 	  case (label, None) => replacement
	 	  case(label, Some((attrName, attrValue))) => {
	 	 	 val attr = "@" + attrName
	 	 	 if((complete \\ attr ).text == attrValue) replacement else replacement
	 	  }
//	 	  case _ => Elem(prefix, label, attributes, scope, childNodes.map(n => replaceNode(n, coordinates, replacement)))
	  } 
  }
 }
 
 def getNodeText(node: Node): String = node match {
  case Elem(_, _, _, _, Text(myText)) => myText
  case _ => ""
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
 
}