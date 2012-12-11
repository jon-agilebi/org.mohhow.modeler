package org.mohhow.bi.lib

//import net.liftweb.json._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer._

object JsonUtility {
	
 def list2Json(l: List[String]): String = compact(render(l))	
 def map2Json(m: Map[String,String]): String = compact(render(m))

}