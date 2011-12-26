package org.mohhow.bi.lib

import net.liftweb._
import common._
import http._
import rest._
import scala.xml._
import java.lang.Math._;

object BIService extends RestHelper{
	
 var isInitialized: Boolean = false 
 
 def initializeBIService() {
	 
  // lade user.xml, stores.xml und metadata.xml
  	  
  isInitialized = true
 }
 
 def isActual(checksum: String): Boolean = {
  true
 }
 
 def query(blockIds: List[Long]): List[NodeSeq] = {
  // db.runQuery ausfuehren
 // Formel in xml uebertragen
	 
  List()
 }
 
 def sendMetaData() = {
	 <metadata />
 }
 
 def createResponse(answers: List[NodeSeq], mode: String): Node = {
	 
	 
  <data>
	 <block id="1" type="twoNumbers">
	 	<name em="N">A</name><last>1</last><name em="Y">B</name><last>1</last><name em="N">C</name><last>3</last>
	 </block>
	 <block id="2" type="twoNumbers">
	 	<name em="N">A</name><last>1</last><name em="Y">B</name><last>1</last><name em="N">C</name><last>3</last>
	 </block>
	 <block id="3" type="twoNumbers">
	 	<name em="N">A</name><last>{Math.Pi}</last><name em="Y">B</name><last>{Math.Pi/2}</last><name em="N">C</name><last>{Math.Pi/2}</last>
	 </block>
	 <block id="4" type="twoNumbers">
	 	<name em="N">A</name><last>{Math.Pi}</last><name em="Y">B</name><last>{Math.Pi/2}</last><name em="N">C</name><last>{Math.Pi/2}</last>
	 </block>
	 <block id="5" type="twoNumbers">
	 	<name em="N">A</name><last>{Math.Pi}</last><name em="Y">B</name><last>{Math.Pi/2}</last><name em="N">C</name><last>{Math.Pi/2}</last>
	 </block>
	 <block id="6" type="number">-2</block>
	 <block id="7" type="number">1</block>
	 <block id="8" type="number">0</block>
	 <block id="9" type="number">-1</block>
	 <block id="10" type="number">2</block>
	 <block id="11" type="twoNumbers">
	 	<name em="N">A</name><last>1</last><name em="Y">B</name><last>1</last><name em="N">C</name><last>3</last>
	 </block>
	 <block id="12" type="twoNumbers">
	 	<name em="N">A</name><last>1</last><name em="Y">B</name><last>1</last><name em="N">C</name><last>3</last>
	 </block>
	 <block id="13" type="twoNumbers">
	 	<name em="N">A</name><last>1</last><name em="Y">B</name><last>1</last><name em="N">C</name><last>3</last>
	 </block>
  </data>
	 
 }
 
 serve {
	 
  case "blocks" :: mode :: checksum :: blocks :: Nil XmlGet _ => {
   if(!isInitialized) initializeBIService()
		 
   if(isActual(checksum)) {
	val blockIds = blocks.substring(1).split("b").map(_.toLong).toList
	val answers = query(blockIds)
	createResponse(answers, mode)		 
   }
   else sendMetaData()
  }
  
  case "ping" :: Nil XmlGet _ => <answer>OK</answer>
 }
}