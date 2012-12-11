package org.mohhow.bi.lib

import net.liftweb._

import common._
import http._
import rest._
import scala.xml._
import scala.actors._
import java.lang.Math._
import scala.collection.mutable

import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.{Repository => MyRep}

object DeploymentService extends RestHelper {

 var state = "waiting"
 val deploymentItems = mutable.Map.empty[String, Int]
 
 def stopDeployment() = {
  state = "waiting"
  MyRep.emptyTransfer()
 }
 
 def check(name: String, xml:Node): Boolean = {
  if(xml == null || !deploymentItems.contains(name) || xml.hashCode != deploymentItems(name)) {
	stopDeployment()
	false
  }
  else true
 }
 
 serve {
	 
  case "deployment" :: "start" :: _ XmlPost xml -> _ => {
   println("starte deployment")	  
   deploymentItems.clear
   
   for(item <- xml \\ "item") {
	val itemName = MyUtil.getSeqHeadText(item \\ "name")
	val itemChecksum = MyUtil.getSeqHeadText(item \\ "checksum").toInt
	deploymentItems += (itemName -> itemChecksum)
   }
   
   if(!deploymentItems.isEmpty) {
	state = "deploying"
	MyRep.emptyTransfer()
	
	<deploying />
   }
   else <nothingToDeploy />
   
  }
  
  case "deployment" :: "transfer" :: kind :: name :: mode :: _ XmlPost xml -> _  => { 
   if(state == "deploying" && check(name, xml)) {
	   Repository.write("deployment", 0, kind, name, 0, xml)
	   deploymentItems -= name
	   
	   if(deploymentItems.isEmpty) {
	  	   state = "waiting"
	  	   if(mode == "complete") MyRep.deploy(true) else MyRep.deploy(false)
	  	   BIService.isInitialized = false
	  	   <deployed />
	   }
	   else  <transferred />
   }
   else <badData />
  }
 } 
}