package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ProductBacklog extends ProductBacklog with LongKeyedMetaMapper[ProductBacklog] {
   override def dbTableName = "PRODUCT_BACKLOG"
}

class ProductBacklog extends LongKeyedMapper[ProductBacklog] with IdPK {
    def getSingleton = ProductBacklog
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }

	
 object description extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "DESCRIPTION"
 }

	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }


 def tr = <tr><td>{fkScenario}</td><td>{description}</td><td>{dateCreated}</td></tr>
     
}