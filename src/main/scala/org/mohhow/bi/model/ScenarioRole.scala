package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._
import scala.xml._

object ScenarioRole extends ScenarioRole with LongKeyedMetaMapper[ScenarioRole] {
   override def dbTableName = "SCENARIO_ROLE"
}

class ScenarioRole extends LongKeyedMapper[ScenarioRole] with IdPK {
    def getSingleton = ScenarioRole
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }

 object fkUser extends MappedLongForeignKey(this, User) {
   override def dbColumnName = "FK_USER"
 }
	
 object role extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ROLE"
 }
	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }     
}