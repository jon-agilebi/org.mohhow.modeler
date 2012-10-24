package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object RoleToGroup extends RoleToGroup with LongKeyedMetaMapper[RoleToGroup] {
   override def dbTableName = "ROLE_TO_GROUP"
}

class RoleToGroup extends LongKeyedMapper[RoleToGroup] with IdPK {
  def getSingleton = RoleToGroup
	
  object fkProvider extends MappedLongForeignKey(this, Provider) {
   override def dbColumnName = "FK_PROVIDER"
  }
  
  object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
  }
    
  object fkRole extends MappedLongForeignKey(this, UserRole) {
   override def dbColumnName = "FK_ROLE"
  }
	
  object dn extends MappedPoliteString(this, 500) {
   override def dbColumnName = "DN"
  }
	
  object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
  }     
}