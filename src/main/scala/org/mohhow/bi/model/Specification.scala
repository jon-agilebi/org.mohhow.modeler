package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Specification extends Specification with LongKeyedMetaMapper[Specification] {
   override def dbTableName = "SPECIFICATION"
}

class Specification extends LongKeyedMapper[Specification] with IdPK  {
    def getSingleton = Specification
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }
    
 object fkFeature extends MappedLongForeignKey(this, Feature) {
   override def dbColumnName = "FK_FEATURE"
 }
    
 object status extends MappedPoliteString(this, 50) {
   override def dbColumnName = "STATUS"
 }
	
 object name extends MappedPoliteString(this, 50) {
   override def dbColumnName = "NAME"
 }
 
 object implementationType extends MappedPoliteString(this, 50) {
   override def dbColumnName = "IMPLEMENTATION_TYPE"
 }

 object description extends MappedPoliteString(this, 500) {
   override def dbColumnName = "DESCRIPTION"
 }

 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
 
 def findRoles = {
  def getRole(sTr: SpecificationToRole) = UserRole.findAll(By(UserRole.id, sTr.fkRole))

  List.flatten(SpecificationToRole.findAll(By(SpecificationToRole.fkSpecification, id)).map(getRole))
 }
 
}