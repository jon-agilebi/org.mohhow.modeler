package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object UserRole extends UserRole with LongKeyedMetaMapper[UserRole] {
 override def dbTableName = "USER_ROLE"
}

class UserRole extends LongKeyedMapper[UserRole] with IdPK with ManyToMany {
 def getSingleton = UserRole
 
 object roleName extends MappedPoliteString(this, 100) {
  override def dbColumnName = "ROLE_NAME"
 }
	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 } 
 
 object specs extends MappedManyToMany(SpecificationToRole, SpecificationToRole.fkRole, SpecificationToRole.fkSpecification, Specification)
}