package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object SpecificationToRole extends SpecificationToRole with LongKeyedMetaMapper[SpecificationToRole] {
   override def dbTableName = "SPECIFICATION_TO_ROLE"
}

class SpecificationToRole extends LongKeyedMapper[SpecificationToRole] with IdPK {
    def getSingleton = SpecificationToRole
	
 object fkSpecification extends MappedLongForeignKey(this, Specification) {
   override def dbColumnName = "FK_SPECIFICATION"
 }

 object fkRole extends MappedLongForeignKey(this, UserRole) {
   override def dbColumnName = "FK_ROLE"
 }
	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
 
 def getRoleName = UserRole.findAll(By(UserRole.id, fkRole))(0).roleName
}