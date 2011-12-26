package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object UserRole extends UserRole with LongKeyedMetaMapper[UserRole] {
   override def dbTableName = "USER_ROLE"
}

class UserRole extends LongKeyedMapper[UserRole] with IdPK {
    def getSingleton = UserRole
	
 object roleId extends MappedLong(this) {
   override def dbColumnName = "ROLE_ID"
 }
	
 object roleName extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ROLE_NAME"
 }
	
 object parentId extends MappedLong(this) {
   override def dbColumnName = "PARENT_ID"
 }
	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
     
}