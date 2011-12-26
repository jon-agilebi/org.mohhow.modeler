package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object UserGroup extends UserGroup with LongKeyedMetaMapper[UserGroup] {
   override def dbTableName = "USER_GROUP"
}

class UserGroup extends LongKeyedMapper[UserGroup] with IdPK {
    def getSingleton = UserGroup
	
 object groupId extends MappedLong(this) {
   override def dbColumnName = "GROUP_ID"
 }

	
 object shortName extends MappedPoliteString(this, 50) {
   override def dbColumnName = "SHORT_NAME"
 }

	
 object parentId extends MappedLong(this) {
   override def dbColumnName = "PARENT_ID"
 }

	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }

 def tr = {
				<tr><td>{groupId}</td><td>{shortName}</td><td>{parentId}</td><td>{dateCreated}</td></tr>
 }
     
}


