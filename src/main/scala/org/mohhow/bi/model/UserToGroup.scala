package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object UserToGroup extends UserToGroup with LongKeyedMetaMapper[UserToGroup] {
   override def dbTableName = "USER_TO_GROUP"
}

class UserToGroup extends LongKeyedMapper[UserToGroup] with IdPK {
    def getSingleton = UserToGroup
	
 object fkGroup extends MappedLongForeignKey(this, UserGroup) {
   override def dbColumnName = "FK_GROUP"
 }

	
 object userId extends MappedPoliteString(this, 50) {
   override def dbColumnName = "USER_ID"
 }

	
 object authorisation extends MappedPoliteString(this, 50) {
   override def dbColumnName = "AUTHORISATION"
 }

	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }


 def tr = {
				<tr><td>{fkGroup}</td><td>{userId}</td><td>{authorisation}</td><td>{dateCreated}</td></tr>
 }
     
}


