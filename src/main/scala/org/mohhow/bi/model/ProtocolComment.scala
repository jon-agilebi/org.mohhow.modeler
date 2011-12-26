package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ProtocolComment extends ProtocolComment with LongKeyedMetaMapper[ProtocolComment] {
   override def dbTableName = "PROTOCOL_COMMENT"
}

class ProtocolComment extends LongKeyedMapper[ProtocolComment] with IdPK {
    def getSingleton = ProtocolComment
	
 object fkProtocolItem extends MappedLongForeignKey(this, ProtocolItem) {
   override def dbColumnName = "FK_PROTOCOL_ITEM"
 }

 object fkUser extends MappedLongForeignKey(this, User) {
   override def dbColumnName = "FK_USER"
 }

 object comment extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "COMMENT"
 }
	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
  
}