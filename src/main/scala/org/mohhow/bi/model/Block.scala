package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Block extends Block with LongKeyedMetaMapper[Block] {
   override def dbTableName = "BLOCK"
}

class Block extends LongKeyedMapper[Block] with IdPK {
    def getSingleton = Block
	
 object fkSpecification extends MappedLongForeignKey(this, Specification) {
   override def dbColumnName = "FK_SPECIFICATION"
 }
	
 object name extends MappedPoliteString(this, 50) {
   override def dbColumnName = "NAME"
 }

 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }     
}