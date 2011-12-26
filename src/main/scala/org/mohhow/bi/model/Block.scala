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

	
 object representationType extends MappedPoliteString(this, 50) {
   override def dbColumnName = "REPRESENTATION_TYPE"
 }

	
 object validFrom extends MappedDateTime(this) {
   override def dbColumnName = "VALID_FROM"
 }

	
 object validUntil extends MappedDateTime(this) {
   override def dbColumnName = "VALID_UNTIL"
 }

	
 object isCurrent extends MappedLong(this) {
   override def dbColumnName = "IS_CURRENT"
 }


 def tr = {
				<tr><td>{fkSpecification}</td><td>{name}</td><td>{representationType}</td><td>{validFrom}</td><td>{validUntil}</td><td>{isCurrent}</td></tr>
 }
     
}


