package org.mohhow.snippet

import net.liftweb._
import http._
import util._

import mapper._
import util._
import Helpers._

import org.mohhow.model._
import java.util.Date
import org.mohhow.bi.lib.Repository
import org.mohhow.bi.util.{Utility => MyUtil}

object TableCopyForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
	
 val tableName = field(S.?("name"), "", valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val tableType = select(S.?("tableType"), "", MyUtil.tableTypes.map(aType => S.?(aType)))
 val surrogateKey = field(S.?("surrogateKey"), false)
 val fks = field(S.?("noReferenceDim"), false)
 val description = textarea(S.?("description"), "", "class" -> "termEditor")
 
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def tableTypeKey(display: String) = {
	 val tableTypes = MyUtil.tableTypes.filter(tt => S.?(tt) == display)
	 if(tableTypes.isEmpty) "" else tableTypes(0)
 }
 
 def copyAttr(attr: PAttribute, t: PTable) = {
	 val newAttr = PAttribute.create
	 newAttr.fkPTable(t.id).name(attr.name).dataType(attr.dataType).length(attr.length).scale(attr.scale).isNotNullable(attr.isNotNullable).comment(attr.comment).isPartOfUniqueKey(attr.isPartOfUniqueKey).validFrom(new Date).isCurrent(1).dependsOn(attr.id).save
 }
 
 def finish() {
	 val original = SelectedTable.is
	 val newTable = PTable.create
	 newTable.fkScenario(original.fkScenario).name(tableName).description(description).tableType(tableTypeKey(tableType)).isDerivedFromModel(0).validFrom(new Date).isCurrent(1).dependsOn(original.id).save
	 
	 // take all attributes of the original table but not the original primary key
	 
	 val attrs = PAttribute.findAll(By(PAttribute.fkPTable, original.id), By(PAttribute.isCurrent, 1)).filter(attr => attr.isPrimaryKey != 1 && attr.fkModelAttribute != -1)
	 
	 // remove foreign keys if wanted
	 
	 val attrsProcessedFk = if(fks) attrs.filter(_.reference == 0) else attrs
	 
	 // add a surrogate primary key if wanted
	 
	 if(surrogateKey) {
		 val pkAttribute = PAttribute.create
		 pkAttribute.fkPTable(newTable).name(MyUtil.nameFromPattern(newTable.name, "pk")).validFrom(new Date).isCurrent(1).isDerivedFromModel(2).fkModelAttribute(0).isPrimaryKey(1)
		 MyUtil.setAttributePhysicalType(MyUtil.getSeqHeadText(Setup.is \\ "defaultKeyType"), pkAttribute).save 
	 }
	 
	 attrsProcessedFk.map(attr => copyAttr(attr, newTable))
	 
	 // add metadata
	 
	 MyUtil.addAdditionalAttributes(newTable.tableType, newTable)
 }	 
}