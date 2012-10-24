package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import org.mohhow.model._
import java.util.Date
import org.mohhow.bi.util._
import java.util.regex.Pattern

object ClientForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
 
 val shortName = field(S.?("shortName"), nvl(SelectedClient.is.shortName.toString), valMinLen(1, S.?("textToShort")), valMaxLen(50, S.?("textToLong")), uniqueClientName _)
 val longName = field(S.?("longName"), nvl(SelectedClient.is.longName.toString), valMinLen(1, S.?("textToShort")), valMaxLen(200, S.?("textToLong")))

 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def uniqueClientName(clientName: String): List[FieldError] = {
  if(Client.findAll().exists(cl => cl.shortName == clientName && cl.id != SelectedClient.is.id)) S.?("clientAlreadyExists") else Nil 
 }
 
 def finish() {
  val client = SelectedClient.is 
  client.shortName(shortName).longName(longName).dateCreated(new Date).save
  S.redirectTo("/clientAdministration")
 }
}
