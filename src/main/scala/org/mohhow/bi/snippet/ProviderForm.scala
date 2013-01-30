package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import util.Helpers._
import common._
import org.mohhow.model._
import java.util.Date
import java.util.regex.Pattern

object ProviderForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
 
 val friendlyName = field(S.?("friendlyName"), nvl(ProviderToEdit.is.friendlyName.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val url = field(S.?("url"), nvl(ProviderToEdit.is.url.toString), valMinLen(1, S.?("textToShort")), valMaxLen(500, S.?("textToLong")))
 val base = field(S.?("base"), nvl(ProviderToEdit.is.base.toString), valMinLen(1, S.?("textToShort")), valMaxLen(500, S.?("textToLong")))
 val userName = field(S.?("user"), nvl(ProviderToEdit.is.userName.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val pwd = password(S.?("password"), nvl(ProviderToEdit.is.pwd.toString), valMinLen(6, S.?("textToShort")), valMaxLen(20, S.?("textToLong")))
 val authType = field(S.?("authenticationType"), nvl(ProviderToEdit.is.authType.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val initialContextFactory = field(S.?("initialContextFactory"), nvl(ProviderToEdit.is.initialContextFactory.toString), valMaxLen(500, S.?("textToLong")))
 val testLookup = field(S.?("testLookup"), nvl(ProviderToEdit.is.testLookup.toString), valMaxLen(500, S.?("textToLong")))
 val retryIntervall = field(S.?("retryIntervall"), nvl(ProviderToEdit.is.retryIntervall.toString), valMaxLen(10, S.?("textToLong")))
 val maxRetries = field(S.?("maximalRetries"), nvl(ProviderToEdit.is.maxRetries.toString))
 val searchTerm = field(S.?("searchTerm"), nvl(ProviderToEdit.is.searchTerm.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val memberAttribute = field(S.?("memberAttribute"), nvl(ProviderToEdit.is.memberAttribute.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val displayAttribute = field(S.?("displayAttribute"), nvl(ProviderToEdit.is.displayAttribute.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val bindPattern = field(S.?("bindPattern"), nvl(ProviderToEdit.is.bindPattern.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def finish() {
	 asLong(maxRetries.is) match {
		 case Full(mr) => {
			 val provider = ProviderToEdit.is
			 provider.friendlyName(friendlyName).url(url).base(base).userName(userName).pwd(pwd).authType(authType).initialContextFactory(initialContextFactory).testLookup(testLookup).retryIntervall(retryIntervall).maxRetries(mr).searchTerm(searchTerm).memberAttribute(memberAttribute).displayAttribute(displayAttribute).bindPattern(bindPattern).save
			 SelectedProvider(Some(provider))
			 S.redirectTo("/user") 
		 }
		 
		 case _ => S.error(S.?("notAnInteger"));
	 } 
  }
}