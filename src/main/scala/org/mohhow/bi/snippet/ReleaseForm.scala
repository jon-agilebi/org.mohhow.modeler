package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import util.Helpers._
import common._
import org.mohhow.model._
import java.util.Date
import java.util.regex.Pattern
import org.mohhow.bi.util.{Utility => MyUtil}
import scala.xml._
import mapper._

object ReleaseForm extends LiftScreen {
	
 val kind = select(S.?("releaseKind"), "", List(S.?("developmentSnapshot"), S.?("showcase"), S.?("productivity")))
 val impact = select(S.?("impact"), "", List(S.?("majorRelease"), S.?("minorRelease"), S.?("patch")))
 
 val dateField = new Field { 
    type ValueType = String 
    override def name = S.?("scheduledEnd") 
    lazy val manifest = buildIt[String]
    override def default = MyUtil.formatDate(SelectedRelease.is.scheduledEnd)
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
 }  
 
 val scheduledEnd = dateField
 
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def finish() {
	 
  val release = SelectedRelease.is	 
  release.kind(kind).scheduledEnd(MyUtil.asDate(scheduledEnd)).status("active")	
  val releases = Release.findAll(By(Release.fkScenario, SelectedScenario.is), OrderBy(Release.id, Descending))
  
  if(releases.isEmpty) {
	  
	if (impact.toString == S.?("majorRelease")) {
		release.majorRelease(1)
		release.minorRelease(0)
		release.patch(0)
	} 
	else if (impact.toString == S.?("minorRelease")) {
		release.majorRelease(0)
		release.minorRelease(1)
		release.patch(0)
	}
	else {
		release.majorRelease(0)
		release.minorRelease(0)
		release.patch(1)
	}	  
  }
  else {
	val oldRelease = releases.apply(0)
	if (impact.toString == S.?("majorRelease")) {
		release.majorRelease(oldRelease.majorRelease.toLong + 1)
		release.minorRelease(0)
		release.patch(0)
	} 
	else if (impact.toString == S.?("minorRelease")) {
		release.majorRelease(oldRelease.majorRelease)
		release.minorRelease(oldRelease.minorRelease.toLong + 1)
		release.patch(0)
	}
	else {
		release.majorRelease(oldRelease.majorRelease)
		release.minorRelease(oldRelease.minorRelease)
		release.patch(oldRelease.patch.toLong + 1)
	}
  }
  release.save
  S.redirectTo("/release")
 }
}
