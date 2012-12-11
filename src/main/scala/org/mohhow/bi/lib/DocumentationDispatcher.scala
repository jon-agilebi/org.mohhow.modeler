package org.mohhow.bi.lib

import org.mohhow._
import model._
import net.liftweb._
import http._
import SHtml._
import S._
import common._

import mapper._
import util._
import Helpers._

import org.apache.commons.io._
import org.mohhow.bi.lib.{Repository => MyRep};

object DocumentationDispatcher {
	
 def sendDocumentation(releaseId : String) : Box[LiftResponse] = {
  val f = MyRep.readAsFile("release", 0, "documentation", "documentation", releaseId.toLong)

  if(f.exists) {
		 Full(InMemoryResponse(FileUtils.readFileToByteArray(f),
                  ("Content-Type" -> "application/pdf") :: Nil,
                  Nil,
                  200))
  }
  else Full(RedirectResponse("/index"))
 }
}