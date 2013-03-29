package org.mohhow.bi.lib

import org.apache.http.client._
import org.apache.http.client.methods._
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.protocol.HTTP
import org.apache.http.impl.client._
import org.apache.http.entity._
import org.apache.http.util._
import org.apache.http.NameValuePair
import org.apache.http.message.BasicNameValuePair

import org.mohhow.bi.util.{Utility => MyUtil}

import scala.xml._
import java.io._
import java.util.ArrayList
import org.apache.commons.io._

import org.mohhow._
import model._

object HTTPClientUtility {

	val httpClient = new DefaultHttpClient()
	
	def postTextOrNode(url: String, content: Node, user: User, text: String) = {
		
		val postRequest = new HttpPost(url)
		
		if(user != null) {
			val nvps = new ArrayList[NameValuePair]()
			nvps.add(new BasicNameValuePair("username", user.email))
			nvps.add(new BasicNameValuePair("password", user.password))
			nvps.add(new BasicNameValuePair("Accept", "text/xml"))
			nvps.add(new BasicNameValuePair("Content-Type", "text/xml"))
			
			postRequest.setEntity(new UrlEncodedFormEntity(nvps, HTTP.UTF_8))
		}
		
		val f = new File("post.xml")
		
		if(text == null) XML.saveFull(f.getPath(), content, "UTF-8", true, null)
		else FileUtils.writeStringToFile(f, text)
		
		val fe = new FileEntity(f,"application/xml;charset=utf-8")
		postRequest.setEntity(fe)
		val response=httpClient.execute(postRequest)
		val entity = response.getEntity();
		
		val bufferedReader = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
		var line = ""
		var goOn = true
		var lines = List("")
		
		while (goOn) {
			line = bufferedReader.readLine()
			if(line == null) goOn = false 
			else lines = line :: lines
		}
		
		EntityUtils.consume(entity)
		
		MyUtil.getSeqHeadText(XML.loadString((("" /: lines.reverse) (_ + _)).trim) \\ "status")
	}
	
	def post(url: String, content: Node, user: User) = postTextOrNode(url, content, user, null) 
	def postText(url: String, text: String, user: User) = postTextOrNode(url, null, user, text) 
	
	def get(url: String) = {
		val getRequest = new HttpGet(url);
		val response = httpClient.execute(getRequest);
		val status = response.getStatusLine().toString
		val entity = response.getEntity();
		EntityUtils.consume(entity)
		status
	}
}