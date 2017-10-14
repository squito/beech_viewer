/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.imranrashid

import java.io.{FileInputStream, File}
import java.net.{HttpURLConnection, URL}
import java.util.concurrent.TimeUnit

import org.brunocvcunha.instagram4j.requests.payload.InstagramFeedItem

import scala.collection.JavaConverters._

import com.quantifind.sumac.{ArgMain, FieldArgs}
import org.brunocvcunha.instagram4j.Instagram4j
import org.brunocvcunha.instagram4j.requests.{InstagramUserFeedRequest, InstagramSearchUsernameRequest}


object InstagramDownloader extends ArgMain[InstagramDownloaderArgs] {

  def main(args: InstagramDownloaderArgs): Unit = {
    val instagram = Instagram4j.builder().username(args.user).password(args.password).build()
    instagram.setup()
    instagram.login()

    val userReq = new InstagramSearchUsernameRequest("beech3111")
    val userInfo = instagram.sendRequest(userReq)
    val userPk = userInfo.getUser.getPk
    println(s"beech pk = ${userPk}")


    val now = System.currentTimeMillis()
    val tenDaysAgo = (now - TimeUnit.DAYS.toMillis(10)) / 1000
    val feedReq = new InstagramUserFeedRequest(userPk, null, tenDaysAgo)
    val feed = instagram.sendRequest(feedReq)
    println(s"Got ${feed.getNum_results} items from feed")


    feed.getItems.asScala.take(2).foreach { item =>
      val likers = if (item.getLikers != null) {
        item.getLikers.asScala.map{_.getUsername}.mkString(",")
      } else {
        "<none>"
      }
      println(s"feed item ${item.getId} with likers $likers")
      println(s"taken at ${item.getTaken_at}")
      println(s"device timestamp${item.getDevice_timestamp}")
      item.getImage_versions2.asScala.foreach { case (k, v) =>
        println(k -> v)
        println(v.getClass())
          // SUCCESS !!! the full size URL is buried in here
      }
      println()
    }

    val itr = new FeedIterator(instagram, userPk, tenDaysAgo)
    // just in case, don't want to accidentally go crazy
    val subItr = itr.take(10000)

    val liked = subItr.filter { item =>
      // TODO check that it has no more than maxKnownLikers (if it does, extra calls I guess?)
      if (item.getLikers() != null) {
        item.getLikers.asScala.exists { liker => args.likers.contains(liker.getUsername) }
      } else {
        false
      }
    }.toIndexedSeq


    liked.foreach { item => println(getMaxImgUrl(item)) }
  }


  def getMaxImgUrl(item: InstagramFeedItem): String = {
    val candidates = item.getImage_versions2.get("candidates").asInstanceOf[java.util.ArrayList[java.util.Map[String, AnyRef]]]
    val widthToUrl = candidates.asScala.map { kvs => kvs.get("width").asInstanceOf[java.lang.Integer] -> kvs.get("url").asInstanceOf[String]}
    widthToUrl.maxBy{_._1}._2
  }

  class FeedIterator(val instagram: Instagram4j, val userPk: Long, val minTimeSeconds: Long) extends Iterator[InstagramFeedItem] {
    var currentFeedItems: Iterator[InstagramFeedItem] = null
    var nextMinId:String = null

    private def prepNext(): Unit = {
      if (currentFeedItems == null || currentFeedItems.isEmpty) {
        val nextFeed = instagram.sendRequest(
          new InstagramUserFeedRequest(userPk, nextMinId, minTimeSeconds))
        currentFeedItems = nextFeed.getItems().asScala.iterator
      }
    }

    override def hasNext: Boolean = {
      prepNext()
      currentFeedItems.hasNext
    }

    override def next(): InstagramFeedItem = {
      prepNext()
      val next = currentFeedItems.next()
      if (nextMinId == null || next.getId < nextMinId) {
        nextMinId = next.getId
      }
      next
    }
  }
}

class InstagramDownloaderArgs extends FieldArgs {
  var likers: Set[String] = Set("dianarashid", "nabiha610", "imranrashid81")
  var maxKnownLikers = 10
  var user: String = _
  var password: String = _
  var loginPropertyFile: File = _

  lazy val loginProperties = {
    val p = new java.util.Properties()
    val in = new FileInputStream(loginPropertyFile)
    try {
      p.load(in)
      p
    } finally {
      in.close()
    }
  }

  addValidation {
    // TODO there should be a generic way to get this from a file ... but right now sumac only supports
    // typesafe config which seems like overkill
    if (user == null) {
      user = sys.env.getOrElse("USER", loginProperties.get("user").asInstanceOf[String])
    }
    if (password == null) {
      password = sys.env.getOrElse("PASSWORD", loginProperties.get("password").asInstanceOf[String])
    }
  }
}

