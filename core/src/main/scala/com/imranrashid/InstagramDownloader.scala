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

import java.io.{FileOutputStream, FileInputStream, File}
import java.net.{HttpURLConnection, URL}
import java.nio.channels.Channels
import java.nio.file.attribute.{FileTime, BasicFileAttributeView}
import java.nio.file.{Paths, Files}
import java.text.SimpleDateFormat
import java.util.Date
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

    val userReq = new InstagramSearchUsernameRequest(args.targetAccount)
    val userInfo = instagram.sendRequest(userReq)
    val userPk = userInfo.getUser.getPk

    val now = System.currentTimeMillis()
    val nDaysAgo = (now - TimeUnit.DAYS.toMillis(args.lastNDays)) / 1000

    val itr = new FeedIterator(instagram, userPk, nDaysAgo)
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


    liked.foreach { item =>
      download(item, args.dataDir)
    }
  }


  def getMaxImgUrl(item: InstagramFeedItem): String = {
    val candidates = item.getImage_versions2.get("candidates").asInstanceOf[java.util.ArrayList[java.util.Map[String, AnyRef]]]
    val widthToUrl = candidates.asScala.map { kvs => kvs.get("width").asInstanceOf[java.lang.Integer] -> kvs.get("url").asInstanceOf[String]}
    widthToUrl.maxBy{_._1}._2
  }

  def getMaxVideoUrl(item: InstagramFeedItem): String = {
    val versions = item.getVideo_versions.asInstanceOf[java.util.ArrayList[java.util.Map[String, AnyRef]]]
    val widthToUrl = versions.asScala.map { kvs => kvs.get("width").asInstanceOf[java.lang.Integer] -> kvs.get("url").asInstanceOf[String]}
    widthToUrl.maxBy{_._1}._2
  }

  val simpleFormatter = new SimpleDateFormat("yyyyMMddHHmmss")
  def localFileName(item: InstagramFeedItem): String = {
    val d = new Date(item.getTaken_at * 1000)
    simpleFormatter.format(d) + "_" + item.getId
  }

  def download(item: InstagramFeedItem, destDir: File): Unit = {
    val url = new URL(if (item.getVideo_versions != null) {
      getMaxVideoUrl(item)
    } else {
      getMaxImgUrl(item)
    })
    val p = url.getPath.lastIndexOf('.')
    val extension = if (p > 0) {
      url.getPath().substring(p)
    } else {
      ""
    }
    val dest = new File(destDir, localFileName(item) + extension)
    // TODO more general behavior if file exists?  probably best to always delete ...
    if (dest.exists()) {
      dest.delete()
    }
    downloadWithRetries(url, dest)
    val attributes = Files.getFileAttributeView(dest.toPath, classOf[BasicFileAttributeView]);
    val time = FileTime.fromMillis(item.getTaken_at * 1000);
    attributes.setTimes(time, time, time);
    println(s"downloaded $url to $dest")
    // TODO get hash of downloaded file
  }

  def downloadWithRetries(url: URL, dest: File, maxTries: Int = 10, waitMs: Int = 1000): Unit = {
    // Some files really take a long time to download -- perhaps especially long videos.  eg
    // wget needed 6 tries to get https://instagram.ford4-1.fna.fbcdn.net/t50.2886-16/21847929_1398889866894051_306498340940414976_n.mp4
    // so just retry
    var tries = 0
    var done = false
    while (!done && tries < maxTries) {
      if (dest.exists()) {
        dest.delete()
      }
      val conn = url.openConnection().asInstanceOf[HttpURLConnection]
      val expectedLength = conn.getContentLength
      val downloadedBytes = Files.copy(conn.getInputStream(), dest.toPath)
      if (downloadedBytes == 0 || (expectedLength != -1 && downloadedBytes != expectedLength)) {
        println(s"failed to download $url after $tries (got $downloadedBytes, short ${expectedLength - downloadedBytes})")
        Thread.sleep(waitMs)
      } else {
        done = true
      }
      tries += 1
    }
    if (!done) {
      throw new RuntimeException(s"failed to download $url after $tries")
    }
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
  var targetAccount = "beech3111"
  var lastNDays = 30
  var likers: Set[String] = Set("dianarashid", "nabiha610", "imranrashid81")
  var maxKnownLikers = 10
  var user: String = _
  var password: String = _
  var loginPropertyFile: File = new File("login.props")
  var dataDir: File = new File("/Users/irashid/Dropbox/photos/instagram_raw_download")


  lazy val loginProperties = {
    val p = new java.util.Properties()
    if (loginPropertyFile.exists() && loginPropertyFile.isFile()) {
      val in = new FileInputStream(loginPropertyFile)
      try {
        p.load(in)
        p
      } finally {
        in.close()
      }
    } else {
      p
    }
  }

  addValidation {
    // TODO there should be a generic way to get this from a file ... but right now sumac only supports
    // typesafe config which seems like overkill
    if (user == null) {
      user = sys.env.getOrElse("INSTA_USER", loginProperties.get("user").asInstanceOf[String])
    }
    if (password == null) {
      password = sys.env.getOrElse("PASSWORD", loginProperties.get("password").asInstanceOf[String])
    }

    dataDir.mkdirs()
  }
}


case class DownloadLogLine(instaId: String, takenAt: Long, murmur: String, localFile: String) {
  def toLogLine: String = {
    productIterator.mkString("\t")
  }
}

object DownloadLogLine {
  def parse(logLine: String): DownloadLogLine = {
    logLine.split("\t") match {
      case Array(instaId, takenAtStr, murmur, localFile) =>
        DownloadLogLine(instaId, takenAtStr.toLong, murmur, localFile)
      case other =>
        throw new RuntimeException("can't parse line \"" + logLine + "\"")
    }
  }
}
