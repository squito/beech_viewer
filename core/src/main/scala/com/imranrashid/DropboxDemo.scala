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

import scala.collection.JavaConverters._

import com.dropbox.core.DbxRequestConfig
import com.dropbox.core.v2.DbxClientV2
import com.quantifind.sumac.{FieldArgs, ArgMain}

object DropboxDemo extends ArgMain[DropboxDemoArgs] {

  def main(args: DropboxDemoArgs): Unit = {
    val config = DbxRequestConfig.newBuilder("dropbox/java-tutorial").withUserLocale("en_US")
      .build()
    val client = new DbxClientV2(config, args.token)
    val account = client.users().getCurrentAccount()
    println(account.getName().getDisplayName())

    var result = client.files().listFolder("");
    var keepGoing = true
    while (keepGoing) {
      result.getEntries().asScala.foreach { metadata =>
        println(metadata.getPathLower())
      }

      if (!result.getHasMore()) {
        keepGoing = false
      } else {
        result = client.files().listFolderContinue(result.getCursor());
      }
    }
  }
}

class DropboxDemoArgs extends FieldArgs {
  var credentialsFile: File = new File("dropbox.props")
  var token: String = _

  lazy val loginProperties = {
    val p = new java.util.Properties()
    if (credentialsFile.exists() && credentialsFile.isFile()) {
      val in = new FileInputStream(credentialsFile)
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
    if (token == null) {
      token = sys.env.getOrElse("DROP_TOKEN", loginProperties.get("dropbox_token").asInstanceOf[String])
    }
  }
}
