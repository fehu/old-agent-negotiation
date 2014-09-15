package feh.tec.web.util

import feh.tec.web.common.WebsocketConf
import feh.util.file._

object GenNginxConfig{

  def HttpServerWebSocketConfig(incomingPath: String, port: Int, socketName: String,  redirectTo: String) =
  s"""
    map $$http_upgrade $$connection_upgrade {
        default upgrade;
        '' close;
    }

    upstream ws-$socketName {
        server $redirectTo;
    }

    server {
        listen $port;
        location $incomingPath {
            proxy_pass http://ws-$socketName;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $$http_upgrade;
            proxy_set_header Connection $$connection_upgrade;
	      }
    }"""

  object byConf extends WebsocketConf{
    def HttpServerWebSocketConfig(path: String) = GenNginxConfig
      .HttpServerWebSocketConfig(wsConf.front.path(path), wsConf.front.port(path), path, wsConf.back.asString(path))
  }
}

object GenNginxConfigs extends App{
  import GenNginxConfig._

  val base = args(0)

  val configs: Map[String, String] = Map(
    "nginx.conf-n-queen.ws" -> byConf.HttpServerWebSocketConfig("n-queen")
  )


  private def writeConfigs() = configs foreach{
    case (filename, contents) => File(base / filename) withOutputStream File.write.utf8(contents)
  }

  writeConfigs()
}