package feh.tec.web.common

import com.typesafe.config.ConfigFactory

trait WebsocketConf {
  
  object wsConf{
    lazy val get = ConfigFactory.load("websocket.conf")

    def secure(path: String) = get.getBoolean(s"websocket.$path.secure")

    object front{
      def host(path: String) = get.getString(   s"websocket.$path.front.host")
      def port(path: String) = get.getInt(      s"websocket.$path.front.port")
      def path(path: String) = get.getString(   s"websocket.$path.front.path")

      def url(path: String) = {
        val pref = if(wsConf.secure(path)) "wss" else "ws"
        s"$pref://${front.host(path)}:${front.port(path)}${front.path(path)}"
      }
    }

    object back{
      def host(path: String) = get.getString(   s"websocket.$path.back.host")
      def port(path: String) = get.getInt(      s"websocket.$path.back.port")

      def asString(path: String) = host(path) + ":" + port(path)
    }


  }
  
}
