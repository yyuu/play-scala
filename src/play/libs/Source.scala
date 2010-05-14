package play.libs


/**
 * add timeout handling and fixing resource closing for scala.io.Source.fromURL
 */

object Source {
  def fromURLPath(url: String, readTimeout: Int = 5000, connectionTimeOut: Int = 3000):io.Source = {
    import io.Source.{fromInputStream,DefaultBufSize}
    val conn = new java.net.URL(url).openConnection()
    conn.setReadTimeout(readTimeout)
    conn.setConnectTimeout(connectionTimeOut)
    val inputStream = conn.getInputStream
    fromInputStream(inputStream,DefaultBufSize, null,() => inputStream.close())
   }
}


