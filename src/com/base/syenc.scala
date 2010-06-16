package com.base

import java.io._
import java.util.zip.CRC32

/*
Example USE:

val baos = new ByteArrayOutputStream
SYEnc.decode(new ByteArrayInputStream(stringFromStream.getBytes("UTF-8")), baos)
println(baos.toString)

*/

/**
 * Scala Decoder Implementation for the YEncoding project.<br>
 * This class is used to decode data encoded with yenc<br><br>
 * See <a href="http://www.yenc.org">www.yenc.org</a> for details.<br><br>
 *
 * <b>Version History:</b><br><br>
 * &nbsp;&nbsp;v. 1 Initial Port of Alex Rass' <a href="http://nicestep.sourceforge.net/java_ydec.zip">Java Yenc Decoder</a> to Scala<br>
 * @author &lt; Victor Igumnov &gt; victori@fabulously40.com
 * @version 1<br>
 * <P>
 * Copyright &copy; 2002. By Victor Igumnov.  This software is to be treated according with
 * the GNU license (easily available on the Internet)
 * <P>
 */
object SYEnc {
  private val BUFFERSIZE = 32768

  @throws(classOf[IOException])
  def decode(is: InputStream, os: OutputStream): Boolean = {
    /**buffer for caching the writes */
    val buffer: Array[Byte] = new Array(BUFFERSIZE)

    /**length of the buffer thst's caching the writes */
    var bufferLength = 0

    val streamReader = new BufferedReader(new RawInputStreamReader(is, "UTF-8"))

    /* Get initial parameters */
    var line = streamReader.readLine
    while (line != null && !line.startsWith("=ybegin")) {
      line = streamReader.readLine
    }
    if (line == null) throw new IOException("Error while looking for start of a file.  Could not locate line starting with \"=ybegin\".")
    
    /* Decode the file */
    var character = 0
    var special = false

    line = streamReader.readLine
    var crc = new CRC32
    while (line != null && !line.startsWith("=yend")) {
      for (lcv <- 0 until line.length) {
        character = line.charAt(lcv).asInstanceOf[Int]
        if (character != 61) {
          buffer(bufferLength) = (if (special) character - 106 else character - 42).toByte //  Decodes the character into binary.
          bufferLength = bufferLength + 1 // just to be explicit.
          if (bufferLength == BUFFERSIZE) {
            os.write(buffer)
            crc.update(buffer)
            bufferLength = 0
          }
          special = false
        } else {
          special = true
        }
      }
      line = streamReader.readLine()
    }
    
    if (bufferLength > 0) {
      os.write(buffer, 0, bufferLength)
      crc.update(buffer, 0, bufferLength)
    }

    os.flush
    os.close
    streamReader.close
    // add CRC check here
    if (line != null && line.startsWith("=yend")) {
      var fileCRC: Long = -1
      var crcVal = parseForString(line, "pcrc32") // part
      if (crcVal == null) crcVal = parseForCRC(line)
      if (crcVal != null) fileCRC = BigInt.apply(crcVal, 16).longValue
      return (fileCRC == crc.getValue)
    } else {
      return false // no CRC found.
    }
  }


  /**Parsing function, used to obtain CRC value
   *
   * @param line line from the file that contains descriptors.
   * @return value part of the name-value pair
   */
  def parseForCRC(line: String): String = {
    var indexStart: Int = line.indexOf(" crc32=") + 1
    var indexEnd: Int = line.indexOf(" ", indexStart)
    if (indexEnd == -1) indexEnd = line.length()
    if (indexStart > -1) line.substring(indexStart + 6, indexEnd) else null
  }

  /**General parsing function - do not use for name (because doesn't handle blank spaces)
   *  or crc32 (because will stop on String pcrc32).
   *
   * @param line line from the file that contains descriptors.
   * @return value part of the name-value pair
   */
  def parseForString(line: String, param: String): String = {
    var indexStart: Int = line.indexOf(param + "=")
    var indexEnd: Int = line.indexOf(" ", indexStart)
    if (indexEnd == -1) indexEnd = line.length()
    if (indexStart > -1) line.substring(indexStart + param.length() + 1, indexEnd) else null
  }

  /**
   * Parsing function, used to obtain the name of the file.
   * Supports spaces in the name at a cost of an assumption
   * that the name is the last parameter in the line.
   *
   * @param line line from the file that contains descriptors.
   * @return value part of the name-value pair
   */
  def parseForName(line: String): String = {
    var indexStart: Int = line.indexOf("name=")
    var indexEnd: Int = line.indexOf(" ", indexStart)
    if (indexEnd == -1) indexEnd = line.length()
    if (indexStart > -1) line.substring(indexStart + 5, line.length()) else null
  }
}