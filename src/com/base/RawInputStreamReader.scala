package com.base

import java.io._

class RawInputStreamReader(var in : InputStream) extends Reader {
	
	override def read(cbuf : Array[Char], off : Int, len : Int) = {
		if(!isOpen)
			throw new IOException("rawReader is closed")
			
		val buf = new Array[Byte](len) 
		val ret = in.read(buf)
		
		for(i <- 0 to len - 1) cbuf(off + i) = buf(i).toChar
		
		ret
	}
	
	override def close =  {
		 	lock.synchronized {
      if (in != null) {
      	in.close();
				in = null;
      }
		}  
	}
	
	def isOpen = in != null
}