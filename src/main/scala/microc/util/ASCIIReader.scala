package microc.util

import microc.util.CharacterSets.NL

import java.io.Reader

class ASCIIReader(reader: Reader) extends Reader {
  override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    var nchars = 0
    var empty = false

    // UTF 5 chars + NL
    while (nchars + 6 < len && !empty) {
      val r = reader.read()

      if (r == -1) {
        empty = true
      } else {
        val src = (r.toString + NL).toCharArray
        if (nchars + src.length <= len) {
          Array.copy(src, 0, cbuf, nchars, src.length)
          nchars += src.length
        }
      }
    }

    if (nchars > 0) {
      nchars
    } else {
      -1
    }
  }

  override def close(): Unit = reader.close()
}
