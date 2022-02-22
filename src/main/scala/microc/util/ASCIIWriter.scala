package microc.util

import microc.util.CharacterSets.isNewLine

import java.io.Writer

class ASCIIWriter(writer: Writer) extends Writer {
  private var b: Int = 0

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    if (off < 0 || len < 0 || off + len > cbuf.length) {
      throw new IndexOutOfBoundsException()
    }

    for (i <- 0 until len) {
      val ch = cbuf(off + i)
      if (isNewLine(ch)) {
        writer.write(b.toChar)
        b = 0
      } else {
        b = b * 10 + ch.asDigit
      }
    }
  }

  override def flush(): Unit = {
    if (b != 0) {
      writer.write(b.toChar)
    }

    writer.flush()
  }

  override def close(): Unit = writer.close()
}
