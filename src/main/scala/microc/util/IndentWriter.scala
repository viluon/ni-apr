package microc.util

import microc.util.CharacterSets.{NL, isNewLine}

import java.io.Writer

class IndentWriter(writer: Writer, indent: Option[Int]) extends Writer {
  private var level = 0

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    if (indent.isDefined) {
      if (off < 0 || len < 0 || off + len > cbuf.length) {
        throw new IndexOutOfBoundsException()
      }

      for (i <- 0 until len) {
        val ch = cbuf(off + i)
        if (isNewLine(ch)) {
          nl()
        } else {
          writer.write(ch)
        }
      }
    } else {
      writer.write(cbuf, off, len)
    }
  }

  override def flush(): Unit = writer.flush()

  override def close(): Unit = writer.close()

  def <<(s: String): this.type = {
    append(s)
    this
  }

  def indent(thunk: => Unit): this.type = {
    if (indent.isDefined) {
      incLevel()
      thunk
      decLevel()
    } else {
      thunk
    }
    this
  }

  private def nl(): this.type = {
    writer.append(NL).append(" ".repeat(indent.map(level * _).getOrElse(0)))
    this
  }

  private def incLevel(): Unit = {
    level += 1
    nl()
  }

  private def decLevel(): Unit = {
    level -= 1
    nl()
  }
}
