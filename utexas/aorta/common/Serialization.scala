// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream, PrintWriter, BufferedReader, FileReader, File}

trait Serializable {
  def serialize(w: StateWriter)
}

abstract class StateWriter(fn: String) {
  // opt writes this if the value is None
  private val default = -1

  def done()
  def int(x: Int)
  def double(x: Double)
  def string(x: String)
  def bool(x: Boolean)
  def obj(x: Serializable) {
    x.serialize(this)
  }
  def list(ls: Seq[Serializable]) {
    int(ls.size)
    ls.foreach(x => obj(x))
  }
  def list_int(ls: Seq[Int]) {
    int(ls.size)
    ls.foreach(x => int(x))
  }
  def opt(x: Option[Int]) {
    int(x.getOrElse(default))
  }

  def ints(ls: Int*) {
    ls.foreach(x => int(x))
  }
  def doubles(ls: Double*) {
    ls.foreach(x => double(x))
  }
  def strings(ls: String*) {
    ls.foreach(x => string(x))
  }
  def bool(ls: Boolean*) {
    ls.foreach(x => bool(x))
  }
  def objs(ls: Serializable*) {
    ls.foreach(x => obj(x))
  }
  def lists(ls: Seq[Serializable]*) {
    ls.foreach(x => list(x))
  }
  def lists_ints(ls: Seq[Int]*) {
    ls.foreach(x => list_int(x))
  }
  def opts(ls: Option[Int]*) {
    ls.foreach(x => opt(x))
  }
}

class BinaryStateWriter(fn: String) extends StateWriter(fn) {
  private val out = new ObjectOutputStream(new FileOutputStream(fn))
  def done() {
    out.close()
  }

  def int(x: Int) {
    out.writeInt(x)
  }
  def double(x: Double) {
    out.writeDouble(x)
  }
  def string(x: String) {
    out.writeUTF(x)
  }
  def bool(x: Boolean) {
    out.writeBoolean(x)
  }
}

class StringStateWriter(fn: String) extends StateWriter(fn) {
  private val out = new PrintWriter(fn)
  def done() {
    out.close()
  }

  def int(x: Int) {
    out.println(x)
  }
  def double(x: Double) {
    out.println(x)
  }
  def string(x: String) {
    out.println(x)
  }
  def bool(x: Boolean) {
    out.println(x)
  }
}

abstract class StateReader(fn: String) {
  def int: Int
  def double: Double
  def string: String
  def bool: Boolean
}

class BinaryStateReader(fn: String) extends StateReader(fn) {
  private val in = new ObjectInputStream(new FileInputStream(fn))
  def int = in.readInt
  def double = in.readDouble
  def string = in.readUTF
  def bool = in.readBoolean
}

class StringStateReader(fn: String) extends StateReader(fn) {
  private val in = new BufferedReader(new FileReader(fn))
  def int = in.readLine.toInt
  def double = in.readLine.toDouble
  def string = in.readLine
  def bool = in.readLine.toBoolean
}
