// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

import java.io.{File, PrintWriter, FileWriter}

class IO(gs_prefix: Option[String]) {
  def notify(status: String) {
    println(s"*** $status ***")
    gs_prefix match {
      case Some(prefix) => upload_gs(prefix + "status", status)
      case _ =>
    }
  }
  
  // TODO mark a bunch of files with this class, auto upload
  def upload_gs(fn: String, contents: String) {
    Util.blockingly_run(Seq("./tools/cloud/upload_gs.sh", fn, contents))
  }
  
  def upload(fn: String) {
    gs_prefix match {
      case Some(prefix) => Util.blockingly_run(Seq(
        "gsutil", "cp", fn, prefix + fn
      ))
      case None =>
    }
  }
  
  // TODO auto close this, and auto upload to GS.
  // TODO and keep it open if needed
  def output_file(fn: String) = new PrintWriter(new FileWriter(new File(fn)), true /* autoFlush */)
  def compress(fn: String) {
    Util.blockingly_run(Seq("gzip", "-f", fn))
  }

  def done(fn: String) {
    compress(fn)
    upload(fn + ".gz")
  }
}
