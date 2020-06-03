package org.hackaton.bcg

import java.io.{File, FileInputStream}

import com.google.auth.oauth2.ServiceAccountCredentials
import com.google.cloud.translate.{Translate, TranslateOptions}

import scala.annotation.tailrec
import scala.util.Using

/**
 * Created by Delcho Delov on 24.05.20 г.
 */
object Trans extends TransUtils {

  def getChunks(text: String, upTo: Int = 3000): Seq[String] = {
    @tailrec
    def cc(str: String, ind: Int, chunks: Seq[String]): Seq[String] = {
      if (str.trim.isEmpty || ind == 0) chunks
      else {
        val (part1, len1) = cutSentence(str, upTo)
        cc(str.drop(len1), ind, chunks :+ part1)
      }
    }

    cc(text, upTo, Seq())
  }

  def main(args: Array[String]): Unit = {
    val documents: Seq[ScraperMetadata] = readMetadata().filterNot(_.language.equals("en")).filter(x => x.isPdf || !x.isTranslated)
    implicit lazy val translateService: Translate = {
      val sac:ServiceAccountCredentials =
        Using.resource(new FileInputStream("/home/ddelov/Documents/GoogleCloudTranslateProject-5e1b4269eb00.json")) {sac=>ServiceAccountCredentials.fromStream(sac)}
      TranslateOptions.newBuilder.setCredentials(sac).build.getService
    }
//    if(args.isEmpty) {
//      println("Please provide Google Cloud credentials json")
//      System.exit(1)
//    }

    documents.foreach(d => {
      val localName = if(d.isPdf) s"${d.filename}.pdf.txt" else s"${d.filename}.txt"
      val len:Long = new File(s"resources/${d.country}/$localName").length()
      if(len<20 || len>40000){
        //just register file as not translated
        d.copy(length = len).saveDocument()
      } else{
        //translate
        val (firstBg, srcLang) = d.loadDocument
        import java.io._
        val newFileName = s"${d.filename}-translated.txt"
        Using(new PrintWriter(new File(s"resources/${d.country}/$newFileName"))) { writer =>
          getChunks(firstBg).foreach(txt => {
            writer.print(translate(txt, srcLang))
          })
        }
        d.copy(filename = newFileName, language = "en", length=len, isTranslated = true).saveDocument()
      }
    })
  }

}
