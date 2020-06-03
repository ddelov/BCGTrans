package org.hackaton.bcg

import java.io.{FileWriter, PrintWriter}
import java.net.URI
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import com.google.cloud.translate.{Translate, Translation}

import scala.io.{Codec, Source}
import scala.util.Using

/**
 * Created by Delcho Delov on 24.05.20 г.
 */
trait TransUtils {

  private val metadataFile = "resources/scraper-metadata.csv"
  private val countriesFile = "resources/BCG_Google_Queries_non_english.csv"

  implicit val codec: Codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  private type ContentLang = (String, String)

  case class ScraperMetadata(country: String, url: URI, filename: String, query: String, language: String, isPdf: Boolean, isTranslated: Boolean, length: Long) {
    def loadDocument: ContentLang = {
      val countryCode = country
      val localName = if (isPdf) s"$filename.pdf.txt" else s"$filename.txt"
      val path = Paths.get(s"resources/$countryCode/$localName")
      val text: String = new String(Files.readAllBytes(path), UTF_8)
      (text, language)
    }

    def saveDocument(): Unit =
      Using(new PrintWriter(new FileWriter(metadataFile, true))) {
        _.println(s"$country,${url.toString},$filename,$query,$language,$isPdf,$isTranslated,$length")
      }
  }

  private def loadCountries(): Map[String, String] = {
    var country2code: Map[String, String] = Map()
    val lines: Iterator[String] = Using.resource(Source.fromFile(countriesFile)){ f=>f.getLines.drop(1)}
    for (line <- lines) {
      val fields = line.split(',')
      if (fields.length > 1) {
        country2code += (fields(1).trim -> fields(0).trim)
      }
    }
    country2code
  }

  def readMetadata(): Seq[ScraperMetadata] = {
    val country2code = loadCountries()

    def transform(line: String): Option[ScraperMetadata] = {
      val fields = line.split(',')
      if (fields.length > 6) {
        val country = fields(0).trim
        val countryCode = country2code.getOrElse(country, country)
        val url = new URI(fields(1))
        val filename = fields(2)
        val query = fields(3)
        val language = fields(4)
        val isPdf: Boolean = fields(5).toBoolean
        val isTranslated: Boolean = fields(6).toBoolean
        val length: Long = 0L
        Option(ScraperMetadata(countryCode, url, filename, query, language, isPdf, isTranslated, length))
      } else None
    }

    val source = Source.fromFile(metadataFile)
    val lines = source.getLines()
    val res = lines.drop(1).flatMap(x => transform(x)).toSeq
    source.close()
    res
  }

  def translate(text: String, srcLang: String = "auto", tgtLang: String = "en", format: String = "text")(implicit translateService: Translate): String = {
    val translation: Translation = translateService.translate(text, Translate.TranslateOption.sourceLanguage(srcLang), Translate.TranslateOption.targetLanguage(tgtLang), Translate.TranslateOption.format(format))
    translation.getTranslatedText
  }

  def cutSentence(text: String, upTo: Int = 3000): (String, Int) = {
    if (text.isEmpty) return ("", 0)
    val slice = text.substring(0, Seq(upTo, text.length).min)

    val maxIndex: Int = slice match {
      case v if Seq(v.lastIndexOf('.'), v.lastIndexOf('?'), v.lastIndexOf('!')).max > 1 => Seq(v.lastIndexOf('.'), v.lastIndexOf('?'), v.lastIndexOf('!')).max + 1
      case v if v.lastIndexOf(' ') > 1 => v.lastIndexOf(' ') + 1
      case _ => slice.length
    }
    (slice.substring(0, maxIndex), maxIndex)
  }

}
