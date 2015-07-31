package com.anadathur.elastic.easymapping

import java.io.{File, InputStreamReader}
import java.util.{Map => JMap, UUID}
import com.google.gson.Gson
import com.squareup.javapoet.{TypeSpec, JavaFile}
import javax.lang.model.element.Modifier
/**
 * Created by ajay.nadathur on 7/18/15.
 */
object Main extends App {
  val gson = new Gson()
  def reader = new InputStreamReader(getClass.getResourceAsStream("/TweetMapping.json"))
  val mapping: JMap[_, _] = gson.fromJson(reader, classOf[JMap[_, _]])

  val tweetMapping =
    TypeSpec.classBuilder("TweetMapping")
      .addModifiers(Modifier.PUBLIC, Modifier.FINAL)
      .build()
  val javaFile =
    JavaFile.builder("com.anadathur.elastic.easymapping", tweetMapping)
      .build()
  println(mapping)
  javaFile.writeTo(System.out)

  println("\n\n\n\n\n")
  val folder = new File("/tmp/f0619557-c343-4a35-92e3-9bf3b175d8b5")
  println(s"Writing to: $folder")
  new ClassGenerator(Config("com.anadathur.easy", folder, verbose = true,
                            resources = Map("TweetMapping" -> getClass.getResourceAsStream("/TweetMapping.json")))).generate()
}
