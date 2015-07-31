package com.anadathur.elastic

import javax.lang.model.element.Modifier
import java.util.{List => JList}

import com.squareup.javapoet._

/**
 * Created by ajay.nadathur on 7/18/15.
 */
package object easymapping {
  val privFinal = Array(Modifier.PRIVATE, Modifier.FINAL)
  val pubAbs = Array(Modifier.PUBLIC, Modifier.ABSTRACT)

  def requireNotNull(obj: Any, msg: String = "Cannot be null") = require(obj != null, msg)

  def requireNotEmpty(str: String, msg: String = "Cannot be null/empty") = {
    requireNotNull(str, msg)
    require(!str.trim.isEmpty, msg)
  }

  def className(pkg: String, clazz: String) = ClassName.get(pkg, clazz)

  def className(clz: Class[_]) = ClassName.get(clz)

  def paramTypeName(rawType: ClassName, typeArguments: TypeName*) = ParameterizedTypeName.get(rawType, typeArguments: _*)

  case class MetaParam(name: String, typeName: TypeName, comments: String = "") {
    def methodBuilder = {
      val methodName = (if (typeName.equals(TypeName.BOOLEAN)) "is" else "get") + name.capitalize
      MethodSpec.methodBuilder(methodName)
        .returns(typeName)
        .addJavadoc(comments)
    }

    def fieldBuilder = FieldSpec.builder(typeName, name).addJavadoc(comments)
    def paramBuilder = ParameterSpec.builder(typeName, name)
  }
  
  def getParamsOfMappingType(config: Config) =
    List(
      MetaParam("typeName", TypeName.get(classOf[String]), "The name of the mapping type\n"),
      MetaParam("parent", config.mappingTypeClass, "The parent mapping object\n"),
      MetaParam("properties", paramTypeName(className(classOf[JList[_]]), config.propertyClassName),
                "List of properties declared in this type\n"),
      MetaParam("dynamic", TypeName.BOOLEAN, "Is the type dynamic\n"),
      //MetaParam("indexName", TypeName.get(classOf[String]), "name of index, defaults to field "),
      MetaParam("nested", TypeName.BOOLEAN, "Is the type nested\n")
    )
    .map( m => (m.name, m)).toMap
  
  def getParamsOfMappingTypeParam(config: Config) = 
    List(
      MetaParam("name", TypeName.get(classOf[String]), "The name of the field\n"),
      MetaParam("parent", config.mappingTypeClass, "The parent mapping object\n"),
      MetaParam("type", config.mappingTypeClass, "Type of param\n"),
      MetaParam("path", TypeName.get(classOf[String]), "The absolute path of the property in the mapping file\n"),
      MetaParam("stored", TypeName.BOOLEAN, "Is the value stored\n")
    )
    .map(m => (m.name, m)).toMap
}
