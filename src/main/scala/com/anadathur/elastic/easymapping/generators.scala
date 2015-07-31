package com.anadathur.elastic.easymapping

import javax.lang.model.element.Modifier
import com.squareup.javapoet.MethodSpec.Builder
import com.squareup.javapoet._

class DefaultMappingGen extends TypeGenerator {
  import scala.collection.JavaConversions._
  val generators = List(
    generateMappingType _,
    generateDefaultMappingType _,
    generateMappingTypeProperty _,
    generateDefaultMappingTypeProperty _
  )

  override def apply(config: Config): List[TypeSpec] = {
    generators.map(_.apply(config)).toList
  }

  def generateDefaultMappingType(config: Config): TypeSpec = {
    val params = getParamsOfMappingType(config)
    val builder =
      TypeSpec.classBuilder("DefaultMappingType")
        .addModifiers(Modifier.PUBLIC)
        .addSuperinterface(config.mappingTypeClass)
        .addFields(params.values.map {_.fieldBuilder.addModifiers(privFinal: _*).build() }.toList)

    val constructorParams = params.values.map { _.paramBuilder.build() }

    val codeBlockBuilder = CodeBlock.builder()
    for (param <- params.values) {
      codeBlockBuilder.addStatement(s"this.${param.name} = ${param.name}")
    }

    val constructorBuilder =
      MethodSpec.constructorBuilder()
        .addModifiers(Modifier.PUBLIC)
        .addParameters(constructorParams.toList)
        .addCode(codeBlockBuilder.build())
    val methods = getMethodBuilders(params).map { case (name, typeName, bldr) =>
        bldr.addCode(CodeBlock.builder().addStatement(s"return $name").build())
          .build()
    }

    val hashSet: ClassName = ClassName.get("java.util", "HashSet")
    val simpleMethod =
      MethodSpec.methodBuilder("isSimpleType")
        .returns(TypeName.BOOLEAN)
        .addModifiers(Modifier.PUBLIC)
        .addStatement("$T set = new $T<>()",  hashSet, hashSet)

    Array("string", "integer", "long", "float", "double", "boolean", "date").foreach {name =>
      simpleMethod.addStatement("set.add($S)", name)
    }

    simpleMethod.addStatement("return set.contains(this.typeName)")

    builder.addMethod(constructorBuilder.build())
      .addMethods(methods.toList)
      .addMethod(simpleMethod.build())
      .build()
  }

  def getMethodBuilders(params: Map[String, MetaParam]) =
    params.values.map { method =>
      (method.name, method.typeName, method.methodBuilder)
    }

  def getMethodName(name: String, typeName: TypeName) =
    (if (typeName.equals(TypeName.BOOLEAN)) "is" else "get") + name.capitalize

  def generateMappingType(config: Config): TypeSpec = {
    val fields =
      List("string", "integer", "long", "float", "double", "boolean", "date", "binary", "object", "nested")
        .map { name =>
          FieldSpec.builder(classOf[String], name.toUpperCase + "_TYPE")
             .addModifiers(Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
            .initializer("\"" + name + "\"")
            .build()
        }

    val builders: Iterable[(String, TypeName, Builder)] = getMethodBuilders(getParamsOfMappingType(config))
    val builder =
      TypeSpec.interfaceBuilder("MappingType")
        .addModifiers(Modifier.PUBLIC)
        .addMethods(builders.map {_._3.addModifiers(Modifier.ABSTRACT, Modifier.PUBLIC).build()}.toList)
        .addMethod(
          MethodSpec.methodBuilder("isSimpleType")
            .addModifiers(Modifier.PUBLIC, Modifier.ABSTRACT)
            .returns(TypeName.BOOLEAN)
            .build()
        )
        .addFields(fields)

    builder.build()
  }
  
  def generateMappingTypeProperty(config: Config): TypeSpec = {
    val methods = getMethodBuilders(getParamsOfMappingTypeParam(config)).map { case (name, tName, bldr) =>
      bldr.addModifiers(pubAbs: _*).build()
     }

    TypeSpec.interfaceBuilder("MappingTypeProperty")
      .addModifiers(Modifier.PUBLIC)
      .addMethods(methods.toList)
      .build()
  }

  def generateDefaultMappingTypeProperty(config: Config): TypeSpec = {
    val param: Map[String, MetaParam] = getParamsOfMappingTypeParam(config)
    val methods = getMethodBuilders(param).map { case (name, tName, bldr) =>
      bldr.addModifiers(Modifier.PUBLIC)
        .addStatement("return $N", name)
        .build()
    }

    val block = CodeBlock.builder()
    param.values.foreach { p => block.addStatement("this.$N = $N", p.name, p.name)}
    TypeSpec.classBuilder("DefaultMappingTypeProperty")
      .addModifiers(Modifier.PUBLIC)
      .addMethod(
        MethodSpec.constructorBuilder()
          .addModifiers(Modifier.PUBLIC)
          .addParameters(param.values.map(_.paramBuilder.build()).toList)
          .addCode(block.build())
          .build()
      )
      .addFields(param.values.map(_.fieldBuilder.addModifiers(privFinal: _*).build()).toList)
      .addMethods(methods.toList)
      .build()

  }
}