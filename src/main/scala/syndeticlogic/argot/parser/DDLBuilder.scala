package syndeticlogic.argot.parser

trait SpecialTypesBuilder {
  def vector(v: VectorDef): String = "vector["+v.typeName+"] "+v.id
  
  def map(m: MapDef): String = "map["+m.keyName+","+m.valueName+"] "+m.id
  
  def matchSpecialType(st: ArgotSpecialType): String = st match {
    case x: VectorDef => vector(x)
    case x: MapDef => map(x)
  }
}

trait KeyBuilder {
  def key(key: Key): String = key match {
      case x: PrimaryKey => " primary"
      case x: ForeignKey => " foreign"
      case x: IndexKey => " index"
      case x: NoKey => ""
  }
}

trait TypesBuilder extends KeyBuilder with SpecialTypesBuilder with Types {
  def typeType(t: ArgotTypeType): String = "type "+t.id+ key(t.key)

  def booleanType(t: ArgotBoolean): String = "boolean "+t.id+key(t.key)

  def byteType(t: ArgotByte): String = "byte "+t.id+key(t.key)

  def charType(t: ArgotChar): String = "char "+t.id+key(t.key)

  def shortType(t: ArgotShort): String = "short " +t.id+key(t.key)

  def integerType(t: ArgotInteger): String = "integer "+t.id+key(t.key)

  def longType(t: ArgotLong): String = "long "+t.id+key(t.key)

  def floatType(t: ArgotFloat): String = "float "+t.id+key(t.key)

  def doubleType(t: ArgotDouble): String = "double "+t.id+key(t.key)

  def stringType(t: ArgotString): String = "string "+t.id+key(t.key)

  def binaryType(t: ArgotBinary): String = "binary "+t.id+key(t.key)

  def codeableRef(t: CodeableRef): String = "codeable "+t.typeName+" "+t.id+ storageStrategy(t.storageStrategy)

  def storageStrategy(s: StorageStrategy): String = s match {
    case x: Compose => ""
    case x: Decompose => " decompose"
  }

  def matchTypes(t: List[ArgotType]): String = {
    t.foldLeft("")((result, current) => concat(result, current))
  }

  def concat(r: String, c: ArgotType): String = r match {
    case "" => System.getProperty("line.separator")+"\t"+matchType(c)
    case _ => r+","+System.getProperty("line.separator")+"\t"+matchType(c)
  }
 
  def matchType(c: ArgotType): String = c match {
    case x: ArgotTypeType => typeType(x)
    case x: ArgotBoolean => booleanType(x)
    case x: ArgotByte => byteType(x)
    case x: ArgotChar => charType(x)
    case x: ArgotShort => shortType(x)
    case x: ArgotInteger => integerType(x)
    case x: ArgotLong => longType(x)
    case x: ArgotFloat => floatType(x)
    case x: ArgotDouble => doubleType(x)
    case x: ArgotString => stringType(x)
    case x: ArgotBinary => binaryType(x)
    case x: CodeableRef => codeableRef(x)
    case x: ArgotSpecialType => matchSpecialType(x)
  }
}

trait TableBuilder extends TypesBuilder with SpecialTypes {
  def tableDef(t: TableDef): String = "table "+t.id+" {"+matchTypes(t.typeList)+"\n}"
}

trait ObjectBuilder extends TypesBuilder with SpecialTypes {
  def matchMethod(t: Method): String = {""}
  
  def superType(s: String): String = {
    if("".equals(s)) ""
    else " extends "+s
  }

  def objectDef(t: SingletonDef): String = "object "+t.typeName+" {"+matchTypes(t.typeList)+System.getProperty("line.separator")+"}"

  def codeableDef(t: Codeable): String =
    "codeable "+t.typeName+superType(t.superType)+" {"+matchTypes(t.typeList)+matchMethod(t.method)+matchMethod(t.method1)+System.getProperty("line.separator")+"}"
}

class DDLBuilder extends ArgotBuilder with TypesBuilder with ObjectBuilder with TableBuilder {
  override def build(t: ArgotParseTree): String = t match {
    case x: TableDef => tableDef(x)
    case x: SingletonDef => objectDef(x)
    case x: ArgotSpecialType => matchSpecialType(x)
    case x: Codeable => codeableDef(x)
  }
}

