package syndeticlogic.argot

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
  
  def integerType(t: ArgotInteger): String = "integer "+t.id+key(t.key)
  
  def codeableRef(t: CodeableRef): String = "codeable "+t.typeName+" "+t.id+key(t.key)+ storageStrategy(t.storageStrategy)
  
  def storageStrategy(s: StorageStrategy): String = s match {
    case x: Compose => " compose"
    case x: Decompose => " decompose"
  }
  
  def matchTypes(t: List[ArgotType]): String = {
    t.foldLeft("")((result, current) => concat(result, current))
  }
 
  def concat(r: String, c: ArgotType): String = r match {
      case "" => matchType(c)
      case _ => r+", "+matchType(c) 
  }
 
  def matchType(c: ArgotType): String = c match {
    case x: ArgotTypeType => typeType(x)
    case x: ArgotBoolean => booleanType(x)
    case x: ArgotByte => byteType(x)
    case x: ArgotChar => ""
    case x: ArgotShort => ""
    case x: ArgotInteger => integerType(x)
    case x: ArgotLong => ""
    case x: ArgotFloat => ""
    case x: ArgotDouble => ""
    case x: ArgotString => ""
    case x: ArgotBinary => ""
    case x: CodeableRef => codeableRef(x)
    case x: ArgotSpecialType => matchSpecialType(x)
  }
}

trait TableBuilder extends TypesBuilder with SpecialTypes {
}

trait ObjectBuilder extends TypesBuilder with SpecialTypes {
  def objectDef()
}

class DDLBuilder extends ArgotBuilder with TypesBuilder with SpecialTypesBuilder {

  def tableDef(t: TableDef): String = "table "+t.id+" { "+matchTypes(t.typeList)+" }" 
    
  def objectDef(t: SingletonDef): String = ""
  
  override def build(t: ArgotParseTree): String = t match {
    case x: TableDef => tableDef(x)
    case x: SingletonDef => objectDef(x)
    case x: ArgotSpecialType => matchSpecialType(x)
  }  
}

