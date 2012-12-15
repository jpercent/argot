package syndeticlogic.argot.parser

class DDLBuilder extends ArgotBuilder {
  val ls = System.getProperty("line.separator")

  def matchSpecialType(st: ArgotSpecialType): String = st match {
    case x: VectorDef =>  "vector[" + x.typeName + "] " + x.id
    case x: MapDef => "map[" + x.keyName + "," + x.valueName + "] " + x.id
  }
  
  def matchKey(key: Key): String = key match {
    case x: PrimaryKey => " primary"
    case x: ForeignKey =>  " foreign"
    case x: IndexKey => " index"
    case x: NoKey => ""
  }

  def matchStorageStrategy(s: StorageStrategy): String = s match {
    case x: Compose => ""
    case x: Decompose => " decompose"
  }

  def matchTypes(t: List[ArgotType]): String = {
    t.foldLeft("")((result, current) => concat(result, current))
  }

  def concat(r: String, c: ArgotType): String = r match {
    case "" => ls + "\t" + matchType(c)
    case _ => r + "," + ls + "\t" + matchType(c)
  }

  def matchType(c: ArgotType): String = c match {
    case t: ArgotTypeType => "type " + t.id + matchKey(t.key)
    case t: ArgotBoolean => "boolean " + t.id + matchKey(t.key)
    case t: ArgotByte => "byte " + t.id + matchKey(t.key)
    case t: ArgotChar => "char " + t.id + matchKey(t.key)
    case t: ArgotShort => "short " + t.id + matchKey(t.key)
    case t: ArgotInteger => "integer " + t.id + matchKey(t.key)
    case t: ArgotLong => "long " + t.id + matchKey(t.key)
    case t: ArgotFloat => "float " + t.id + matchKey(t.key)
    case t: ArgotDouble => "double " + t.id + matchKey(t.key)
    case t: ArgotString => "string " + t.id + matchKey(t.key)
    case t: ArgotBinary => "binary " + t.id + matchKey(t.key)
    case t: CodeableRef => "codeable " + t.typeName + " " + t.id + matchStorageStrategy(t.storageStrategy)
    case t: ArgotSpecialType => matchSpecialType(t)
  }
  
  def matchReference(r: Reference): String = r match {
    case x: MemberReference => x.member
    case x: VectorReference => x.id+"["+x.index+"]"
    case x: MapReference => x.id +"["+matchReference(x.key)+"]"
    case x: QualifiedMemberReference => x.obj+"."+matchReference(x.member)
  } 
  
  def matchBinary(lhs: Reference, op: String, rhs: Reference): String = matchReference(lhs)+op+matchReference(rhs)
  
  def matchCondition(bf: BooleanFunction): String = bf match {
    case x: Negation => "!("+matchCondition(x.f)+")"
    case x: Less => matchBinary(x.lhs, " < ", x.rhs)
    case x: LessOrEqual => matchBinary(x.lhs, " < ", x.rhs)
    case x: EqualEqual => matchBinary(x.lhs, " < ", x.rhs) 
    case x: NotEqual => matchBinary(x.lhs, " < ", x.rhs)
    case x: GreaterOrEqual => matchBinary(x.lhs, " < ", x.rhs)
    case x: Greater => matchBinary(x.lhs, " < ", x.rhs)
    case x: EqualsObject => ""
    case x: And => matchCondition(x.lhs)+" && "+matchCondition(x.rhs)
    case x: Or => matchCondition(x.lhs)+" || "+matchCondition(x.rhs)
  } 
    
  def condition(c: Condition): String = matchCondition(c.f)
  
  def matchIfClause(r: String, c: SubStatement, depth: Int): String = c match {
    case x: IfStatement => r + "if ("+condition(x.c)+")"+block(x.b, depth)
    case x: ElseIfStatement => r + "else if ("+condition(x.c)+")"+block(x.b, depth)
    case x: ElseStatement => r + "else "+block(x.b, depth)
  }
  
  def ifstatment(ifs: IfThenElseStatement, depth: Int): String = 
        ifs.clauses.foldLeft("")((result, current) => matchIfClause(result, current, depth))
    
  def block(b: Block, depth: Int): String = "{"+ls+"\t"+statements(b.l, (depth+1))+ls+Seq.fill(depth)("\t")+"}"
  
  def foreach(fe: Foreach, depth: Int): String = block(fe.block, depth)
  
  def matchStatement(r: String, s: Statement, depth: Int): String = s match {
    case x: IfThenElseStatement => r+ifstatment(x, depth)
    case x: Foreach => r+foreach(x, depth)
    case x: BooleanReturnStatement => r+x.b.toString()
    case x: TernaryReturnStatement => r+x.value.toString()
  }
    
  def statements(s: List[Statement], depth: Int): String = s.foldLeft("")((result, current) => matchStatement(result, current, depth))
  
  def equalsMethod(e: EqualsMethod): String =
    "equals("+e.paramName+") {"+ls+"\t"+statements(e.functionBody, 0)
       
  def compareMethod(c: CompareMethod): String = {
    "compare("+c.paramName+") {"+ls+"\t"//+e.functionBody.foldLeft("")((result, current))
  }

  def matchMethod(t: Method): String = t match {
    case x: EqualsMethod => equalsMethod(x)
    case x: CompareMethod => compareMethod(x)
    case x: MethodUndefined => ""
  }

  def matchSuperType(s: String): String = s match {
    case "" => ""
    case _ => " extends " + s
  }

  def tableDef(t: TableDef): String = "table " + t.id + " {" + matchTypes(t.typeList) + "\n}"
  
  def objectDef(t: SingletonDef): String = "object " + t.typeName + " {" + matchTypes(t.typeList) +ls+"}"

  def codeableDef(t: Codeable): String =
    "codeable " + t.typeName + matchSuperType(t.superType) + " {" + matchTypes(t.typeList) + matchMethod(t.method) +
    matchMethod(t.method1) + ls + "}"

  override def build(t: ArgotParseNode): String = t match {
    case x: TableDef => tableDef(x)
    case x: SingletonDef => objectDef(x)
    case x: ArgotSpecialType => matchSpecialType(x)
    case x: Codeable => codeableDef(x)
  }
}
