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

  def foldTypes(t: List[ArgotType]): String = {
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
  
  def binaryFunction(lhs: Reference, op: String, rhs: Reference): String = matchReference(lhs)+op+matchReference(rhs)
  
  def matchBooleanFunction(bf: BooleanFunction): String = bf match {
    case x: Negation => "!("+matchBooleanFunction(x.f)+")"
    case x: Less => binaryFunction(x.lhs, " < ", x.rhs)
    case x: LessOrEqual => binaryFunction(x.lhs, " <= ", x.rhs)
    case x: EqualEqual => binaryFunction(x.lhs, " == ", x.rhs) 
    case x: NotEqual => binaryFunction(x.lhs, " != ", x.rhs)
    case x: GreaterOrEqual => binaryFunction(x.lhs, " >= ", x.rhs)
    case x: Greater => binaryFunction(x.lhs, " > ", x.rhs)
    case x: EqualsObject => ""
    case x: And => matchBooleanFunction(x.lhs)+" && "+matchBooleanFunction(x.rhs)
    case x: Or => matchBooleanFunction(x.lhs)+" || "+matchBooleanFunction(x.rhs)
  }
    
  def condition(c: Condition): String = matchBooleanFunction(c.f)
  
  def repeat(s: String, depth: Int): String = depth match { 
    case 1 => s
    case _ => s+repeat(s, depth-1)
  }
  
  def matchIfClause(c: SubStatement, depth: Int): String = c match {
    case x: IfStatement => repeat("\t", depth)+"if ("+condition(x.c)+")"+block(x.b, depth)
    case x: ElseIfStatement => repeat("\t", depth)+"else if ("+condition(x.c)+")"+block(x.b, depth)
    case x: ElseStatement => repeat("\t", depth)+"else "+block(x.b, depth)
  }
  
  def ifstatment(ifClauses: List[SubStatement], depth: Int): String = 
        ifClauses.foldLeft("")((result, current) => matchIfClause(current, depth))
    
  def block(b: Block, depth: Int): String = "{"+ls+statements(b.l, (depth+1))+repeat("\t", depth)+"}"+ls
  
  def foreach(fe: Foreach, depth: Int): String = block(fe.block, depth)
    
  def matchStatement(s: Statement, depth: Int): String = s match {
    case x: IfThenElseStatement => ifstatment(x.clauses, depth)
    case x: Foreach => foreach(x, depth)
    case x: BooleanReturnStatement => repeat("\t", depth)+"return "+x.b.toString()+ls
    case x: TernaryReturnStatement => repeat("\t", depth)+x.value.toString()+ls
  }
    
  def statements(s: List[Statement], depth: Int): String = 
    s.foldLeft("")((result,current) => matchStatement(current, depth))
  
  def method(methodName: String, paramName: String, body: List[Statement]): String = 
    repeat(ls,2)+"\t"+methodName+"("+paramName+") {"+ls+statements(body, 2)+"\t}"
  
  def equalsMethod(e: EqualsMethod): String = method("equals", e.paramName, e.functionBody)

  def compareMethod(c: CompareMethod): String = method("compare", c.paramName, c.functionBody)

  def matchMethod(t: Method): String = t match {
    case x: EqualsMethod => equalsMethod(x)
    case x: CompareMethod => compareMethod(x)
    case x: MethodUndefined => ""
  }

  def matchSuperType(s: String): String = s match {
    case "" => ""
    case _ => " extends " + s
  }

  def tableDef(t: TableDef): String = "table " + t.id + " {" + foldTypes(t.typeList) + "\n}"
  
  def objectDef(t: SingletonDef): String = "object " + t.typeName + " {" + foldTypes(t.typeList) +ls+"}"

  def codeableDef(t: Codeable): String =
    "codeable " + t.typeName + matchSuperType(t.superType) + " {" + foldTypes(t.typeList) + matchMethod(t.method) +
    matchMethod(t.method1) + ls + "}"

  override def build(t: ArgotParseNode): String = t match {
    case x: TableDef => tableDef(x)
    case x: SingletonDef => objectDef(x)
    case x: ArgotSpecialType => matchSpecialType(x)
    case x: Codeable => codeableDef(x)
  }
}
