package parser

object SaslData:
  enum NonTerminal:
    case System
    case FuncDefsP
    case DefsP
    case Abstraction
    case ExprP
    case CondExpr
    case ListExpr
    case ListExprP
    case OpExprP
    case ConjunctP
    case ComparP
    case AddP
    case MulP
    case Factor
    case CombP
    case Simple
    case ListP
    case ListElemsP

  val emMap: gen.EmptyMap = Array(
    // System
    false,
    // FuncDefsP
    true,
    // DefsP
    true,
    // Abstraction
    false,
    // ExprP
    true,
    // CondExpr
    false,
    // ListExpr
    false,
    // ListExprP
    true,
    // OpExprP
    true,
    // ConjunctP
    true,
    // ComparP
    true,
    // AddP
    true,
    // MulP
    true,
    // Factor
    false,
    // CombP
    true,
    // Simple
    false,
    // ListP
    false,
    // ListElemsP
    true,
  )

  val gen: ParserGenerator[lexer.Token, NonTerminal] = ParserGenerator()

  import lexer.Token.*
  import NonTerminal.*
  import GrammarSymbolGeneric.*

  val derMap: gen.DerivationsMap = Array(
    //  ⟨system⟩ → def id ⟨abstraction⟩ ⟨expr’⟩ ⟨funcdefs’⟩ . ⟨condexpr⟩ ⟨expr’⟩
    //    | ⟨condexpr⟩ ⟨expr’⟩
    Array(
      Array[gen.GrammarSymbol](T(KDef), T(Id("")), NT(Abstraction), NT(ExprP), NT(FuncDefsP), T(KDot), NT(CondExpr), NT(ExprP)),
      Array[gen.GrammarSymbol](NT(CondExpr), NT(ExprP))
    ),
    //  ⟨funcdefs’⟩ → def id ⟨abstraction⟩ ⟨expr’⟩ ⟨funcdefs’⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](T(KDef), T(Id("")), NT(Abstraction), NT(ExprP), NT(FuncDefsP))),
    //  ⟨defs’⟩ → ; id ⟨abstraction⟩ ⟨defs’⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](T(KSemicolon), T(Id("")), NT(Abstraction), NT(DefsP))),
    //  ⟨abstraction⟩ → = ⟨condexpr⟩
    //    | id ⟨abstraction⟩
    Array(
      Array[gen.GrammarSymbol](T(SEqual), NT(CondExpr)/*, NT(ExprP)*/),
      Array[gen.GrammarSymbol](T(Id("")), NT(Abstraction)),
    ),
    //  ⟨expr’⟩ → where id ⟨abstraction⟩ ⟨defs’⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](T(KWhere), T(Id("")), NT(Abstraction), NT(DefsP))),
    //  ⟨condexpr⟩ → if ⟨condexpr⟩ ⟨expr’⟩ then ⟨condexpr⟩ else ⟨condexpr⟩
    //  | ⟨listexpr⟩
    Array(
      Array[gen.GrammarSymbol](T(KIf), NT(CondExpr), NT(ExprP), T(KThen), NT(CondExpr), T(KElse), NT(CondExpr)),
      Array[gen.GrammarSymbol](NT(ListExpr)),
    ),
    //  ⟨listexpr⟩ → ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩ ⟨opexpr’⟩ ⟨listexpr’⟩
    Array(Array[gen.GrammarSymbol](NT(Factor), NT(MulP), NT(AddP), NT(ComparP), NT(ConjunctP), NT(OpExprP), NT(ListExprP))),
    //  ⟨listexpr’⟩ → : ⟨listexpr⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](T(KColon), NT(ListExpr))),
    //  ⟨opexpr’⟩ → or ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩ ⟨opexpr’⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](T(SOr), NT(Factor), NT(MulP), NT(AddP), NT(ComparP), NT(ConjunctP), NT(OpExprP))),
    //  ⟨conjunct’⟩ → and ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](T(SAnd), NT(Factor), NT(MulP), NT(AddP), NT(ComparP), NT(ConjunctP))),
    //  ⟨compar’⟩ → = ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
    //    | ~= ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
    //    | < ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
    //    | > ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
    //    | <= ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
    //    | >= ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
    //    | ε
    Array(
      Array[gen.GrammarSymbol](T(SEqual), NT(Factor), NT(MulP), NT(AddP), NT(ComparP)),
      Array[gen.GrammarSymbol](T(SNotEqual), NT(Factor), NT(MulP), NT(AddP), NT(ComparP)),
      Array[gen.GrammarSymbol](T(SLess), NT(Factor), NT(MulP), NT(AddP), NT(ComparP)),
      Array[gen.GrammarSymbol](T(SGreater), NT(Factor), NT(MulP), NT(AddP), NT(ComparP)),
      Array[gen.GrammarSymbol](T(SLessEqual), NT(Factor), NT(MulP), NT(AddP), NT(ComparP)),
      Array[gen.GrammarSymbol](T(SGreaterEqual), NT(Factor), NT(MulP), NT(AddP), NT(ComparP)),
    ),
    //  ⟨add’⟩ → + ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩
    //    | - ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩
    //    | ε
    Array(
      Array[gen.GrammarSymbol](T(SPlus), NT(Factor), NT(MulP), NT(AddP)),
      Array[gen.GrammarSymbol](T(SMinus), NT(Factor), NT(MulP), NT(AddP))
    ),
    //  ⟨mul’⟩ → * ⟨factor⟩ ⟨mul’⟩
    //    | / ⟨factor⟩ ⟨mul’⟩
    //    | ε
    Array(
      Array[gen.GrammarSymbol](T(SMul), NT(Factor), NT(MulP)),
      Array[gen.GrammarSymbol](T(SDiv), NT(Factor), NT(MulP)),
    ),
    //  ⟨factor⟩ → + ⟨simple⟩ ⟨comb’⟩
    //    | - ⟨simple⟩ ⟨comb’⟩
    //    | not ⟨simple⟩ ⟨comb’⟩
    //    | ⟨simple⟩ ⟨comb’⟩
    Array(
      Array[gen.GrammarSymbol](T(SPlus), NT(Simple), NT(CombP)),
      Array[gen.GrammarSymbol](T(SMinus), NT(Simple), NT(CombP)),
      Array[gen.GrammarSymbol](T(SNot), NT(Simple), NT(CombP)),
      Array[gen.GrammarSymbol](NT(Simple), NT(CombP)),
    ),
    //  ⟨comb’⟩ → ⟨simple⟩ ⟨comb’⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](NT(Simple), NT(CombP))),
    //  ⟨simple⟩ → id
    //    | num
    //    | bool
    //    | string
    //    | nil
    //    | [ ⟨list’⟩
    //    | ( ⟨condexpr⟩ ⟨expr’⟩ )
    // extended for anonymous functions
    //    | { ⟨abstraction⟩ }
    Array(
      Array[gen.GrammarSymbol](T(Id(""))),
      Array[gen.GrammarSymbol](T(CNum(0))),
      Array[gen.GrammarSymbol](T(CBool(false))),
      Array[gen.GrammarSymbol](T(CString(""))),
      Array[gen.GrammarSymbol](T(CNil)),
      Array[gen.GrammarSymbol](T(KOpenBracket), NT(ListP)),
      Array[gen.GrammarSymbol](T(KOpenParen), NT(CondExpr), NT(ExprP), T(KCloseParen)),
      // extended for anonymous functions
      Array[gen.GrammarSymbol](T(KOpenCurlyBracket), NT(CondExpr), T(KCloseCurlyBracket)),
    ),
    //  ⟨list’⟩ → ]
    //    | ⟨condexpr⟩ ⟨expr’⟩ ⟨listelems’⟩ ]
    Array(
      Array[gen.GrammarSymbol](T(KCloseBracket)),
      Array[gen.GrammarSymbol](NT(CondExpr), NT(ExprP), NT(ListElemsP), T(KCloseBracket))
    ),
    //  ⟨listelems’⟩ → , ⟨condexpr⟩ ⟨expr’⟩ ⟨listelems’⟩
    //    | ε
    Array(Array[gen.GrammarSymbol](T(KComma), NT(CondExpr), NT(ExprP), NT(ListElemsP)))
  )