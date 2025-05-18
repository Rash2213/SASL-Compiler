package parser

enum GrammarSymbolGeneric[Token <: scala.reflect.Enum, NonTerminal <: scala.reflect.Enum]:
  case T(token: Token)
  case NT(nonTerminal: NonTerminal)

class ParserGenerator[Token <: scala.reflect.Enum, NonTerminal <: scala.reflect.Enum]:
  type GrammarSymbol = GrammarSymbolGeneric[Token, NonTerminal]
  /** The GrammarSymbols a NT can be replaced with */
  type Derivation = Array[GrammarSymbol]
  /** The different derivations a single NT can take */
  type Derivations = Array[Derivation]
  /** a map from NT to its possible derivations */
  type DerivationsMap = Array[Derivations]

  /** a map from NT to its possible starting symbols */
  type FirstMap = Array[Set[Token]]
  /** a map from NT to its possible following symbols */
  type FollowMap = Array[Set[Token]]
  /** a map from NT to whether that NT can be empty */
  type EmptyMap = Array[Boolean]
  /** a map from NT to whether the input can end after the NT */
  type FinalMap = Array[Boolean]

  /** a map from NT and T to Derivation */
  type ParseTable = Array[Array[Option[Derivation]]]

  def first(numNonTerminals: Int, dm: DerivationsMap, em: EmptyMap): (FirstMap, EmptyMap) = {
    val firstMap = Array.fill(numNonTerminals)(Set[Token]())
    //val canBeEmpty = Array.fill(numNonTerminals)(false)

    var changed = true
    while (changed) {
      changed = false
      for (ord <- 0 until numNonTerminals) {
        for (prod <- dm(ord)) {
          prod(0) match {
            case GrammarSymbolGeneric.T(sym) =>
              if (!firstMap(ord).contains(sym)) {
                changed = true
                firstMap(ord) = firstMap(ord).incl(sym)
              }
            case GrammarSymbolGeneric.NT(sym) =>
              if (!firstMap(sym.ordinal).subsetOf(firstMap(ord))) {
                changed = true
                firstMap(ord) = firstMap(ord).concat(firstMap(sym.ordinal))
              }
              // Left-most Symbol can only be empty if it is the only symbol
              if (!em(ord) && em(sym.ordinal)) {
                changed = true
                em(ord) = true
              }
          }
        }
      }
    }

    (firstMap, em)
  }

  def follow(numNonTerminals: Int, dm: DerivationsMap, em: EmptyMap, firstMap: FirstMap): FollowMap = {
  //private def follow(numNonTerminals: Int, dm: DerivationsMap, em: EmptyMap, firstMap: FirstMap, fm: FinalMap): (FollowMap, FinalMap) = {
    val followMap = Array.fill(numNonTerminals)(Set[Token]())

    var changed = true
    while (changed) {
      changed = false
      for (ord <- 0 until numNonTerminals) {
        for (prod <- dm(ord)) {
          var trailer = followMap(ord)
          //var trailerEnd = fm(ord)
          for (i <- prod.length - 1 to 0 by -1) {
            prod(i) match {
              case GrammarSymbolGeneric.NT(nt) =>
                if (!trailer.subsetOf(followMap(nt.ordinal))) {
                  changed = true
                  followMap(nt.ordinal) = followMap(nt.ordinal).concat(trailer)
                }
                /*if (trailerEnd && !fm(nt.ordinal)) {
                  changed = true
                  fm(nt.ordinal) = true
                }*/
                if (em(nt.ordinal)) {
                  trailer = trailer.concat(firstMap(nt.ordinal))
                } else {
                  trailer = firstMap(nt.ordinal)
                  //trailerEnd = false
                }
              case GrammarSymbolGeneric.T(t) =>
                trailer = Set(t)
            }
          }
        }
      }
    }

    //(followMap, fm)
    followMap
  }

  def oracleTable(numTokens: Int, numNonTerminals: Int, dm: DerivationsMap, emi: EmptyMap/*, fmi: FinalMap*/): ParseTable = {
    val (firstMap, em) = first(numNonTerminals, dm, emi)
    //val (followMap, fm) = follow(numNonTerminals, dm, em, firstMap, fmi)
    val followMap = follow(numNonTerminals, dm, emi, firstMap)
    val empty: Derivation = Array()

    val tbl = Array.fill[Option[Derivation]](numNonTerminals, numTokens) {
      None
    }

    for (ord <- 0 until numNonTerminals) {
      for (prod <- dm(ord)) {
        prod(0) match {
          case GrammarSymbolGeneric.NT(nt) =>
            for (t <- firstMap(nt.ordinal)) {
              tbl(ord)(t.ordinal) = Some(prod)
            }
          case GrammarSymbolGeneric.T(t) =>
            tbl(ord)(t.ordinal) = Some(prod)
        }
      }
      if (em(ord)) {
        for (t <- followMap(ord)) {
          tbl(ord)(t.ordinal) = Some(empty)
        }
      }
    }

    tbl
  }
