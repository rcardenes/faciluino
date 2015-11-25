from __future__ import print_function

from pyparsing import Forward, Keyword, Literal, Word
from pyparsing import Combine, Group, MatchFirst, Suppress
from pyparsing import OneOrMore, Optional, Or
from pyparsing import alphas, alphanums, nums, quotedString
from pyparsing import delimitedList, oneOf, operatorPrecedence, opAssoc
from pyparsing import ParseResults

bops       = {'<', '<=', '>', '>=', '='}

class LangComp(object):
    pass

class Expression(LangComp):
    def __init__(self, str_, loc, toks):
        if isinstance(toks[0], (LangComp, int)):
            self._cont = toks[0]
        else:
            self._cont = tuple(x for x in toks[0])

    def __repr__(self):
        return "<Exp {}>".format(str(self))

    def __str__(self):
        if isinstance(self._cont, (list, tuple)):
            return "(" + " ".join(str(x) for x in self._cont) + ")"
        else:
            return str(self._cont)

class IfSentence(LangComp):
    def __init__(self, str_, loc, toks):
        self._cond = None
        self._then = None
        self._else = None

class Use(LangComp):
    def __init__(self, str_, loc, toks):
        self._name = toks[0][1:-1].strip()

    def __repr__(self):
        return "<Use {!r}>".format(self._name)

class FuncAppl(LangComp):
    def __init__(self, str_, loc, toks):
        self._name = toks[0][0]
        self._args = toks[0][1]

    def __repr__(self):
        return "<FApp {}({})>".format(self._name, self._args)

    def __str__(self):
        try:
            s = ', '.join(self._args._cont)
        except AttributeError:
            s = self._args
        return "{}({})".format(self._name, s)

class Program(LangComp):
    def __init__(self, str_, loc, toks):
        pass

"""
coment     ::= '#' ......
usar       ::=  'usar' string
def-func   ::= 'funcion' ident ( '(' ident ( ',' ident )* ')' )? bloque-ini
para       ::= 'para' ident 'en' '(' expr (',' expr)* ')' bloque-hac
mientras   ::= 'mientras' '(' expr ')' bloque-hac
devolver   ::= 'devolver' '(' expr ')'
cond       ::= 'si' '(' expr ')' 'entonces' bloque ( 'sino' bloque ) fin
bloque-ini ::= 'inicio' bloque 'fin'
bloque-hac ::= 'hacer' bloque 'fin'
bloque     ::= sentencia+
sentencia  ::= para | mientras | devolver | cond | bloque-ini
"""

kwlist = (
    'usar', 'funcion', 'para', 'mientras',
    'devolver', 'si', 'entonces', 'sino',
    'hacer', 'inicio', 'fin'
)

keyword    = MatchFirst(map(Keyword, kwlist))

boolops    = oneOf(list(bops))
plusorminus= oneOf('+ -')
pmops      = oneOf('+ -')
mdops      = oneOf('* / //')
point      = Literal('.')
e          = oneOf('e E')

lb         = Suppress(Literal('('))
rb         = Suppress(Literal(')'))
SK         = lambda k: Suppress(Keyword(k))

ident      = ~keyword + Word(initChars = alphas + '+', bodyChars = alphanums + '_')
idents     = delimitedList(ident, delim=',')

expr       = Forward()
number     = Word(nums)
integercst = Combine(Optional(plusorminus) + number)
integer    = integercst.setParseAction(lambda t: int(t[0]))
floatp     = Combine(integercst +
                      Optional( point + Optional(number) ) +
                      Optional( e + integercst ) ).setParseAction(lambda t: float(t[0]))
const_dict = {'Cierto': True, 'Falso': False}
const      = oneOf('Cierto Falso').setParseAction(lambda t: const_dict[t[0]])

expressions= delimitedList(expr, delim=',')
aplfn      = Group(ident + lb + expressions + rb).setParseAction(FuncAppl)

parenexp   = (lb + expr + rb).setParseAction(lambda t: t[0])
expr       << operatorPrecedence(
                    aplfn|integer|floatp|const|ident|parenexp,
                    [ (mdops, 2, opAssoc.LEFT),
                      (pmops, 2, opAssoc.LEFT),
                      (boolops, 2, opAssoc.LEFT) ]
              ).setParseAction(Expression)

bloque     = Forward() # we need this to make a recursive block
bloque_hac = Forward()

para       = SK('para') + ident + (lb + expressions + rb | ident) + bloque_hac
mientras   = SK('mientras') + expr + bloque_hac
devolver   = SK('devolver') + expr
cond       = SK('si') + parenexp + SK('entonces') + bloque + Optional(SK('sino') + bloque) + SK('fin')
asign      = ident + Suppress(Literal('<-')) + expr

bloque_ini = SK('inicio') + bloque + SK('fin')
bloque_hac << SK('hacer') + bloque + SK('fin')
bloque     << OneOrMore(para | mientras | devolver | cond | asign)

usar       = SK('usar') + quotedString.setParseAction(Use)

func_sig   = ident.setName('func_name') + Optional(lb + idents.setName('func_params') + rb)
def_func   = SK('funcion') + func_sig + bloque_ini.setName('func_block')

programa   = OneOrMore(usar | def_func).setParseAction(Program)

ejemplo = """
usar "SD"
usar "WiFi"

funcion add1 (n)
inicio
  devolver n + 1
fin

funcion foo ( a, bar, baz )
inicio
  b <- 0
  mientras ( b < 1 ) hacer
    b <- add1(b) * ( 1+2 )
  fin

  devolver 2
fin
"""

print (programa.parseString(ejemplo, parseAll=True))
