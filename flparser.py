from __future__ import print_function

import sys

from pyparsing import Forward, Keyword, Literal, Word
from pyparsing import Combine, Group, MatchFirst, Suppress
from pyparsing import OneOrMore, Optional, Or
from pyparsing import alphas, alphanums, nums, quotedString
from pyparsing import delimitedList, oneOf, operatorPrecedence, opAssoc
from pyparsing import ParseResults

bops       = {'<', '<=', '>', '>=', '='}

def abort(msg):
    print(msg)
    sys.exit(-1)

class Code(list):
    def __init__(self, *args):
        super(Code, self).__init__(*args)

        self._ind = 0

    def indent(self):
        self._ind += 4
        for s in self:
            if isinstance(s, Code):
                s.indent()

        return self

    def append(self, i):
        super(Code, self).append(i)

    def __add__(self, other):
        c = Code(list(self) + list(other))
        c._ind = max(self._ind, other._ind)

        return c

    def __str__(self):
        ind = self._ind * ' '
        return '\n'.join((str(s) if isinstance(s, Code) else (ind + s)) for s in self)

def safe_generate(obj, table):
    try:
        return obj.generate(table)
    except AttributeError:
        return (Code(), {})

class LangComp(object):
    @property
    def type(self):
        return self._type

    @property
    def name(self):
        return self._name

    def generate(self, table):
        pass

def get_c_type(exp):
    return {
        'integer': 'int',
        'float'  : 'float',
        None     : '<UNKNOWN>'
    }[exp]

def get_type(exp):
    if type(exp) == int:
        return 'integer'
    elif type(exp) == float:
        return 'float'
    else:
        try:
            return exp.type
        except AttributeError:
            print (dir(exp))
            abort('Unknown type: {}'.format(exp))

def coerce_types(t1, t2):
    if t1 == t2:
        return t1
    elif t1 is None:
        return t2
    elif t2 is None:
        return t1
    elif 'integer' in (t1, t2) and 'float' in (t1, t2):
        return 'float'

    raise RuntimeError('Incompatible types {}, {}'.format(t1, t2))

class Ident(LangComp):
    def __init__(self, str_, loc, toks):
        self._name = toks[0]
        self._type = None

    def __repr__(self):
        return "<Ident {}>".format(self.name)

    def __str__(self):
        return self._name

class Expression(LangComp):
    def __init__(self, str_, loc, toks):
        if isinstance(toks[0], (LangComp, int, float)):
            self._cont = toks[0]
            self._type = get_type(self._cont)
        else:
            self._cont = tuple(x for x in toks[0])
            # This assumes an infix operand of 2-arity
            t1, t2 = get_type(self._cont[0]), get_type(self._cont[2])
            self._type = coerce_types(t1, t2)

    def generate(self, table):
        if isinstance(self._cont, (list, tuple)):
            safe_generate(self._cont[0], table)
            safe_generate(self._cont[2], table)
        else:
            safe_generate(self._cont, table)

        return (str(self), {})

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
    'usar "FooBar"'
    def __init__(self, str_, loc, toks):
        self._name = toks[0][1:-1].strip()

    def __repr__(self):
        return "<Use {!r}>".format(self._name)

class While(LangComp):
    "mientras <bool> hacer <bloque> fin"
    def __init__(self, str_, loc, toks):
        self._cond = toks[0][0]
        self._body = toks[0][1]

    def generate(self, table):
        table = table.copy()
        collect = Code()
        collect.append('while ({}) {{'.format(str(self._cond)))
        code, exports = self._body.generate(table)
        collect.append(code.indent())
        collect.append('}')

        return (collect, {})

class Assign(LangComp):
    "ident <- expr"
    def __init__(self, str_, loc, toks):
        self._var = toks[0][0]
        self._exp = toks[0][1]
        self._type = None

    @property
    def name(self):
        return self._var.name

    def generate(self, table):
        exp_code, exports = self._exp.generate(table.copy())
        if self._var.name not in table:
            self._var._type = self._exp.type
        else:
            self._var._type = coerce_types(self._type, self._exp.type)

        exports.update({'__assign__': self._var})
        code = Code()
        code.append("{} = {};".format(self._var, exp_code))

        return (code, exports)

class Return(LangComp):
    def __init__(self, str_, loc, toks):
        self._exp = toks[0][0]
        self._type = None

    def generate(self, table):
        self._exp.generate(table.copy())
        self._type = self._exp.type
        code = Code()
        code.append("return {};".format(self._exp))
        return (code, {})

    def __repr__(self):
        return "<Ret {}>".format(self._exp)

class FuncAppl(LangComp):
    def __init__(self, str_, loc, toks):
        self._var = toks[0][0]
        self._args = toks[0][1]
        self._type = None

    @property
    def name(self):
        return self._var.name

    def __repr__(self):
        return "<FApp {}({})>".format(self.name, self._args)

    def __str__(self):
        try:
            s = ', '.join(self._args._cont)
        except (AttributeError, TypeError):
            s = self._args
        return "{}({})".format(self.name, s)

    def generate(self, table):
        if self.name not in table:
            abort('Undefined {}'.format(self.name))

class Block(LangComp):
    def __init__(self, str_, loc, toks):
        self._toks = toks[0]

    def generate(self, table):
        table = table.copy()
        exports = {}
        collect = Code()

        defs = Code()

        for t in self._toks:
            code, exports = t.generate(table)
            collect.append(code)
            for (k, v) in exports.items():
                if k == '__assign__' and v.name not in table:
                    defs.append('{} {};'.format(get_c_type(v.type), v.name))
                    table[v.name] = v

        return (defs + collect, exports)

class FuncDef(LangComp):
    def __init__(self, str_, loc, toks):
        self._fname = toks[0][0]
        self._args  = toks[0][1]
        self._body  = toks[0][2]
        self._type  = None

    def __repr__(self):
        return '<Fun {}({})>'.format(self.name, ', '.join(a.name for a in self._args))

    @property
    def name(self):
        return str(self._fname.name)

    def generate(self, table):
        table = table.copy()
        table.update(dict((a.name, a) for a in self._args))
        code = Code()
        code.append('{} {} ({}) {{'.format(get_c_type(self.type), self.name, ', '.join('{} {}'.format(get_c_type(a.type), a) for a in self._args)))
        block_code, exports = self._body.generate(table)
        code.append(block_code.indent())
        code.append('}')

        print(code)

class Program(LangComp):
    def __init__(self, str_, loc, toks):
        self._toks = toks

    def generate(self):
        table = {}
        for t in self._toks:
            try:
                table[t.name] = t
            except AttributeError as e:
                print(e)

        for t in self._toks:
            t.generate(table)

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

keyword     = MatchFirst(map(Keyword, kwlist))

boolops     = oneOf(list(bops))
plusorminus = oneOf('+ -')
pmops       = oneOf('+ -')
mdops       = oneOf('* / //')
point       = Literal('.')
e           = oneOf('e E')

lb          = Suppress(Literal('('))
rb          = Suppress(Literal(')'))
SK          = lambda k: Suppress(Keyword(k))

ident       = (~keyword + Word(initChars = alphas + '+', bodyChars = alphanums + '_')).setName("ident").setParseAction(Ident)
idents      = delimitedList(ident, delim=',')

expr        = Forward().setName('expr')
number      = Word(nums)
integercst  = Combine(Optional(plusorminus) + number)
integer     = integercst.setParseAction(lambda t: int(t[0]))
floatp      = Combine(integercst +
                      Optional( point + Optional(number) ) +
                      Optional( e + integercst ) ).setParseAction(lambda t: float(t[0]))
const_dict  = {'Cierto': True, 'Falso': False}
const       = oneOf('Cierto Falso').setParseAction(lambda t: const_dict[t[0]])

expressions = delimitedList(expr, delim=',')
aplfn       = Group(ident + lb + expressions + rb).setParseAction(FuncAppl)

parenexp    = (lb + expr + rb).setParseAction(lambda t: t[0])
expr       << operatorPrecedence(
                    aplfn|integer|floatp|const|ident|parenexp,
                    [ (mdops, 2, opAssoc.LEFT),
                      (pmops, 2, opAssoc.LEFT),
                      (boolops, 2, opAssoc.LEFT) ]
              ).setParseAction(Expression)

bloque      = Forward() # we need this to make a recursive block
bloque_hac  = Forward()

para        = Group(SK('para') + ident + (lb + expressions + rb | ident) + bloque_hac)
mientras    = Group(SK('mientras') + expr + bloque_hac).setParseAction(While)
devolver    = Group(SK('devolver') + expr).setParseAction(Return)
cond        = Group(SK('si') + parenexp + SK('entonces') + bloque + Optional(SK('sino') + bloque) + SK('fin'))
asign       = Group(ident + Suppress(Literal('<-')) + expr).setParseAction(Assign)

bloque_ini  = SK('inicio') + bloque + SK('fin')
bloque_hac << SK('hacer') + bloque + SK('fin')
bloque     << Group(OneOrMore(para | mientras | devolver | cond | asign)).setParseAction(Block)

usar        = SK('usar') + quotedString.setParseAction(Use)

func_sig    = ident + Group(Optional(lb + idents + rb))
def_func    = Group(SK('funcion') + func_sig + bloque_ini).setParseAction(FuncDef)

programa    = OneOrMore(usar | def_func).setParseAction(Program)

ejemplo = """
usar "SD"
usar "WiFi"

funcion add1 (n)
inicio
  devolver n + 1
fin

funcion foo (a, bar, baz)
inicio
  b <- 0
  mientras b < 1 hacer
    b <- add1(b) * ( 1+2 )
  fin

  devolver 2
fin
"""

if __name__ == '__main__':
    prog = programa.parseString(ejemplo, parseAll=True)
    prog[0].generate()
