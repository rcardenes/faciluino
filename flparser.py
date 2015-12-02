from __future__ import print_function

import sys

from pyparsing import Forward, Keyword, Literal, Word
from pyparsing import Combine, Group, MatchFirst, Suppress
from pyparsing import OneOrMore, Optional, Or
from pyparsing import alphas, alphanums, nums, quotedString
from pyparsing import delimitedList, oneOf, operatorPrecedence, opAssoc
from pyparsing import ParseResults

from collections import defaultdict

bops         = {'<', '<=', '>', '>=', '='}

# The entry points are functions that are never invoked explicitly, like
# 'main' in C or 'setup' and 'loop' for Arduino. We need to keep track
# of them, because we don't generate code for other non-invoked functions
entry_points = {'preparacion', 'bucle_central'}

def abort(msg):
    print(msg)
    sys.exit(-1)

class VariableNameGenerator(object):
    def __init__(self):
        self.last_counter = 0
        self.last_iterable = 0

    def nextCounter(self):
        self.last_counter += 1
        return '_c{:04d}'.format(self.last_counter)

    def nextIterable(self):
        self.last_iterable += 1
        return '_i{:04d}'.format(self.last_iterable)

    def userIdent(self, name):
        return '_u_{}'.format(name)

varGen = VariableNameGenerator()

class Attribs(dict):
    def __getitem__(self, key):
        try:
            return super(Attribs, self).__getitem__(key)
        except KeyError:
            new = []
            self[key] = new

            return new

    def update(self, d):
        for (k, v) in d.items():
            self[k].extend(v)

    def copy(self):
        return Attribs(super(Attribs, self).copy())

class Code(list):
    def __init__(self, *args):
        super(Code, self).__init__(*args)

        self._ind = 0
        self._delim = '\n'

    def set_delim(self, d):
        self._delim = d

        return self

    def indent(self):
        self._ind += 4
        for s in self:
            if isinstance(s, Code):
                s.indent()

        return self

    def append(self, i):
        super(Code, self).append(i)

        return self

    def __add__(self, other):
        c = Code(list(self) + list(other))
        c._ind = max(self._ind, other._ind)
        c._delim = self._delim

        return c

    def __str__(self):
        ind = self._ind * ' '
        return self._delim.join((str(s) if isinstance(s, Code) else (ind + s)) for s in self)

def safe_generate(obj, table, verb = False, delim='\n'):
    try:
        return obj.generate(table)
    except AttributeError:
        return (Code() if not verb else Code([str(obj)])).set_delim(delim)

def safe_get_attrs(obj):
    try:
        return obj.attrs
    except AttributeError:
        return Attribs()

class LangComp(object):
    @property
    def attrs(self):
        return self._attr.copy()

    @property
    def type(self):
        return self._type

    @property
    def name(self):
        return self._name

    def generate(self, table):
        return Code()

def get_c_type(exp):
    return {
        'integer': 'int',
        'float'  : 'float',
        'boolean': 'bool',
        None     : '<UNKNOWN>'
    }[exp]

def get_type(exp):
    if type(exp) == int:
        return 'integer'
    elif type(exp) == float:
        return 'float'
    elif type(exp) == bool:
        return 'boolean'
    elif type(exp) == str:
        return 'string'
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

class Collection(LangComp):
    def __init__(self, str_, loc, toks):
        self._type = None
        self._toks = tuple(toks)
        assert len(set(t.type for t in self._toks)) == 1
        self._type = self._toks[0].type
        self._attr = Attribs()

    def __str__(self):
        return ', '.join(str(t) for t in self._toks)

    def generate(self, table):
        return Code([t.generate(table) for t in self._toks]).set_delim(',')

class Arbitrary(LangComp):
    def __init__(self, txt, tp=None, attr=None):
        self._txt = txt
        self._type = tp
        self._attr = attr or Attribs()

    def generate(self, table):
        return Code([self._txt])

class LiteralElement(LangComp):
    def __init__(self, str_, loc, toks):
        t = toks[0]
        self._cont = t
        self._type = get_type(t)
        self._attr = Attribs()

    def generate(self, table):
        return Code([str(self)]).set_delim(' ')

    def __str__(self):
        return str(self._cont)

class Ident(LangComp):
    def __init__(self, str_, loc, toks):
        self._name = toks[0]
        self._type = None
        self._attr = Attribs()

    def generate(self, table):
        return Code([varGen.userIdent(self.name)]).set_delim(' ')

    def __repr__(self):
        return "<Ident {}>".format(self.name)

    def __str__(self):
        return self._name

class Expression(LangComp):
    def __init__(self, str_, loc, toks):
        t = toks[0]

        if isinstance(t, (LangComp, int, float)):
            self._cont = t
            self._type = get_type(self._cont)
            self._attr = safe_get_attrs(t)
        else:
            self._cont = tuple(x for x in t)
            # This assumes an infix operand of 2-arity
            t1, t2 = get_type(self._cont[0]), get_type(self._cont[2])
            self._type = coerce_types(t1, t2)
            self._attr = safe_get_attrs(t1)
            self._attr.update(safe_get_attrs(t2))

    def generate(self, table):
        c = self._cont
        if isinstance(c, tuple):
            return Code([c[0].generate(table), c[1], c[2].generate(table)]).set_delim(' ')
        else:
            return c.generate(table).set_delim(' ')

    def __repr__(self):
        return "<Exp {}>".format(str(self))

    def __str__(self):
        if isinstance(self._cont, (list, tuple)):
            return "(" + " ".join(str(x) for x in self._cont) + ")"
        else:
            return str(self._cont)

class ForSentence(LangComp):
    def __init__(self, str_, loc, toks):
        t = toks[0]
        self._var  = t[0]
        self._iter = t[1][0]
        self._blk  = t[2]
        self._attr = Attribs({'assign': [self._var]})
        self._attr.update(self._iter.attrs)
        self._attr.update(self._blk.attrs)

    def generate(self, table):
        c = Code()
        counter = varGen.nextCounter()
        iterab  = varGen.nextIterable()
        c.append('int {it}[] = {{ {lst} }};'.format(it=iterab, lst=str(self._iter)))
        c.append('for(int {cn} = 0; {cn} < (sizeof({it})/sizeof(*{it})); {cn}++)'.format(cn=counter, it=iterab))
        self._blk.prepend_token(Arbitrary('{} = {}[{cn}];'.format(self._var.name, iterab, cn=counter)))
        c.append(self._blk.generate(table))

        return Code().append('{').append(c.indent()).append('}')

class IfSentence(LangComp):
    def __init__(self, str_, loc, toks):
        t = toks[0]
        self._cond = t[0]
        self._then = t[1]
        self._attr = Attribs()
        self._attr.update(t[0].attrs)
        self._attr.update(t[1].attrs)
        if len(t) == 3:
            self._else = t[2]
            self._attr.update(t[2].attrs)
        else:
            self._else = None

    def generate(self, table):
        c = Code()
        c.append('if ({})'.format(self._cond.generate(table)))
        c.append(self._then.generate(table))
        if self._else:
            c.append('else')
            c.append(self._else.generate(table))

        return c

class Use(LangComp):
    'usar "FooBar"'
    def __init__(self, str_, loc, toks):
        self._name = toks[0][1:-1].strip()
        self._attr = Attribs({'expose': [self]})

    def __repr__(self):
        return "<Use {!r}>".format(self._name)

class While(LangComp):
    "mientras <bool> hacer <bloque> fin"
    def __init__(self, str_, loc, toks):
        self._cond = toks[0][0]
        self._body = toks[0][1]

        self._attr = self._cond.attrs
        self._attr.update(self._body.attrs)

    def generate(self, table):
        table = table.copy()
        collect = Code()
        collect.append('while ({})'.format(str(self._cond.generate(table))))
        code = self._body.generate(table)
        collect.append(code)

        return collect

class Assign(LangComp):
    "ident <- expr"
    def __init__(self, str_, loc, toks):
        self._var = toks[0][0]
        self._exp = toks[0][1]
        self._type = None
        self._attr = self._exp.attrs
        self._attr.update({'assign': [self._var]})

    @property
    def name(self):
        return self._var.name

    def generate(self, table):
        exp_code = self._exp.generate(table.copy())
        if self._var.name not in table:
            self._var._type = self._exp.type
        else:
            self._var._type = coerce_types(self._type, self._exp.type)

        return Code().append("{} = {};".format(self._var.generate(table), exp_code))

class Return(LangComp):
    def __init__(self, str_, loc, toks):
        self._exp = toks[0][0]
        self._type = None
        self._attr = self._exp.attrs

    def generate(self, table):
        self._exp.generate(table.copy())
        self._type = self._exp.type
        return Code().append("return {};".format(self._exp.generate(table)))

    def __repr__(self):
        return "<Ret {}>".format(self._exp)

class FuncAppl(LangComp):
    def __init__(self, str_, loc, toks):
        t = toks[0]
        self._var = t[0]
        c = t[1]
        self._args = c[0]
        self._type = None
        self._attr = Attribs()
        self._attr.update(safe_get_attrs(self._args))
        self._attr['call'].append(self)

    @property
    def name(self):
        return self._var.name

    def __repr__(self):
        return "<FApp {}({})>".format(self.name, self._args)

    def __str__(self):
        return "{}({})".format(self.name, str(self._args))

    def generate(self, table):
        if self.name not in table:
            abort('Undefined {}'.format(self.name))
        return Code(['{}({})'.format(varGen.userIdent(self.name), self._args.generate(table))])

class Block(LangComp):
    def __init__(self, str_, loc, toks):
        self._toks = [t for t in toks[0]]
        self._attr = Attribs()

        for t in self._toks:
            self._attr.update(t.attrs)

    def prepend_token(self, snt):
        self._toks = [snt] + self._toks

    def generate(self, table, enclosed=True):
        table = table.copy()
        exports = {}
        collect = Code()

        defer_defs = []
        for t in self.attrs['assign']:
            if t.name not in table:
                table[t.name] = t
                defer_defs.append(t)

        for t in self._toks:
            code = t.generate(table)
            collect.append(code)

        defs = Code()
        for t in defer_defs:
            defs.append('{} {};'.format(get_c_type(t.type), t.generate(table)))

        final = defs + collect

        if enclosed:
            return Code(['{', final.indent(), '}'])
        else:
            return final

class FuncDef(LangComp):
    def __init__(self, str_, loc, toks):
        self._fname = toks[0][0]
        self._args  = toks[0][1]
        self._body  = toks[0][2]
        self._type  = None
        self._attr  = {}

    def __repr__(self):
        return '<Fun {}({})>'.format(self.name, ', '.join(a.name for a in self._args))

    @property
    def name(self):
        return str(self._fname.name)

    def generate(self, table):
        table = table.copy()
        table.update(dict((a.name, a) for a in self._args))
        code = Code()
        code.append('{} {} ({})'.format(get_c_type(self.type), self._fname.generate(table), ', '.join('{} {}'.format(get_c_type(a.type), a.generate(table)) for a in self._args)))
        block_code = self._body.generate(table)
        code.append(block_code)

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
    'hacer', 'inicio', 'fin', 'Cierto', 'Falso'
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

ident       = (~keyword + Word(initChars = alphas + '+', bodyChars = alphanums + '_')).setParseAction(Ident)
idents      = delimitedList(ident, delim=',')

expr        = Forward()
number      = Word(nums)
integercst  = Combine(Optional(plusorminus) + number)
integer     = integercst.setParseAction(lambda t: int(t[0])).addParseAction(LiteralElement)
floatp      = Combine(integercst +
                      Optional( point + Optional(number) ) +
                      Optional( e + integercst ) ).setParseAction(lambda t: float(t[0])).setParseAction(LiteralElement)
const_dict  = {'Cierto': True, 'Falso': False}
const       = oneOf('Cierto Falso').setParseAction(lambda t: const_dict[t[0]]).addParseAction(LiteralElement)

expressions = Group(delimitedList(expr, delim=',').setParseAction(Collection))
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

para        = Group(SK('para') + ident + SK('en') + (lb + expressions + rb | ident) + bloque_hac).setParseAction(ForSentence)
mientras    = Group(SK('mientras') + expr + bloque_hac).setParseAction(While)
devolver    = Group(SK('devolver') + expr).setParseAction(Return)
cond        = Group(SK('si') + expr + SK('entonces') + bloque + Optional(SK('sino') + bloque) + SK('fin')).setParseAction(IfSentence)
asign       = Group(ident + Suppress(Literal('<-')) + expr).setParseAction(Assign)

bloque_ini  = SK('inicio') + bloque + SK('fin')
bloque_hac << SK('hacer') + bloque + SK('fin')
bloque     << Group(OneOrMore(para | mientras | devolver | cond | asign | bloque_ini)).setParseAction(Block)

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

  si b > 5 entonces
    b <- 0
  sino
    b <- 10
  fin

  inicio
    c <- 13
  fin

  para k en (1, 2, 3, 4, 5) hacer
    b <- k
  fin

  devolver 2
fin
"""

if __name__ == '__main__':
    prog = programa.parseString(ejemplo, parseAll=True)
    prog[0].generate()
