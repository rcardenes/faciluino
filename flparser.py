from __future__ import print_function

import sys

from pyparsing import Forward, Keyword, Literal, Word
from pyparsing import Combine, Group, MatchFirst, Suppress
from pyparsing import OneOrMore, Optional, Or
from pyparsing import alphas, alphanums, nums, quotedString
from pyparsing import delimitedList, oneOf, operatorPrecedence, opAssoc
from pyparsing import ParseResults

from collections import defaultdict

from functools import wraps

bops         = {'<', '<=', '>', '>=', '='}

# The entry points are functions that are never invoked explicitly, like
# 'main' in C or 'setup' and 'loop' for Arduino. We need to keep track
# of them, because we don't generate code for other non-invoked functions
entry_points = {'setup', 'loop'}

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

class Scope(object):
    def __init__(self, higher = None):
        self.contents = {}
        self.higher = higher

    def __getitem__(self, item):
        try:
            return self.contents[item]
        except KeyError:
            try:
                return self.higher[item]
            except TypeError:
                raise KeyError("Object '{}' can't be found".format(item))

    def __setitem__(self, item, value):
        self.contents[item] = value

    def __contains__(self, item):
        try:
            return (item in self.contents) or (item in self.higher)
        except TypeError:
            return False

    def pop(self):
        return self.higher

def nested_scope(mth):
    @wraps(mth)
    def wrapper(self, table, *args, **kw):
        table.push()
        try:
            return mth(self, table, *args, **kw)
        finally:
            table.pop()
    return wrapper

class SymbolTable(object):
    def __init__(self):
        self.functions = Scope()
        self.other     = Scope()

    def __contains__(self, item):
        return item in self.other or item in self.functions

    def __getitem__(self, item):
        return self.get(item)

    def __setitem__(self, item, value):
        self.set(item, value)

    def get(self, item):
        return self.other[item]

    def set(self, item, value):
        self.other[item] = value

    def getfn(self, item, signature):
        raise NotImplementedError('SymbolTable.getfn is not implemented')

    def setfn(self, item, signature, value):
        raise NotImplementedError('SymbolTable.setfn is not implemented')

    def update(self, dct):
        for key, value in dct.items():
            self[key] = value

    def push(self):
        self.other = Scope(self.other)

    def pop(self):
        self.other = self.other.pop()
        if self.other is None:
            raise RuntimeError('SymbolTable has been asked to get back beyond the highest level scope')

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

    @property
    def definitions(self):
        return self.attrs.get('define', [])

    @property
    def func_calls(self):
        return self.attrs.get('call', [])

    def define(self, var):
        dfn = self.definitions
        if not isinstance(var, list):
            dfn.append(var)
        else:
            dfn.extend(var)
        self._attr['define'] = dfn

    def func_call(self, fn):
        cl = self.func_calls
        if not isinstance(fn, list):
            cl.append(fn)
        else:
            cl.extend(fn)
        self._attr['call'] = cl

    def generate(self, table):
        return Code()

def get_c_type(exp):
    return {
        'integer': 'int',
        'float'  : 'float',
        'boolean': 'bool',
        'void'   : 'void',
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
    elif t1 is None or t2 is None:
        return None
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

    def __str__(self):
        return self._txt

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

    def generate(self, table):
        c = self._cont
        # TODO: In here we need to figure out if any of the elements from the expression is
        #       a function call. If it is, we need to figure out the signature, check if the
        #       function has been generated and, if not, try to generate it.
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
        # TODO: Fix this
        # self._attr = Attribs({'define': [self._var]})
        # self._attr.update(self._iter.attrs)
        # self._attr.update(self._blk.attrs)
        self._attr = Attribs()

    def generate(self, table):
        c = Code()
        counter = varGen.nextCounter()
        iterab  = varGen.nextIterable()
        cnt_expr = Arbitrary('{}[{cn}]'.format(iterab, cn=counter), tp='integer')
        self._blk.prepend_token(Assign(None, None, [[self._var, cnt_expr]]))
        new_block_toks = [
            Arbitrary('int {it}[] = {{ {lst} }};'.format(it=iterab, lst=str(self._iter))),
            Arbitrary('for(int {cn} = 0; {cn} < (sizeof({it})/sizeof(*{it})); {cn}++)'.format(cn=counter, it=iterab)),
            self._blk
        ]

        return Block(None, None, [new_block_toks]).generate(table)

class IfSentence(LangComp):
    def __init__(self, str_, loc, toks):
        t = toks[0]
        self._cond = t[0]
        self._then = t[1]
        self._attr = Attribs()
        # TODO: Fix this
        # self._attr.update(t[0].attrs)
        # self._attr.update(t[1].attrs)
        if len(t) == 3:
            self._else = t[2]
            # TODO: Fix this
            # self._attr.update(t[2].attrs)
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
        # TODO: Fix this
        # self._attr.update(self._body.attrs)

    @nested_scope
    def generate(self, table):
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
        self.define(self._var)

    @property
    def name(self):
        return self._var.name

    @nested_scope
    def generate(self, table):
        exp_code = self._exp.generate(table)
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

    @nested_scope
    def generate(self, table):
        self._exp.generate(table)
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
        self.func_call(self)

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

    def prepend_token(self, snt):
        self._toks = [snt] + self._toks

    @nested_scope
    def generate(self, table, enclosed=True):
        exports = {}
        collect = Code()

        defer_defs = []

        for t in self._toks:
            dfn = t.definitions
            for d in dfn:
                if d.name not in table:
                    table[d.name] = d
                    defer_defs.append(d)

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

    @nested_scope
    def generate(self, table):
        # TODO: Fix this
        # table.update(dict((a.name, a) for a in self._args))
        block_code = self._body.generate(table)

        if self.name in entry_points:
            name = self.name
            if self._type is not None:
                abort('{} no puede devolver valores'.format(name))
            if len(self._args) > 0:
                abort('{} no admite argumentos'.format(name))
            self._type = 'void'
        else:
            name = self._fname.generate(table)

        code = Code()
        code.append('{} {} ({})'.format(get_c_type(self.type), name, ', '.join('{} {}'.format(get_c_type(a.type), a.generate(table)) for a in self._args)))
        code.append(block_code)

        print(code)

class Program(LangComp):
    def __init__(self, str_, loc, toks):
        self._toks = toks

    def generate(self):
        table = SymbolTable()
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

kwset = {
    'usar', 'funcion', 'para', 'mientras',
    'devolver', 'si', 'entonces', 'sino',
    'hacer', 'inicio', 'fin', 'Cierto', 'Falso'
}

keyword     = MatchFirst(map(Keyword, kwset))

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

funcion setup
inicio
  j <- add1(0)
fin
"""

if __name__ == '__main__':
    prog = programa.parseString(ejemplo, parseAll=True)
    prog[0].generate()
