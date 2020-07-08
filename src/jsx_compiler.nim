import strutils

const Sym = {'<', '>', '=', '{', '}', ')', '.', '('}

type 
  Compiler = ref object of RootObj
   tokens:seq[TokenLine]
   current:int
   factory:string
   in_tag:bool
   output:string
  Token = ref object of RootObj
    kind:string
    data:string
  TokenLine = ref object of Token
    line:int

proc parseTag(self:Compiler): void 
proc parseBody(self:Compiler, jsexpr:bool = false): void

proc isSpace(c:char): bool = 
  return c == ' ' or c == '\t' or c == '\n'

proc isNumeric(c:char): bool = 
  let c = ord(c)
  return c >= 48 and c <= 57

proc isAlpha(c:char): bool = 
  let c = ord(c)
  return c >= 65 and c <= 90 or c >= 97 and c <= 122

proc isSpecial(c:char): bool = 
  return c == '-' or c == '_' or c == '$'

proc isIdChar(c:char): bool = 
  return isAlpha(c) or isNumeric(c) or isSpecial(c)

proc isString(c:char): bool = 
  return c == '\"' or c == '\'' or c == '`'

proc isSym(c:char): bool = 
  c in Sym

proc printLine(self:Compiler, l:int, caret:int, message:string): void = 
  var chars = ""
  var start = $l & " | "
  var line = ""
  var i:int
  var nl:int
  var c = 0
  while i < self.tokens.len:
    if i == caret:
      c = line.len
    if self.tokens[i].line >= l:
      if i > 0:
        nl = self.tokens[i - 1].data.find("\n")
        if self.tokens[i - 1].line < l and nl != - 1:
          line &= self.tokens[i - 1].data[ (nl + 1) ..< self.tokens[i - 1].data.high]
      if self.tokens[i].line == l:
        if self.tokens[i].kind != "space":
          line &= self.tokens[i].data
        else:
          if i < self.tokens.len - 1 and self.tokens[i + 1].line == l:
            line &= self.tokens[i].data
  while i < start.len + c:
    chars &= " "
  raise newException(CatchableError, chars)

proc compile(self:Compiler,js:string): void = 
  var i = 0
  var eof = js.len
  var skip = 0
  var lineno = 1
  var dat:string
  var tok = new Token
  while (i < eof) :
    if isSym(js[i]):
      tok.kind = $js[i]
      tok.data = $js[i]
      inc i
    elif isString(js[i]):
      var del = js[i]
      inc i
      dat = ""
      while (i < eof):
        if js[i] == del and js[i-1] != '\\':
          break
        if js[i] == '\n':
          inc skip
        dat &= $js[i]
        inc i
      inc i
      tok.kind = "string"
      tok.data =  del & dat & del
      
    elif js[i] == '/':
      inc i
      var n = js[i]
      dat = ""
      if (n == '/') :
        inc i
        while (i < eof) :
          if js[i] == '\n' :
            inc skip
            break
          dat &= js[i]
          inc i
        
        tok.kind = "comment"
        tok.data = dat
      elif n == '*' :
        inc i
        while (i < eof) :
          if js[i] == '*' and js[i+1] == '/' :
            i += 2
            break
          
          if js[i] == '\n':
            inc skip
          dat &= js[i]
          inc i

        tok.kind = "comment"
        tok.data = dat
      else:
        tok.kind = "/"
        tok.data = "/"
    else :
      if isAlpha(js[i]) or isSpecial(js[i]) :
        dat = ""
        while i < eof:
          if not isIdChar(js[i]):
            break
          dat &= js[i]
          inc i

        tok.kind = "name"
        tok.data = dat
      elif isSpace(js[i]) :
        dat = ""
        while i < eof:
          if not isSpace(js[i]):
            break
          if js[i] == '\n':
            inc skip
          dat &= js[i]
          inc i
 
        tok.kind = "space"
        tok.data = dat
      else :
        dat = ""
        while i < eof:
          if isSpace(js[i]) or isSym(js[i]) or isString(js[i]):
            break
          dat &= js[i]
          inc i
        
        tok.kind = "code"
        tok.data = dat
    var token = new TokenLine
    token.kind = tok.kind
    token.data = tok.data
    token.line = lineno
    self.tokens.add token
    lineno += skip
    skip = 0
  self.parseBody()

proc getPrevious(self:Compiler): int = 
  var prev:int
  if self.tokens[self.current - 1].kind == "space":
    prev = self.current - 2
  else:
    prev = self.current - 1
  return prev

proc emit(self:Compiler, code:string): void = 
  self.output &= code

proc next_token(self:Compiler): auto = 
  if not self.in_tag or self.tokens[self.current].kind == "space":
    self.output &= self.tokens[self.current].data
  inc self.current

proc next(self:Compiler):void = 
  if self.current < self.tokens.len - 1:
    self.next_token()
  if self.current < self.tokens.len - 1 and self.tokens[self.current].kind == "space":
    self.next_token()

proc next_raw(self:Compiler): void = 
  if self.current < self.tokens.len - 1:
    self.next_token()

proc peek( self:Compiler,kind:string, num = 0): bool = 
  if self.tokens[self.current + num ].kind == kind:
    return true
  return false

proc accept(self:Compiler, kind:string): bool = 
  if self.tokens[self.current].kind == kind:
    self.next()
    return true
  return false

proc expect(self:Compiler, kind:string): void = 
  var t = self.tokens[self.current]
  if t.kind == kind:
    self.next()
  else:
    self.printLine(t.line,self.current,"Error: unexpected " & t.data)

proc is_tag(self:Compiler): bool = 
  var p:TokenLine 
  var possible = true
  if self.current == 0:
    possible = false
  else:
    p = self.tokens[self.current - 1]
    if p.kind == "space":
      if self.current >= 2:
        p = self.tokens[self.current - 2]
      else:
        possible = false
  if self.peek("<"):
    if possible and (p.kind != "name" or p.data == "return") and p.kind != ")" and p.kind != "}":
      return true
  return false

proc parseBody(self:Compiler, jsexpr:bool = false): void = 
  var c = -1
  while self.current < self.tokens.len - 1:
    if jsexpr:
      if (self.peek("{")):
        dec c
      if (self.peek("}")):
        inc c
      if (c == 0):
        return
    if self.is_tag():
      self.in_tag = true
      self.parseTag()
      self.in_tag = false
    else:
      self.next()

proc parse_jsexpr(self:Compiler): void = 
  self.expect("{")
  self.in_tag = false
  self.emit("(")
  self.parseBody(true)
  self.emit(")")
  self.in_tag = true
  self.expect("}")

proc parse_params(self:Compiler, first:bool): void = 
  var key,val:string
  if self.accept("name"):
    if not first:
      self.emit(", ")
    key = self.tokens[self.getPrevious()].data
    self.emit("'" & key & "'" & ": ")
    if self.accept("="):
      if self.accept("string"):
        val = self.tokens[self.getPrevious()].data
        self.emit(val)
      else:
        self.parse_jsexpr()
    else:
      self.emit("true")
    self.parse_params(false)

proc parseInner(self:Compiler): void = 
  var val:string
  if self.accept("string"):
    self.emit(",")
    val = self.tokens[self.getPrevious()].data
  elif self.peek("{"):
    self.emit(",")
    self.parse_jsexpr()
  elif self.peek("<"):
    if self.peek("/",1):
      return 
    else:
      self.emit(",")
      self.parseTag()
  else:
    var inner = ""
    if self.current > 0 and self.tokens[self.current - 1].kind == "space":
      dec self.current
    while (not self.peek("{") and not self.peek("string") and not self.peek("<")):
      inner &= self.tokens[self.current].data.strip
      self.next_raw()
      
    self.emit(",")
    self.emit("\"" & inner & "\"")
  self.parseInner()

proc parseClosing(self:Compiler, name:string): void = 
  if self.accept("/"):
    self.expect(">")
  else:
    self.expect(">")
    self.parseInner()
    self.expect("<")
    self.expect("/")
    self.expect("name")
    var closing = self.getPrevious()
    if self.tokens[closing].data != name:
      self.printLine(self.tokens[closing].line,closing,"Error: expected closing tag for " & name)
    else:
      self.emit(")")
    self.expect(">")

proc parseTag(self:Compiler): void = 
  var name,formatted:string
  self.expect("<")
  self.expect("name")
  name = self.tokens[self.getPrevious()].data
  formatted = name
  if name[0].toUpperAscii() != name[0]:
    formatted = "\"" & name & "\""
  self.emit(self.factory & "(" & formatted & ", ")
  self.emit("{")
  self.parse_params(true)
  self.emit("}")
  self.parseClosing(name)

proc newCompiler*(factory = "React.createElement"):Compiler = 
  new result
  result.factory = factory

when isMainModule:
  var compiler = newCompiler()
  const jsxContent = """
  let React = require('react')
  class Component extends React.Component {
    constructor (props) {
      super(props)
    }

    render () {
      return <div class="hello">
        from
        <span id="value">JSX</span>
        <MyComponent required />
      </div>
    }
  }

  module.exports = Component
  """
  compiler.compile(jsxContent)
  
  echo compiler.output