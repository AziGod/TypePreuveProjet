%nonassoc DELETE_NODES
%nonassoc DELETE_RELS

%token <string> IDENTIFIER
%token <Lang.attrib_tp> TP
%token <bool> BCONSTANT
%token <int> INTCONSTANT
%token <string> STRINGCONSTANT
%token BLAND BLOR
%token EQ GE GT LE LT NE
%token ADD SUB MUL DIV MOD
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN 
%token DOT COMMA COLON
%token CREATE DELETE MATCH RETURN SET WHERE
%token ARROW
%token EOF

%start<Lang.prog> main

%left BLOR
%left BLAND
%left EQ GE GT LE LT NE
%left ADD SUB
%left MUL DIV MOD

%{ open Lang %}

%%

main: prog EOF { $1 }

prog: td = list(tpDecl);  q = query 
     { let (nts, rts) = List.partition_map Fun.id td in Prog (DBG(nts, rts), q) }

tpDecl:
| n = nodeTpDecl { Either.Left n }
| r = relTpDecl { Either.Right r }

query: cls = list(clause) { Query cls }

clause: 
| CREATE; pts = separated_list(COMMA, pattern) { Create pts }
| MATCH; pts = separated_list(COMMA, pattern) { Match pts }
| DELETE; dp = delete_pattern { Delete dp }
| RETURN; vns = separated_list(COMMA, IDENTIFIER) { Return vns }
| WHERE; e = expr { Where e }
| SET; updates = separated_list(COMMA, set_expr) { Set updates }

pattern: 
| np = npattern { SimpPattern np }
| np = npattern; COLON; t = IDENTIFIER; p = pattern { CompPattern(np, t, p) }

npattern: 
| LPAREN; v = IDENTIFIER; COLON; t = IDENTIFIER; RPAREN  { DeclPattern(v, t) }
| LPAREN; v = IDENTIFIER; RPAREN { VarRefPattern(v) }

delete_pattern:
| LBRACKET; vns = separated_list(COMMA, IDENTIFIER); RBRACKET %prec DELETE_NODES { DeleteNodes vns }
| LBRACKET; rls = separated_list(COMMA, rel_expr); RBRACKET %prec DELETE_RELS { DeleteRels rls }

rel_expr:
| v1 = IDENTIFIER; COLON; t = IDENTIFIER; v2 = IDENTIFIER { (v1, t, v2) }

set_expr:
| v = IDENTIFIER; DOT; f = IDENTIFIER; EQ; e = expr { (v, f, e) }

primary_expr:
| vn = IDENTIFIER; DOT; fn = IDENTIFIER { AttribAcc(vn, fn) }
| c = BCONSTANT { Const(BoolV(c)) }
| c = INTCONSTANT { Const(IntV(c)) }
| c = STRINGCONSTANT { Const(StringV(c)) }
| LPAREN e = expr RPAREN { e }

expr:
| a = primary_expr { a }
| e1 = expr; op = binop; e2 = expr { BinOp(op, e1, e2) }

binop:
| ADD { BArith(BAadd) }
| SUB { BArith(BAsub) }
| MUL { BArith(BAmul) }
| DIV { BArith(BAdiv) }
| MOD { BArith(BAmod) }
| EQ { BCompar(BCeq) }
| GE { BCompar(BCge) }
| GT { BCompar(BCgt) }
| LE { BCompar(BCle) }
| LT { BCompar(BClt) }
| NE { BCompar(BCne) }
| BLAND { BLogic(BLand) }
| BLOR { BLogic(BLor) }

nodeTpDecl: LPAREN; COLON; i = IDENTIFIER; a = attrib_declList; RPAREN  { DBN (i, a) }

attrib_decl: i = IDENTIFIER; t = TP { (i, t) }
attrib_declList: 
| LBRACE; ads = separated_list(COMMA, attrib_decl); RBRACE { ads }

nodeTpRef: LPAREN; COLON; si = IDENTIFIER; RPAREN { si }
relTpDecl: si = nodeTpRef;
           SUB; LBRACKET; COLON; rlab = IDENTIFIER; RBRACKET; ARROW; 
           ti = nodeTpRef
           { Graphstruct.DBR (si, rlab, ti) }

%%
