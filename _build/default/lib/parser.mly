
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

/* TODO: to be completed */
clause: 
| CREATE; pts = separated_list(COMMA, pattern) { Create pts }
| MATCH; pts = separated_list(COMMA, pattern) { Match pts }
| DELETE; dp = delete_pattern { Delete dp }

/* TODO: to be completed */
pattern: 
| np = node_pattern { SimpPattern np }

delete_pattern:
| LPAREN; nl = node_list; RPAREN { DeleteNodes nl }
| LPAREN; rl = rel_list; RPAREN { DeleteRels rl }

node_list:
| vn = STRINGCONSTANT { [vn] }
| vn = STRINGCONSTANT; COMMA; nl = node_list { vn :: nl }

rel_list:
| vn1 = STRINGCONSTANT; COMMA; l = STRINGCONSTANT; COMMA; vn2 = STRINGCONSTANT { [(vn1, l, vn2)] }
| vn1 = STRINGCONSTANT; COMMA; l = STRINGCONSTANT; COMMA; vn2 = STRINGCONSTANT; COMMA; rl = rel_list { (vn1, l, vn2):: rl }


node_pattern: 
| LPAREN; v = IDENTIFIER; COLON; t = IDENTIFIER; RPAREN { DeclPattern(v, t) }
| LPAREN; v = IDENTIFIER; RPAREN { VarRefPattern(v) }


/* Expressions */

primary_expr:
| vn = IDENTIFIER; DOT; fn = IDENTIFIER 
     { AttribAcc(vn, fn) }
| c = BCONSTANT
     { Const(BoolV(c)) }
| c = INTCONSTANT
     { Const(IntV(c)) }
| c = STRINGCONSTANT
     { Const(StringV(c)) }
| LPAREN e = expr RPAREN
     { e }

/* TODO: to be completed */
expr:
| a = primary_expr { a }


/* Types */
nodeTpDecl: LPAREN; COLON; i = IDENTIFIER; a = attrib_declList; RPAREN  { DBN (i, a) }

attrib_decl: i = IDENTIFIER; t = TP { (i, t) }
attrib_declList: 
| LBRACE; ads = separated_list(COMMA, attrib_decl); RBRACE { ads }


/* Relational type declarations of the form (:nt1) -[:rt]-> (:nt2)
 */
nodeTpRef: LPAREN; COLON; si = IDENTIFIER; RPAREN { si }
relTpDecl: si = nodeTpRef;
           SUB; LBRACKET; COLON; rlab = IDENTIFIER; RBRACKET; ARROW; 
           ti = nodeTpRef
           { Graphstruct.DBR (si, rlab, ti) }

%%
