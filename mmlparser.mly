%{
  open Mml
%}

%token TYPE_INT           "int"
%token TYPE_BOOL          "bool"
%token TYPE_UNIT          "unit"
%token TYPE               "type"
%token MUTABLE            "mutable"
%token LPAR               "("
%token RPAR               ")"
%token DOT                "."
%token LACC               "{"
%token RACC               "}"
%token EQ_SIGN            "="
%token SEMI               ";"
%token RARROW             "->"
%token LARROW             "<-"
%token COLON              ":"
%token IF                 "if"
%token THEN               "then"
%token ELSE               "else"
%token FUN                "fun"
%token LET                "let"
%token REC                "rec"
%token IN                 "in"
%token NOT                "not"
%token PLUS               "+"
%token MINUS              "-"
%token STAR               "*"
%token SLASH              "/"
%token MODULUS            "mod"
%token EQUAL              "=="
%token NOT_EQUAL          "!="
%token LESSER_THAN        "<"
%token LESSER_EGUAL_THAN  "<="
%token GREATER_THAN       ">"
%token GREATER_EGUAL_THAN ">="
%token ARRAY_BEGIN        "[|"
%token ARRAY_END          "|]"
%token ARRAY_KEYWORD      "array"
%token STRUCT_NOT_EQUAL   "<>"
%token AND                "&&"
%token OR                 "||"
%token TRUE               "true"
%token FALSE              "false"
%token U_MINUS
%token <int> CST
%token <string> IDENT
%token EOF

%nonassoc IN
%right SEMI
%nonassoc THEN
%nonassoc ELSE
%right RARROW
%left ARRAY_KEYWORD
%nonassoc EQUAL NOT_EQUAL LESSER_EGUAL_THAN LESSER_THAN GREATER_EGUAL_THAN GREATER_THAN EQ_SIGN STRUCT_NOT_EQUAL
%nonassoc LARROW
%left AND OR
%left PLUS MINUS
%left STAR SLASH MODULUS
%nonassoc TRUE FALSE LPAR LACC IDENT CST
%right NOT U_MINUS
%nonassoc ARRAY_BEGIN

%start program
%type <Mml.prog> program
%%

program: types=type_def* code=expr EOF { {types; code} } ;


%inline type_def: "type" id=IDENT "=" "{" types_list=type_def_assoc+ "}" { (id, types_list)};

%inline type_def_assoc: 
| "mutable" id=IDENT ":" t=type_expr ";" { (id, t, true) }
| id=IDENT ":" t=type_expr ";" { (id, t, false) }
;


type_expr: 
| "int" { TInt }
| "bool" { TBool }
| "unit" { TUnit } 
| id=IDENT { TStrct (id) }
| t1=type_expr "->" t2=type_expr { TFun (t1, t2) }
| t=type_expr "array" { TArray(t) }
| "(" t=type_expr ")" { t }
;


expr:
| s=s_expr { s }
| "-" e=expr { Uop(Neg, e) }   %prec U_MINUS
| "not" e=expr { Uop(Not, e) }
| e1=expr op=binop e2=expr { Bop(op, e1, e2) }
| e=expr s=s_expr { App (e, s) }
| "if" e1=expr "then" e2=expr { If(e1, e2, Unit) }
| "if" e1=expr "then" e2=expr "else" e3=expr { If(e1, e2, e3) }
| "fun" id=IDENT "->" e=expr { Fun(id, e) }
| "fun" "(" ")" "->" e=expr { UnitFun(e) }
| "let" id=IDENT args=IDENT* "=" e1=expr  "in" e2=expr { Let(id, mk_fun args e1, e2) }
| "let" id=IDENT "(" ")" "=" e1=expr "in" e2=expr { Let(id, UnitFun(e1), e2) }
| "let" "rec" id=IDENT args=IDENT* "=" e1=expr "in" e2=expr { Let (id, Fix(id, mk_fun args e1), e2) }
| "let" "rec" id=IDENT "(" ")" "=" e1=expr "in" e2=expr { Let (id, Fix(id, UnitFun(e1)), e2) }
| s=s_expr "." id=IDENT "<-" e=expr { SetF(s, id, e)}
| s=s_expr "." "(" index=s_expr ")" "<-" e=expr { SetArray(s, index, e) }
| e1=expr ";" e2=expr { Seq (e1, e2) } 
;


s_expr:
| n=CST { Int (n) }
| "true" { Bool (true) }
| "false" { Bool (false) }
| "(" ")" { Unit }
| id=IDENT { Var (id) }
| s=s_expr "." id=IDENT { GetF(s, id) }
| s=s_expr "." "(" index=s_expr ")" { GetArray(s, index) }
| "{" l=struct_affectation+ "}" { Strct (l) }
| "(" e=expr ")" { e }
| "[|" args=array_member* "|]" { Array(args) }
;

%inline struct_affectation: id=IDENT "=" e=s_expr ";" { (id, e) };

%inline array_member: e=s_expr ";" { e };


%inline binop:
| "+"   { Add }
| "-"   { Sub }
| "*"   { Mul }
| "/"   { Div }
| "mod" { Mod } 
| "=="  { Eq  }
| "!="  { Neq }
| "<"   { Lt  }
| "<="  { Le  }
| ">"   { Gt  }
| ">="  { Ge  }
| "&&"  { And }
| "||"  { Or  }
| "="   { Struct_Eq  }
| "<>"  { Struct_Neq }
;

