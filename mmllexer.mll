{

  open Lexing
  open Mmlparser

  exception Lexing_error of string


  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ (* À *) 
      ("int", TYPE_INT);
      ("bool", TYPE_BOOL);
      ("unit", TYPE_UNIT);
      ("type", TYPE);
      ("mutable", MUTABLE);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("fun", FUN);
      ("let", LET);
      ("rec", REC);
      ("in", IN);
      ("not", NOT);
      ("mod", MODULUS);
      ("and", AND);
      ("or", OR);
      ("true", TRUE);
      ("false", FALSE);
      ("array", ARRAY_KEYWORD);
      ] ;
    fun s ->
      try Hashtbl.find h s
      with Not_found -> IDENT(s)
      
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']
          { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
          { token lexbuf }
  | "(*"
          { comment lexbuf; token lexbuf }
  | number as n
          { CST(int_of_string n) }
  | "("
          { LPAR }
  | ")"
          { RPAR }
  | "."
          { DOT }
  | "{"
          { LACC }
  | "}"
          { RACC }
  | ";"
          { SEMI }
  | "->"
          { RARROW }
  | "<-"
          { LARROW }
  | ":"
          { COLON }
  | "+"
          { PLUS }
  | "-"
          { MINUS }
  | "*"
          { STAR }
  | "/"
          { SLASH }
  | "=="
          { EQUAL }
  | "!="
          { NOT_EQUAL }
  | "<>"
          { STRUCT_NOT_EQUAL }
  | "<="
          { LESSER_EGUAL_THAN }
  | ">="
          { GREATER_EGUAL_THAN }
  | "<"
          { LESSER_THAN }
  | ">"
          { GREATER_THAN }
  | "="
          { EQ_SIGN }
  | "[|"
          { ARRAY_BEGIN }
  | "|]"
          { ARRAY_END }
  | ident as id
          { keyword_or_ident id }
  | _
          { raise (Lexing_error ("unknown character : " ^ (lexeme lexbuf))) }
  | eof
          { EOF }

and comment = parse
  | "*)"      { () }
  | "(*"      { comment lexbuf; comment lexbuf }
  | _      { comment lexbuf }
  | eof      { raise (Lexing_error "unterminated comment") }
