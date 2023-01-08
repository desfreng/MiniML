open Format
open Mml

let rec print_fields ppf = function
  | [] -> fprintf ppf ""
  | (x, t, true) :: l -> fprintf ppf "mutable %s: %s;@, %a" x (string_of_type t) print_fields l
  | (x, t, false) :: l -> fprintf ppf "%s: %s;@, %a" x (string_of_type t) print_fields l

let rec print_types ppf = function
  | [] -> fprintf ppf "@."
  | (t, s) :: l -> fprintf ppf "type %s = { @[%a}@]@.%a" t print_fields s print_types l

let bop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"
  | Struct_Eq -> "="
  | Struct_Neq -> "<>"

let rec print_expr ppf = function
  | Int n -> fprintf ppf "%i" n
  | Bool b -> fprintf ppf "%b" b
  | Array l -> fprintf ppf "[| %a |]" print_array l
  | Unit -> fprintf ppf "()"
  | Uop (Neg, e) -> fprintf ppf "-%a" print_expr e
  | Uop (Not, e) -> fprintf ppf "not %a" print_expr e
  | Bop (op, e1, e2) -> fprintf ppf "@[%a@ %s %a@]" print_expr e1 (bop_to_string op) print_expr e2
  | Var x -> fprintf ppf "%s" x
  | Let (x, e1, e2) -> fprintf ppf "@[let %s =@[@ %a@]@ @[in@ %a@]@]" x print_expr e1 print_expr e2
  | If (c, e1, e2) ->
      fprintf ppf "@[<hv>if @[%a@]@ then @[%a@]@ else @[%a@]@]" print_expr c print_expr e1
        print_expr e2
  | Fun (x, e) -> fprintf ppf "fun %s -> @[@ %a@]" x print_expr e
  | UnitFun e -> fprintf ppf "fun () -> @[@ %a@]" print_expr e
  | App (e1, e2) -> fprintf ppf "%a %a" print_expr e1 print_expr e2
  | Fix (x, e) -> fprintf ppf "fix %s = @[%a@]" x print_expr e (* To Remove *)
  | Strct l -> fprintf ppf "{ @[%a}@]" print_defs l
  | GetF (e, x) -> fprintf ppf "%a.%s" print_expr e x
  | SetF (e1, x, e2) -> fprintf ppf "%a.%s <- %a" print_expr e1 x print_expr e2
  | Seq (e1, e2) -> fprintf ppf "%a;@ %a" print_expr e1 print_expr e2
  | GetArray (e, i) -> fprintf ppf "%a.(%a)" print_expr e print_expr i
  | SetArray (e, i, v) -> fprintf ppf "%a.(%a) <- @[@ %a@]" print_expr e print_expr i print_expr v

and print_defs ppf = function
  | [] -> fprintf ppf ""
  | (x, e) :: l -> fprintf ppf "%s = (%a);@ %a" x print_expr e print_defs l

and print_array ppf = function
  | [] -> fprintf ppf ""
  | [ e ] -> fprintf ppf "%a" print_expr e
  | e :: l -> fprintf ppf "%a; %a" print_expr e print_array l

let print_prog ppf prog = fprintf ppf "%a@.%a@." print_types prog.types print_expr prog.code
let hashtbl_to_list h = Hashtbl.fold (fun key value acc -> (key, value) :: acc) h []

let rec print_value ppf (v, mem) =
  match v with
  | VInt i -> fprintf ppf "%i" i
  | VUnit -> fprintf ppf "()"
  | VBool b -> if b then fprintf ppf "true" else fprintf ppf "false"
  | VPtr p -> (
      let heap_val = Hashtbl.find_opt mem p in
      match heap_val with
      | Some (VClos _) -> fprintf ppf "<fun>"
      | Some (VStrct arg_list) ->
          let rec _print_field ppf = function
            | [] -> ()
            | (arg_name, VPtr ptr) :: l when ptr = p ->
                fprintf ppf "%s = <cycle>;@ %a" arg_name _print_field l
            | (arg_name, value) :: l ->
                fprintf ppf "%s = %a;@ %a" arg_name print_value (value, mem) _print_field l
          in
          fprintf ppf "{@[%a}@]" _print_field (hashtbl_to_list arg_list)
      | Some (VArray value_array) ->
          let rec _print_array ppf = function
            | [] -> ()
            | [ v ] -> fprintf ppf "%a" print_value (v, mem)
            | v :: l -> fprintf ppf "%a; %a" print_value (v, mem) _print_array l
          in
          fprintf ppf "[| %a |] " _print_array (Array.to_list value_array)
      | None -> failwith "Non existing poiter")

let print_end_value ppf (v, t, mem) = fprintf ppf "%s = %a@." (string_of_type t) print_value (v, mem)

let expr_to_string e =
  fprintf str_formatter "%a" print_expr e;
  flush_str_formatter ()
