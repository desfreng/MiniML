(* Interprète Mini-ML *)

open Mml

(* Interprétation d'un programme complet *)
let eval_prog p =
  (* Initialisation de la mémoire globale *)
  let (mem : (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* Création de nouvelles adresses *)
  let new_ptr =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt
  in

  (* Interprétation d'une expression, en fonction d'un environnement
     et de la mémoire globale *)
  let rec eval e env =
    match e with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Int n -> VInt n
    | Var x -> (
        match Env.find_opt x env with Some x -> x | None -> failwith "Undeclared variable")
    | Uop (Not, b) -> VBool (not (evalb b env))
    | Uop (Neg, e) -> VInt (-evali e env)
    | Bop (Add, e_left, e_right) -> VInt (eval_bop e_left e_right ( + ) env)
    | Bop (Sub, e_left, e_right) -> VInt (eval_bop e_left e_right ( - ) env)
    | Bop (Mul, e_left, e_right) -> VInt (eval_bop e_left e_right ( * ) env)
    | Bop (Div, e_left, e_right) -> VInt (eval_bop e_left e_right ( / ) env)
    | Bop (Mod, e_left, e_right) -> VInt (eval_bop e_left e_right ( mod ) env)
    | Bop (Eq, e_left, e_right) -> VBool (equals e_left e_right env)
    | Bop (Neq, e_left, e_right) -> VBool (not (equals e_left e_right env))
    | Bop (Struct_Eq, e_left, e_right) -> VBool (struct_equals e_left e_right env)
    | Bop (Struct_Neq, e_left, e_right) -> VBool (not (struct_equals e_left e_right env))
    | Bop (Lt, e_left, e_right) ->
        let v_right = eval e_right env in
        let v_left = eval e_left env in
        VBool (v_left < v_right)
    | Bop (Le, e_left, e_right) ->
        let v_right = eval e_right env in
        let v_left = eval e_left env in
        VBool (v_left <= v_right)
    | Bop (Gt, e_left, e_right) ->
        let v_right = eval e_right env in
        let v_left = eval e_left env in
        VBool (v_left > v_right)
    | Bop (Ge, e_left, e_right) ->
        let v_right = eval e_right env in
        let v_left = eval e_left env in
        VBool (v_left >= v_right)
    | Bop (And, e_left, e_right) ->
        if evalb e_left env then VBool (evalb e_right env) else VBool false
    | Bop (Or, e_left, e_right) ->
        if evalb e_left env then VBool true else VBool (evalb e_right env)
    | Let (x, e1, e2) ->
        let v = eval e1 env in
        eval e2 (Env.add x v env)
    | If (e1, e2, e3) -> if evalb e1 env then eval e2 env else eval e3 env
    | Fun (x, e) -> build_fun_in (new_ptr ()) x e env
    | UnitFun e ->
        let func_address = new_ptr () in
        Hashtbl.add mem func_address (VClos ("", e, env));
        VPtr func_address
    | App (e1, e2) -> (
        let arg = eval e2 env in
        match eval e1 env with
        | VPtr f -> (
            match Hashtbl.find_opt mem f with
            | Some (VClos ("", e, f_env)) -> eval e f_env
            | Some (VClos (x, e, f_env)) -> eval e (Env.add x arg f_env)
            | Some _ -> failwith "App only possible with VClos"
            | None -> failwith "Pointer does not exists in memory")
        | _ -> failwith "App only possible with VPtr")
    | Fix (obj_name, obj_expr) -> (
        match obj_expr with
        | Fun (x, e) ->
            let func_address = new_ptr () in
            let func_env = Env.add obj_name (VPtr func_address) env in
            build_fun_in func_address x e func_env
        | Strct member_list ->
            let struct_address = new_ptr () in
            let struct_env = Env.add obj_name (VPtr struct_address) env in
            build_struct_in struct_address member_list struct_env
        | _ -> failwith "Fix only possible with Fun or Strct")
    | Strct member_list -> build_struct_in (new_ptr ()) member_list env
    | GetF (strct, member) -> (
        match eval strct env with
        | VPtr strct_address -> (
            match Hashtbl.find_opt mem strct_address with
            | Some (VStrct strct_hashmap) -> (
                match Hashtbl.find_opt strct_hashmap member with
                | Some v -> v
                | None -> failwith "Wrong member in structure")
            | Some _ -> failwith "GetF only possible with a VStrct"
            | None -> failwith "Pointer does not exists in memory")
        | _ -> failwith "GetF only possible with a VPtr")
    | SetF (strct, member, expression) -> (
        let value = eval expression env in
        match eval strct env with
        | VPtr strct_address -> (
            match Hashtbl.find_opt mem strct_address with
            | Some (VStrct strct_hashmap) ->
                Hashtbl.replace strct_hashmap member value;
                VUnit
            | Some _ -> failwith "SetF only possible with a VStrct"
            | None -> failwith "Pointer does not exists in memory")
        | _ -> failwith "SetF only possible with a VPtr")
    | Seq (e1, e2) ->
        (match eval e1 env with VUnit -> () | _ -> Printf.printf "Ignoring non unit return type");
        eval e2 env
    | Array expr_list ->
        let array_address = new_ptr () in
        let value_list = List.map (fun e -> eval e env) expr_list in
        Hashtbl.add mem array_address (VArray (Array.of_list value_list));
        VPtr array_address
    | GetArray (array_expr, index) -> (
        match (eval array_expr env, eval index env) with
        | VPtr array_address, VInt i -> (
            match Hashtbl.find_opt mem array_address with
            | Some (VArray a) ->
                if i >= 0 && i < Array.length a then a.(i) else failwith "Index Out of bound"
            | Some _ -> failwith "GetArray only possible with a VArray"
            | None -> failwith "Pointer does not exists in memory")
        | _ -> failwith "GetArray only possible with a VPtr and a VInt")
    | SetArray (array_expr, index, new_value) -> (
        let n_value = eval new_value env in
        match (eval array_expr env, eval index env) with
        | VPtr array_address, VInt i -> (
            match Hashtbl.find_opt mem array_address with
            | Some (VArray a) ->
                if i >= 0 && i < Array.length a
                then (
                  a.(i) <- n_value;
                  VUnit)
                else failwith "Index Out of bound"
            | Some _ -> failwith "SetArray only possible with a VArray"
            | None -> failwith "Pointer does not exists in memory")
        | _ -> failwith "SetArray only possible with a VPtr and a VInt")
  (* Évaluation d'une expression dont la valeur est supposée entière *)
  and evali e env = match eval e env with VInt n -> n | _ -> failwith "Error, int expected"
  (* Évaluation d'une expression dont la valeur est supposée booléenne *)
  and evalb e env = match eval e env with VBool b -> b | _ -> failwith "Error, bool expected"
  and eval_bop e_left e_right op env =
    let v_right = evali e_right env in
    let v_left = evali e_left env in
    op v_left v_right
  and equals e_left e_right env =
    let v_right = eval e_right env in
    let v_left = eval e_left env in
    match (v_left, v_right) with
    | VInt a, VInt b -> a = b
    | VBool a, VBool b -> a = b
    | VUnit, VUnit -> true
    | VPtr a, VPtr b -> a = b
    | _ -> false
  and struct_equals e_left e_right env =
    let rec value_equals v_left v_right =
      match (v_left, v_right) with
      | VInt a, VInt b -> a = b
      | VBool a, VBool b -> a = b
      | VUnit, VUnit -> true
      | VPtr l, VPtr r -> (
          if l = r
          then true
          else
            match (Hashtbl.find_opt mem l, Hashtbl.find_opt mem r) with
            | Some (VArray a), Some (VArray b) ->
                let length = Array.length a in
                if length == Array.length b
                then
                  let rec loop i =
                    if value_equals a.(i) b.(i) then if i = 0 then true else loop (i - 1) else false
                  in
                  loop (length - 1)
                else false
            | Some (VClos _), Some (VClos _) ->
                failwith "Unable to compare the structure of functions"
            | Some (VStrct a), Some (VStrct b) ->
                let key_set = Hashtbl.to_seq_keys a in
                if Seq.length key_set == Seq.length (Hashtbl.to_seq_keys b)
                then
                  let rec loop seq =
                    match seq with
                    | Seq.Cons (key, next_keys) -> (
                        match (Hashtbl.find_opt a key, Hashtbl.find_opt b key) with
                        | Some val_a, Some val_b -> value_equals val_a val_b && loop (next_keys ())
                        | None, _ | _, None -> false)
                    | Nil -> true
                  in
                  loop (key_set ())
                else false
            | None, _ | _, None -> failwith "Pointer does not exists in memory"
            | _, _ -> false)
      | _, _ -> false
    in

    let v_right = eval e_right env in
    let v_left = eval e_left env in
    value_equals v_left v_right
  and build_struct_in struct_address member_list env =
    let strct_hashmap = Hashtbl.create 16 in
    List.iter
      (fun (key, value) -> Hashtbl.add strct_hashmap key value)
      (List.map (fun (key, expression) -> (key, eval expression env)) member_list);
    Hashtbl.add mem struct_address (VStrct strct_hashmap);
    VPtr struct_address
  and build_fun_in fun_address var_name fun_expression env =
    Hashtbl.add mem fun_address (VClos (var_name, fun_expression, env));
    VPtr fun_address
  in

  (eval p.code Env.empty, mem)
