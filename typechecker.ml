open Mml

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make (String)

type tenv = schema SymTbl.t

(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string

let warning s = Printf.printf "\nWarning : %s\n" s
let undeclared_var = Printf.sprintf "variable '%s' is not declared in current environement"

let no_struct_find t =
  Printf.sprintf "structure not find with keys : %s"
    (List.fold_left (fun acc (elm, _) -> if acc = "" then elm else acc ^ ", " ^ elm) "" t)

let not_a_struct t = Printf.sprintf "type '%s' is not a structure" (string_of_type t)
let not_a_field = Printf.sprintf "field '%s' is not in structure '%s'"
let not_mutable = Printf.sprintf "field '%s' in structure '%s' is not mutable"
let field_multiple_time_in_struct = Printf.sprintf "field '%s' is in structure '%s' multiples times"

let unification_error t1 t2 =
  Printf.sprintf "can't unifiate '%s' and '%s'" (string_of_type t1) (string_of_type t2)

(* Vérification des types d'un programme *)
let type_prog prog =
  let check_types =
    List.iter (fun (type_name, type_list) ->
        List.iter
          (fun (member_name, _, _) ->
            let occurence_nb =
              List.fold_left
                (fun acc (elm, _, _) -> if elm = member_name then acc + 1 else acc)
                0 type_list
            in
            if occurence_nb > 1
            then raise (Type_error (field_multiple_time_in_struct member_name type_name)))
          type_list)
  in
  check_types prog.types;
  let subst = Hashtbl.create 32 in
  let new_var_name =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      Printf.sprintf "tvar_%i" !cpt
  in
  let new_var () = TVar (new_var_name ()) in
  let find_struct_by_name strct_name =
    match List.find_opt (fun (name, _) -> name = strct_name) prog.types with
    | Some (_, strct_def) -> strct_def
    | None -> raise (Type_error (not_a_struct (TStrct strct_name)))
  in
  let find_closest_struct member_name =
    match
      List.find_opt
        (fun (_, strct_def) -> List.exists (fun (def_elm, _, _) -> def_elm = member_name) strct_def)
        prog.types
    with
    | Some (struct_name, _) -> struct_name
    | None -> raise (Type_error (no_struct_find [ (member_name, 0) ]))
  in
  let find_struct strct_l =
    let match_type strct_list strct_def =
      List.fold_left
        (fun acc (strct_elm, _) ->
          List.exists (fun (def_elm, _, _) -> def_elm = strct_elm) strct_def && acc)
        true strct_list
    in
    let strct_size = List.length strct_l in
    List.find_opt
      (fun (_, strct_def) -> List.length strct_def == strct_size && match_type strct_l strct_def)
      prog.types
  in
  let rec unfold = function
    | TVar a -> if Hashtbl.mem subst a then unfold (Hashtbl.find subst a) else TVar a
    | other -> other
  in
  let rec unfold_full t =
    match unfold t with
    | TFun (t1, t2) -> TFun (unfold_full t1, unfold_full t2)
    | TArray t -> TArray (unfold_full t)
    | t -> t
  in
  let rec occur a t =
    match unfold t with
    | TInt | TBool | TUnit | TStrct _ -> false
    | TVar b -> a = b
    | TFun (t1, t2) -> occur a t1 || occur a t2
    | TArray t -> occur a t
  in
  let rec unify t1 t2 =
    match (unfold t1, unfold t2) with
    | TInt, TInt -> ()
    | TBool, TBool -> ()
    | TUnit, TUnit -> ()
    | TStrct a, TStrct b when a = b -> ()
    | TArray a, TArray b -> unify a b
    | TFun (t1, t1'), TFun (t2, t2') ->
        unify t1 t2;
        unify t1' t2'
    | TVar a, TVar b when a = b -> ()
    | TVar a, t | t, TVar a ->
        if occur a t
        then raise (Type_error (unification_error (TVar a) (unfold_full t)))
        else Hashtbl.add subst a t
    | _, _ -> raise (Type_error (unification_error t1 t2))
  in
  let instantiate s =
    let renaming = VSet.fold (fun v r -> SymTbl.add v (new_var ()) r) s.vars SymTbl.empty in
    let rec rename t =
      match unfold t with
      | TVar t_var -> (
          match SymTbl.find_opt t_var renaming with Some t_find -> t_find | None -> TVar t_var)
      | TFun (t1, t2) -> TFun (rename t1, rename t2)
      | TArray t -> TArray (rename t)
      | t -> t
    in
    rename s.typ
  in
  let generalize t tenv =
    let rec fvars t =
      match unfold t with
      | TFun (t1, t2) -> VSet.union (fvars t1) (fvars t2)
      | TVar x -> VSet.singleton x
      | TArray x -> fvars x
      | _ -> VSet.empty
    in
    let schema_fvars s = VSet.diff (fvars s.typ) s.vars in
    let fvt = fvars t in
    let fvenv = SymTbl.fold (fun _ s vs -> VSet.union (schema_fvars s) vs) tenv VSet.empty in
    { vars = VSet.diff fvt fvenv; typ = t }
  in
  (* Calcule le type de l'expression [e] *)
  let rec w e tenv =
    match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Unit -> TUnit
    | Uop (Not, e) ->
        let t1 = w e tenv in
        unify t1 TBool;
        TBool
    | Uop (Neg, e) ->
        let t1 = w e tenv in
        unify t1 TInt;
        TInt
    | Bop ((Add | Sub | Mul | Div | Mod), e1, e2) ->
        let t1 = w e1 tenv in
        let t2 = w e2 tenv in
        unify t1 TInt;
        unify t2 TInt;
        TInt
    | Bop ((Eq | Neq | Struct_Eq | Struct_Neq), e1, e2) ->
        let t1 = w e1 tenv in
        let t2 = w e2 tenv in
        unify t1 t2;
        TBool
    | Bop ((Lt | Le | Gt | Ge), e1, e2) ->
        let t1 = w e1 tenv in
        let t2 = w e2 tenv in
        unify t1 TInt;
        unify t2 TInt;
        TBool
    | Bop ((And | Or), e1, e2) ->
        let t1 = w e1 tenv in
        let t2 = w e2 tenv in
        unify t1 TBool;
        unify t2 TBool;
        TBool
    | Var x -> (
        match SymTbl.find_opt x tenv with
        | Some t -> instantiate t
        | None -> raise (Type_error (undeclared_var x)))
    | Let (var_name, var_expr, expression) ->
        let type_var = w var_expr tenv in
        let schema_var = generalize type_var tenv in
        w expression (SymTbl.add var_name schema_var tenv)
    | If (condition, true_branch, false_branch) ->
        let condition_type = w condition tenv in
        unify condition_type TBool;
        let true_branch_type = w true_branch tenv in
        let false_branch_type = w false_branch tenv in
        unify true_branch_type false_branch_type;
        true_branch_type
    | Fun (var_name, expression) ->
        let var_type = new_var () in
        let expression_env = SymTbl.add var_name { vars = VSet.empty; typ = var_type } tenv in
        let expression_type = w expression expression_env in
        TFun (var_type, expression_type)
    | UnitFun expression ->
        let expression_type = w expression tenv in
        TFun (TUnit, expression_type)
    | App (e1, e2) ->
        let t1 = w e1 tenv in
        let t2 = w e2 tenv in
        let function_out_type = new_var () in
        unify (TFun (t2, function_out_type)) t1;
        function_out_type
    | Fix (rec_var, rec_expr) -> (
        match rec_expr with
        | Fun _ | Strct _ ->
            let rec_type = new_var () in
            let reccursive_env = SymTbl.add rec_var { vars = VSet.empty; typ = rec_type } tenv in
            let rec_deducted_type = w rec_expr reccursive_env in
            unify rec_type rec_deducted_type;
            rec_type
        | _ -> raise (Type_error "recurssion only possible for Function and Structures"))
    | Strct strct_l -> (
        match find_struct strct_l with
        | Some (strct_name, strct_struct) ->
            List.iter
              (fun (elm_name, elm_type, _) ->
                let elm_expr =
                  match List.assoc_opt elm_name strct_l with
                  | Some e -> e
                  | None -> raise (Type_error (not_a_struct (TStrct strct_name)))
                in
                let elm_t = w elm_expr tenv in
                unify elm_t elm_type)
              strct_struct;
            TStrct strct_name
        | None -> raise (Type_error (no_struct_find strct_l)))
    | GetF (strct_expr, member_name) -> (
        let extract_struct_type struct_name =
          let strct_def = find_struct_by_name struct_name in
          match List.find_opt (fun (elm_name, _, _) -> elm_name = member_name) strct_def with
          | Some (_, elm_type, _) -> elm_type
          | None -> raise (Type_error (not_a_field member_name struct_name))
        in
        match w strct_expr tenv with
        | TStrct struct_name -> extract_struct_type struct_name
        | TVar a ->
            let struct_name = find_closest_struct member_name in
            unify (TVar a) (TStrct struct_name);
            extract_struct_type struct_name
        | t -> raise (Type_error (not_a_struct t)))
    | SetF (strct_expr, member_name, member_expr) -> (
        let check_member_mutability struct_name =
          let expr_type = w member_expr tenv in
          let strct_def = find_struct_by_name struct_name in
          match List.find_opt (fun (elm_name, _, _) -> elm_name = member_name) strct_def with
          | Some (_, member_type, mut) when mut ->
              unify expr_type member_type;
              TUnit
          | Some _ -> raise (Type_error (not_a_field member_name struct_name))
          | None -> raise (Type_error (not_a_field member_name struct_name))
        in
        match w strct_expr tenv with
        | TStrct struct_name -> check_member_mutability struct_name
        | TVar _ as t ->
            let struct_name = find_closest_struct member_name in
            unify t (TStrct struct_name);
            check_member_mutability struct_name
        | t -> raise (Type_error (not_a_struct t)))
    | Seq (e1, e2) ->
        let type_e1 = w e1 tenv in
        (match type_e1 with
        | TUnit | TVar _ -> unify type_e1 TUnit
        | _ ->
            warning
              (Printf.sprintf "Ignoring non unit return type %s ('%s')\n" (string_of_type type_e1)
                 (Mmlpp.expr_to_string e1)));
        w e2 tenv
    | Array expr_list -> (
        match expr_list with
        | [] -> TArray (new_var ())
        | e :: l ->
            let array_type = w e tenv in
            List.iter
              (fun e ->
                let elm_type = w e tenv in
                unify elm_type array_type)
              l;
            TArray array_type)
    | GetArray (array, index) ->
        let index_type = w index tenv in
        unify index_type TInt;
        let array_elm_type = new_var () in
        let array_type = w array tenv in
        unify array_type (TArray array_elm_type);
        array_elm_type
    | SetArray (array, index, array_elm) ->
        let index_type = w index tenv in
        unify index_type TInt;
        let array_elm_type = w array_elm tenv in
        let array_type = w array tenv in
        unify array_type (TArray array_elm_type);
        TUnit
  in

  w prog.code SymTbl.empty |> unfold_full
