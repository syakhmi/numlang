open Ast
open Sast

exception Error of string

type variable_decl = {
    name : string;
    const : bool;
    var_type : Ast.v_type;
    return_type : Ast.v_type option;
    args : variable_decl list option
}

type symbol_table = {
   parent : symbol_table option;
   variables : variable_decl list
}

let rec sublist b e l =
 match l with
   [] -> failwith "sublist"
 | h :: t ->
    let tail = if e=0 then [] else sublist (b-1) (e-1) t in
    if b>0 then tail else h :: tail
;;

let convert_vecl decl =
    {name=decl.vname;const=false;var_type=decl.vtype;return_type=None;args=None}

let new_symbol_table parent l =
    {parent = parent; variables = [
        {name="log";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="ln";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="cos";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="log";const=true;var_type=Ast.Func;return_type=None;args=None}

]@l}
       

let rec find_variable (scope : symbol_table) name =
   try
       List.find (fun vdecl -> name = vdecl.name) scope.variables
   with Not_found ->
       match scope.parent with
           Some(parent) -> find_variable parent name
           | _ -> raise Not_found

let rec find_local_variable (scope : symbol_table) name =
   try
       List.find (fun vdecl -> name = vdecl.name) scope.variables
   with Not_found ->
       raise Not_found

let rec find_nonlocal_variable (scope : symbol_table) name =

match scope.parent with

Some(parent) -> find_variable parent name

| _ -> raise Not_found

type translation_environment = {
   scope : symbol_table;
}

let rec index_of i a = function
    [] -> -1
	| hd::tl -> if hd = a then i else index_of (i+1) a tl

and check_fid s l env =
    (let i = index_of 0 s l in
    if i > -1 then
        Sast.Expr(Sast.Funarg(i), Ast.Num)
    else
		let vdecl = try
        	find_variable env.scope s
		with Not_found ->
			raise (Error("undeclared identifier " ^ s))
		in
		let typ = vdecl.var_type in
		Sast.Expr(Sast.Id(s), typ))

and check_fexpr l f_expr env =
    match f_expr with
		FLitnum(s) -> Sast.Expr(Sast.Litnum(s), Ast.Num)
    	| FId(s) -> check_fid s l env
    	| FBinop(e1, op, e2) -> check_binop e1 (convert_ops op) e2 env
    	| FUnop(op, e) -> check_unop op e env
    	| FFCall(fcall) -> check_fcall fcall env
    	| _ -> raise (Error("Weird error check_fexpr!"))

and convert_ops op = 
	match op with
		Ast.FAdd -> Ast.Add
		| Ast.FSub -> Ast.Sub
		| Ast.FMult -> Ast.Mult
		| Ast.FDiv -> Ast.Div
		| Ast.FExp -> Ast.Exp
		| Ast.FMod -> Ast.Mod
	  	| Ast.FEq -> Ast.Eq
	  	| Ast.FNeq -> Ast.Neq
	  	| Ast.FLt -> Ast.Lt
	  	| Ast.FLeq -> Ast.Leq
	  	| Ast.FGt -> Ast.Gt
	  	| Ast.FGeq -> Ast.Geq
	  	| _ -> raise (Error("Invalid fop conversion!"))

and check_matrix l env =
	try
        let returnmatrix = List.map (fun a -> List.map (fun a2 -> expr a2 env) a) l in
			let rowlength = List.length (List.hd returnmatrix) in
        	let b = List.fold_left (fun valid row -> if List.length row <> rowlength then 0*valid else 1*valid) 1 returnmatrix
			and b2 = List.fold_left (fun valid row -> valid * List.fold_left (fun valid e -> 
				match e with Sast.Expr(_, vtype) -> if vtype <> Ast.Num then 0*valid else 1*valid
			) 1 row) 1 returnmatrix
			and rows = List.fold_left (fun rowcount row -> rowcount + 1) 0 returnmatrix
			and cols = List.fold_left (fun colcount col -> colcount + 1) 0 (List.hd returnmatrix)
		in if b=1 & b2=1 then
			Sast.Expr(Sast.Litmatrix(returnmatrix), Ast.Matrix(rows, cols))
		else raise (Error("Invalid matrix"))
	with Failure(hd) -> raise (Error("Invalid matrix"))

and check_list l env =
	try
		let returnlist = List.map (fun a -> expr env a) l in
		match List.hd returnlist with Sast.Expr(_, vartype) ->
			let b = List.fold_left (fun valid e -> match e with
				Sast.Expr(_, vtype) -> if vartype <> vtype then 0*valid else 1*valid) 1 returnlist
			and len = List.fold_left (fun i e -> i + 1) 0 returnlist
		in if b = 1 then Sast.Expr(Sast.Litlist(returnlist), Ast.List(vartype,len))
		else raise (Error("list of multiple types"))
	with Failure(hd) -> raise (Error("Empty list"))
   
and check_id name env =
	let vdecl = try
		find_variable env.scope name
	with Not_found ->
		raise (Error("undeclared identifier " ^ name))
	in
	let typ = vdecl.var_type in
	Sast.Expr(Sast.Id(name), typ)

and check_unop op e env =
    let e = expr env e in
    match e with
    	Sast.Expr(_, t) ->
    		if op = Ast.Uminus then
				if t = Ast.Num then
					Sast.Expr(Sast.Unop(op, e), Ast.Num)
				else
					if t = Ast.Func then
						Sast.Expr(Sast.Unop(op, e), Ast.Func)
					else
						if t = Ast.Matrix(-1,-1) then
							Sast.Expr(Sast.Unop(op, e), Ast.Matrix(-1,-1))
						else raise (Error("Illegal Unary Operator!"))
    		else
				if op = Ast.Not then
        			if t = Ast.Num then
						Sast.Expr(Sast.Unop(op, e), Ast.Num)
					else
						if t = Ast.Func then
							Sast.Expr(Sast.Unop(op, e), Ast.Func)
						else raise (Error("Illegal Unary Operator!"))
				else raise (Error("Illegal Unary Operator!"))
		| _ -> raise (Error("Illegal Unary Operator!"))

and check_binop e1 op e2 env =
    let e1 = expr env e1
    and e2 = expr env e2 in
    match e1 with
    Sast.Expr(_, t1) ->
		match e2 with
        	Sast.Expr(_, t2) ->
    			(* Case for +,-,*,%,/,^ operators *)
    			if op = Ast.Add || op = Ast.Sub|| op = Ast.Mult || op = Ast.Mod || op = Ast.Exp then
        			if (t1 = Ast.Matrix(-1,-1) && (t2 = Ast.Num || t2 = Ast.Matrix(-1,-1)))|| (t2 = Ast.Matrix(-1,-1) && t1 = Ast.Num) then
            			Sast.Expr(Sast.Binop(e1, op, e2), Ast.Matrix(-1,-1))
            		else if (t1 = Ast.Num && t2 = Ast.Num) then
                		Sast.Expr(Sast.Binop(e1, op, e2), Ast.Num)
                	else if (t1 = Ast.Func && (t2 = Ast.Func || t2 = Ast.Num)) || (t2 = Ast.Func && t1 = Ast.Num) then
                		Sast.Expr(Sast.Binop(e1, op, e2), Ast.Func)
            		else raise (Error("Illegal addition/subtraction/multiplication/modulus/division/exponential"))

				(* Case for # *)
				else if op = Ast.MatMult then
					if t1 = Ast.Matrix(-1,-1) && t2 = Ast.Matrix(-1,-1) then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Matrix(-1,-1))
					else raise (Error("Illegal Matrix Multiplication"))
					
				(* Case for <, <=, >, >= *)
				else if op = Ast.Lt || op = Ast.Leq || op = Ast.Gt || op = Ast.Geq then
					if t1 = Ast.Num && t2 = Ast.Num then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Num)
					else if (t1 = Ast.Num && t2 = Ast.Func) || (t1 = Ast.Func && t2 = Ast.Func) then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Func)
					else raise (Error("Illegal Relational Operator use"))

				(* Case for =, != *)
				else if op = Ast.Eq || op = Ast.Neq then
					if t1 = Ast.Num && t2 = Ast.Num then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Num)
					else if (t1 = Ast.Num && t2 = Ast.Func) || (t1 = Ast.Func && t2 = Ast.Func)then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Num)
					else if (t1 = Ast.String && t2 = Ast.String) then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Num)
					else raise (Error("Illegal Equality Operator use"))

				(* Case for . *)
				else if op = Ast.Concat then
					if t1 = Ast.String && t2 = Ast.String then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.String)
					else (match t1 with 
						Ast.List(vartype,len) -> (match t2 with 
							Ast.List(vartype,len2) -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.List(vartype,len+len2))
							| _ -> raise (Error("Illegal List Concatenation!")))
						| _ -> raise (Error("Illegal Concatenation!")))					
						
				else raise (Error("Illegal Binary Operation"))

		| _ ->     raise (Error("Illegal Binary Operator!"))
	| _ ->     raise (Error("Illegal Binary Operator!"))
   
and check_scall name args env =
	let vdecl = try
    	find_variable env.scope name
	with Not_found -> raise (Error("Undeclared sub identifier " ^ name))
	in
		let typ = vdecl.var_type in
		if typ = Ast.Sub then
			let sargs = List.map2 (fun e d ->
				match e with
				Sast.Expr(exp, typ) ->
					if typ = d.var_type then e else
						raise (Error("Incorrect type of sub arg!"))
				| _ -> raise (Error("Weird error check_scall"))
			) args vdecl.args in
			Sast.Expr(Sast.Call(name, sargs), typ)
		else
    		raise (Error(name " is not a Sub!"))

and check_fcall fcall env =
	match fcall with
		KeyFuncCall(f, el) ->
			let el = List.map (fun x -> expr x env) el in
			(match f with
				Flog -> Sast.Expr(Sast.Fcall("log", el), Ast.Num)
				| Fln -> Sast.Expr(Sast.Fcall("ln", el), Ast.Num)
				| Fcos -> Sast.Expr(Sast.Fcall("cos", el), Ast.Num)
				| Fsin -> Sast.Expr(Sast.Fcall("log", el), Ast.Num)
				| _ -> raise (Error("Weird error check_fcall!"))) (* Does this work? *)
		| FuncCall(s, el) ->
			let vdecl = try
				find_variable env.scope s
			with Not_found ->
				raise (Error("undeclared function identifier " ^ name))
			in
			let el = List.map (fun x ->
				let e = expr x env in
				(match e with
					Sast.Expr(e, t) ->
						if t=Ast.Num then e else raise (Error("Non num argument supplied to fcall " ^ s ^ "!"))
					| _ -> raise (Error("Weird error check_fcall!")))
			) el in
            if vdecl.var_type=Ast.Func then
				Sast.Expr(Sast.Fcall(s, el), Ast.Num)
			else
				raise (Error(vdecl.name "not a Func!"))
		|_ -> raise (Error("Weird Error check_fcall!"))

and expr env = function
	Ast.Litnum(s) -> Sast.Expr(Ast.Litnum(s), Ast.Num)
	| Ast.Litstring(s) -> Sast.Expr(Ast.Litstring(s), Ast.String)
	| Ast.Litfunc(l, f_expr) -> check_fexpr l f_expr env
	| Ast.Litlist(l) -> check_list l env
	| Ast.Litmatrix(l) -> check_matrix l env
	| Ast.Id(name) -> check_id name env
	| Ast.Binop(e1, op, e2) -> check_binop e1 op e2 env
	| Ast.Unop(op, e) -> check_unop op e env
	| Ast.Call(name, args) -> check_scall name args env
	| Ast.FCall(fcall) -> check_fcall fcall env

let check_match ms env =
    let top = expr ms.match_top_expr env in
    match top with
    Sast.Expr(top_e, top_t) ->
    let ml = List.map (fun x->
        let me = expr x.match_expr env in
        match me with
            Sast.Expr(e, t) ->
                if (t=top_t||x.match_cmp=Ast.Any||x.match_cmp=Ast.Default) && (t=Ast.String || t=Ast.Num) then
                    {f_type=x.f_type;match_cmp=x.match_cmp;match_expr=me;match_stmt=(stmt x.match_stmt env)}
                else
                raise (Error("Match stmt type error!"))
            | _ -> raise (Error("Weird error!"))
    ) ms.match_list in
    {match_top_expr=top_e;match_list=ml}
    | _-> raise (Error("Weird error!"))

let check_assign name l e env =
	let vdecl = try
		find_local_variable env.scope name
	with Not_found ->
		let se = expr e in
		match l with
			x::x2 -> raise (Error("List " ^ name ^ " undefined!"))
			| [] ->
				let vdecl = {name=name;const=false} in
				env.scope.variables <- vdecl::env.scope.variables;
				Sast.Vdecl(name, se)
	in
    	if vdecl.const = true then
			raise (Error("Variable " ^ name ^ " is const!"))
		else
			let se = expr e in
			let sl = List.map expr l in
			Sast.Assign(name, sl, se)

let check_cassign name e env =
	let vdecl = try
		find_local_variable env.scope name
	with Not_found ->
		let se = expr e in
		let vdecl = {name=name;const=false} in
		env.scope.variables <- vdecl::env.scope.variables;
		Sast.Cdecl(name, se)
	in
	if vdecl.const = true then
        raise (Error("Local variable " ^ name ^ " already const!"))
    else
		let se = expr e in
		let vdecl = {vdecl with const=false} in
		env.scope.variables <- vdecl::env.scope.variables;
		Sast.Cdecl(name, se)

and check_eassign name l e env =
	ignore (try
		find_nonlocal_variable env.scope name
	with Not_found ->
    	raise (Error("External variable " ^ name ^ " has not been declared!")));
	let se = expr env e in
    let sl = List.map (fun x -> expr env x) l in
    Sast.Assign(name, sl, se)
       
and check_block l env =
    let new_scope = new_symbol_table Some(env.scope) [] in
    let new_env = { env with scope = new_scope } in
    let sl = List.map (fun s -> stmt new_env s) l in
    Sast.Block(new_scope, sl)

and check_sub s vdcll stml env =
    match env.scope.parent with
		Some(parent) -> raise (Error("Cannot declare sub " ^ s ^ " in nested scope"))
		| _ ->
			try
				ignore (find_local_variable env.scope s);raise (Error("Sub " ^ s ^ " already declared!"))
			with Not_found ->
				let converted_vdecl = (List.map convert_vdecl vdcll) in
				let new_scope = new_symbol_table Some(env.scope) converted_vdecl in
				let new_env = { env with scope = new_scope } in
				let sl = List.map (fun s -> stmt new_env s) sublist 0 ((List.length stml)-1) stml in
				let last = stmt new_env List.nth stml ((List.length stml)-1) in
				match last with
					Sast.Expr(e, t) ->
						let vdecl = {name=s;const=true;var_type=Ast.Sub;return_type=t;args=converted_vdecl} in
						env.scope.variables <- vdecl::env.scope.variables;
						Sast.Subdecl(s, new_scope, vdcll, sl@[last])
					| _ -> raise (Error("No return value in  " ^ s ^ "!"))

and stmt env = function
    Ast.Block(l) -> check_block l env
    | Ast.Match(ms) -> check_match ms
    | Ast.Assign(name, l, e) -> check_assign name l e env
    | Ast.Constassign(name, e) -> check_cassign name e env
    | Ast.Externassign(name, l, e) -> check_eassign name l e env
    | Ast.Expr(e) -> expr env e
    | Ast.Pass -> Ast.Pass
    | Subdecl(s, vdcll, stmtl) -> check_sub s vdcll stml env

let _ =
   let lexbuf = Lexing.from_channel stdin in
   let expr = Parser.program Scanner.token lexbuf in
   Printf.printf "done\n"