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
   mutable variables : variable_decl list;
   depth : int
}

let head_list l =
    List.rev (List.tl (List.rev l))

let rec sublist b e l =
 match l with
   [] -> failwith "sublist"
 | h :: t ->
    let tail = if e=0 then [] else sublist (b-1) (e-1) t in
    if b>0 then tail else h :: tail
;;

let convert_vdecl decl =
    {name=decl.vname;const=false;var_type=decl.vtype;return_type=None;args=None}

let new_symbol_table parent l =
    {parent = Some(parent); variables = [
        {name="floor";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="ceil";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="log";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="ln";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="cos";const=true;var_type=Ast.Func;return_type=None;args=None};
        {name="sin";const=true;var_type=Ast.Func;return_type=None;args=None}

]@l; depth = parent.depth + 1}

let predefined_funcs = [
    {name="floor";const=true;var_type=Ast.Func;return_type=None;args=None};
    {name="ceil";const=true;var_type=Ast.Func;return_type=None;args=None};
    {name="log";const=true;var_type=Ast.Func;return_type=None;args=None};
    {name="ln";const=true;var_type=Ast.Func;return_type=None;args=None};
    {name="cos";const=true;var_type=Ast.Func;return_type=None;args=None};
    {name="sin";const=true;var_type=Ast.Func;return_type=None;args=None}
]

let predefined_subs = [
	{name="str";const=true;var_type=Ast.Subr;return_type=Some(Ast.String);args=Some([{name="";const=false;var_type=Ast.Num;return_type=None;args=None}])};
	{name="str_func";const=true;var_type=Ast.Subr;return_type=Some(Ast.String);args=Some([{name="";const=false;var_type=Ast.Func;return_type=None;args=None}])};
	{name="num";const=true;var_type=Ast.Subr;return_type=Some(Ast.Num);args=Some([{name="";const=false;var_type=Ast.String;return_type=None;args=None}])};
	{name="scanln";const=true;var_type=Ast.Subr;return_type=Some(Ast.String);args=Some([])};
	{name="scan";const=true;var_type=Ast.Subr;return_type=Some(Ast.String);args=Some([])};
	{name="print";const=true;var_type=Ast.Subr;return_type=Some(Ast.String);args=Some([{name="";const=false;var_type=Ast.String;return_type=None;args=None}])};
	{name="println";const=true;var_type=Ast.Subr;return_type=Some(Ast.String);args=Some([{name="";const=false;var_type=Ast.String;return_type=None;args=None}])};
	{name="m";const=true;var_type=Ast.Subr;return_type=Some(Ast.Matrix);args=Some([{name="";const=false;var_type=Ast.Num;return_type=None;args=None};{name="";const=false;var_type=Ast.Num;return_type=None;args=None}])}
]

let root_symbol_table =
    {parent = None; variables = predefined_funcs@predefined_subs;depth = 0}

let rec find_variable (scope : symbol_table) name =
   try
       (List.find (fun vdecl -> name = vdecl.name) scope.variables, scope.depth)
   with Not_found ->
       match scope.parent with
           Some(parent) -> find_variable parent name
           | _ -> raise Not_found

let rec find_local_variable (scope : symbol_table) name =
   try
       (List.find (fun vdecl -> name = vdecl.name) scope.variables, scope.depth)
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
		let (vdecl, depth) = try
        	find_variable env.scope s
		with Not_found ->
			raise (Error("undeclared identifier " ^ s))
		in
		let typ = vdecl.var_type in
		Sast.Expr(Sast.Id(s, depth), typ))

and check_fexpr l f_expr env =
    match f_expr with
		Ast.Litnum(s) -> Sast.Expr(Sast.Litnum(s), Ast.Num)
    	| Ast.Id(s) -> check_fid s l env
    	| Ast.Binop(e1, op, e2) -> check_fbinop l e1 op e2 env
    	| Ast.Unop(op, e) -> check_funop l op e env
    	| Ast.FCall(ffcall) -> check_ffcall l ffcall env
		| _ -> raise (Error("Invalid function literal!"))

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

and check_matrix l env =
	try
        let returnmatrix = List.map (fun a -> List.map (fun a2 -> check_expr env a2) a) l in
			let rowlength = List.length (List.hd returnmatrix) in
        	let b = List.fold_left (fun valid row -> if List.length row <> rowlength then 0*valid else 1*valid) 1 returnmatrix
			and b2 = List.fold_left (fun valid row -> valid * List.fold_left (fun valid e -> 
				match e with Sast.Expr(_, vtype) -> if vtype <> Ast.Num then 0*valid else 1*valid
			) 1 row) 1 returnmatrix
			and rows = List.fold_left (fun rowcount row -> rowcount + 1) 0 returnmatrix
			and cols = List.fold_left (fun colcount col -> colcount + 1) 0 (List.hd returnmatrix)
		in if b=1 & b2=1 then
			Sast.Expr(Sast.Litmatrix(returnmatrix), Ast.Matrix)
		else raise (Error("Invalid matrix"))
	with Failure(hd) -> raise (Error("Invalid matrix"))

and check_list l env =
	try
		let returnlist = List.map (fun a -> check_expr env a) l in
		match List.hd returnlist with Sast.Expr(_, vartype) ->
			let b = List.fold_left (fun valid e -> match e with
				Sast.Expr(_, vtype) -> if vartype <> vtype then 0*valid else 1*valid) 1 returnlist
			and len = List.fold_left (fun i e -> i + 1) 0 returnlist
		in if b = 1 then Sast.Expr(Sast.Litlist(returnlist), Ast.List(vartype))
		else raise (Error("list of multiple types"))
	with Failure(hd) -> raise (Error("Empty list"))

and num_nested_lists = function
	Ast.List(typ) -> 1 + num_nested_lists typ
	| _ -> 0

and list_access_type typ num = 
	if num = 0 then
		typ
	else
		match typ with
			Ast.List(t) -> list_access_type t (num -1)
			| _ -> raise (Error("Invalid attempt to find list element access type!"))

and check_access s el env =
	let (vdecl, depth) = try
		find_variable env.scope s
	with Not_found ->
		raise (Error("undeclared identifier " ^ s))
	in
	let typ = vdecl.var_type in
	let sel = List.map (fun x -> check_expr env x) el in
	match List.hd sel with
		Sast.Expr(_, vartype) ->
			let nums = List.fold_left (fun valid e -> match e with
				Sast.Expr(_, vtype) -> if vartype <> vtype then 0*valid else 1*valid) 1 sel in
			(match typ with
				Ast.Matrix ->
					if (List.length sel) <> 2 || nums <> 1 then
						raise (Error("Invalid Matrix Access!"))
					else
						Sast.Expr(Sast.Mataccess(s, depth, sel), Ast.Num)
				| Ast.List(typ) ->
					let length = List.length sel in
					if length > (1 + (num_nested_lists typ)) || nums <> 1 then
						raise (Error("Invalid List Access!"))
					else
						let typ = list_access_type typ (length - 1) in
						Sast.Expr(Sast.Listaccess(s, depth, sel), typ)
				| _ -> raise (Error("Invalid element access!")))

and check_id name env =
	let (vdecl, depth) = try
		find_variable env.scope name
	with Not_found ->
		raise (Error("undeclared identifier " ^ name))
	in
	let typ = vdecl.var_type in
	Sast.Expr(Sast.Id(name, depth), typ)

and check_unop op e env =
    let e = check_expr env e in
    match e with
    	Sast.Expr(_, t) ->
    		if op = Ast.Uminus then
				if t = Ast.Num then
					Sast.Expr(Sast.Unop(op, e), Ast.Num)
				else
					if t = Ast.Func then
						Sast.Expr(Sast.Unop(op, e), Ast.Func)
					else
						if t = Ast.Matrix then
							Sast.Expr(Sast.Unop(op, e), Ast.Matrix)
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
				
and check_funop l op e env =
   let e = check_fexpr l e env in
   match e with
   	Sast.Expr(_, t) ->
   		if op = Ast.Uminus then
			if t = Ast.Num then
				Sast.Expr(Sast.Unop(op, e), Ast.Num)
			else
				if t = Ast.Func then
					Sast.Expr(Sast.Unop(op, e), Ast.Func)
				else
					if t = Ast.Matrix then
						Sast.Expr(Sast.Unop(op, e), Ast.Matrix)
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

and check_binop e1 op e2 env =
    let e1 = check_expr env e1
    and e2 = check_expr env e2 in
    match e1 with
    Sast.Expr(_, t1) ->
		match e2 with
        	Sast.Expr(_, t2) ->
    			(* Case for +,-,*,%,/,^ operators *)
    			if op = Ast.Add || op = Ast.Sub|| op = Ast.Mult || op = Ast.Mod || op = Ast.Exp then
        			if (t1 = Ast.Matrix && (t2 = Ast.Num || t2 = Ast.Matrix))|| (t2 = Ast.Matrix && t1 = Ast.Num) then
            			Sast.Expr(Sast.Binop(e1, op, e2), Ast.Matrix)
            		else if (t1 = Ast.Num && t2 = Ast.Num) then
                		Sast.Expr(Sast.Binop(e1, op, e2), Ast.Num)
                	else if (t1 = Ast.Func && (t2 = Ast.Func || t2 = Ast.Num)) || (t2 = Ast.Func && t1 = Ast.Num) then
                		Sast.Expr(Sast.Binop(e1, op, e2), Ast.Func)
            		else match op with
						Ast.Add -> raise (Error("Illegal Addition!"))
						| Ast.Sub -> raise (Error("Illegal Subtraction!"))
						| Ast.Mult -> raise (Error("Illegal Multiplication!"))
						| Ast.Mod -> raise (Error("Illegal Modulus!"))
						| Ast.Exp -> raise (Error("Illegal Exponent!"))
						| _ -> raise (Error("Weird Error! (check_binop)"))

				(* Case for # *)
				else if op = Ast.MatMult then
					if t1 = Ast.Matrix && t2 = Ast.Matrix then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Matrix)
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
						Ast.List(vartype) -> (match t2 with 
							Ast.List(vartype) -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.List(vartype))
							| _ -> raise (Error("Illegal List Concatenation!")))
						| _ -> raise (Error("Illegal Concatenation!")))					
						
				else raise (Error("Illegal Binary Operation"))

and check_fbinop l e1 op e2 env =
    let e1 = check_fexpr l e1 env
    and e2 = check_fexpr l e2 env in
    match e1 with
    Sast.Expr(_, t1) ->
		match e2 with
        	Sast.Expr(_, t2) ->
    			(* Case for +,-,*,%,/,^ operators *)
    			if op = Ast.Add || op = Ast.Sub|| op = Ast.Mult || op = Ast.Mod || op = Ast.Exp then
        			if (t1 = Ast.Matrix && (t2 = Ast.Num || t2 = Ast.Matrix))|| (t2 = Ast.Matrix && t1 = Ast.Num) then
            			Sast.Expr(Sast.Binop(e1, op, e2), Ast.Matrix)
            		else if (t1 = Ast.Num && t2 = Ast.Num) then
                		Sast.Expr(Sast.Binop(e1, op, e2), Ast.Num)
                	else if (t1 = Ast.Func && (t2 = Ast.Func || t2 = Ast.Num)) || (t2 = Ast.Func && t1 = Ast.Num) then
                		Sast.Expr(Sast.Binop(e1, op, e2), Ast.Func)
            		else match op with
						Ast.Add -> raise (Error("Illegal Addition!"))
						| Ast.Sub -> raise (Error("Illegal Subtraction!"))
						| Ast.Mult -> raise (Error("Illegal Multiplication!"))
						| Ast.Mod -> raise (Error("Illegal Modulus!"))
						| Ast.Exp -> raise (Error("Illegal Exponent!"))
						| _ -> raise (Error("Weird Error! (check_binop)"))

				(* Case for # *)
				else if op = Ast.MatMult then
					if t1 = Ast.Matrix && t2 = Ast.Matrix then
						Sast.Expr(Sast.Binop(e1, op, e2), Ast.Matrix)
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
						Ast.List(vartype) -> (match t2 with 
							Ast.List(vartype) -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.List(vartype))
							| _ -> raise (Error("Illegal List Concatenation!")))
						| _ -> raise (Error("Illegal Concatenation!")))					

				else raise (Error("Illegal Binary Operation"))

and check_scall name args env =
	try
    	let (vdecl, depth) = find_variable env.scope name in
		let typ = vdecl.var_type in
		if typ = Ast.Subr then
			match vdecl.return_type with
			Some(return_typ) -> 
			match vdecl.args with
			Some(vargs) ->
				let args = List.map (fun x ->
					check_expr env x
				) args in
				let sargs = List.map2 (fun e d ->
					match e with
					Sast.Expr(_, typ) ->
						if typ = d.var_type then e else
							raise (Error("Incorrect type of sub arg!"))
				) args vargs in
				Sast.Expr(Sast.Call(name, sargs), return_typ)
			| _ -> raise (Error("Subr " ^ name ^ " has no args def!"))
		else
    		raise (Error(name ^ " is not a Sub!"))
	with Not_found -> 	let sargs = List.map (fun x -> check_expr env x ) args in
		(match name with
		"pop" ->
			if (List.length sargs) = 1 then
				match (List.hd sargs) with
					Sast.Expr(_, List(typ)) -> Sast.Expr(Sast.Call(name, sargs), typ)
					| _ -> raise (Error("Pop requires a List as its argument"))
			else
				raise (Error("Incorrect number of args supplied to sub identifier " ^ name))
		| "rm" ->
			if (List.length sargs) = 1 then
				match (List.hd sargs) with
					Sast.Expr(_, List(typ)) -> Sast.Expr(Sast.Call(name, sargs), typ)
					| _ -> raise (Error("Rm requires a List as its argument"))
			else
				raise (Error("Incorrect number of args supplied to sub identifier " ^ name))
		| "rmi" ->
			if (List.length sargs) = 2 then
				match (List.nth sargs ((List.length sargs)-1)) with
					Sast.Expr(_, List(typ)) -> Sast.Expr(Sast.Call(name, sargs), typ)
					| _ -> raise (Error("Pop requires a List as its argument"))
			else
				raise (Error("Incorrect number of args supplied to sub identifier " ^ name))
		| _ -> raise (Error("Undeclared sub identifier " ^ name)))

and check_fcall fcall env =
	match fcall with
		Ast.KeyFuncCall(f, e) ->
			let e = check_expr env e in
			let e = (match e with
				Sast.Expr(_, typ) -> if typ = Ast.Num then e else raise (Error("NonNum Func Arg!"))) in
			(match f with
				Ast.Flog -> Sast.Expr(Sast.FCall("log", 0, [e]), Ast.Num)
				| Ast.Fln -> Sast.Expr(Sast.FCall("ln", 0, [e]), Ast.Num)
				| Ast.Fcos -> Sast.Expr(Sast.FCall("cos", 0, [e]), Ast.Num)
				| Ast.Fsin -> Sast.Expr(Sast.FCall("sin", 0, [e]), Ast.Num))
		| Ast.FuncCall(s, el) ->
			let (vdecl, depth) = try
				find_variable env.scope s
			with Not_found ->
				raise (Error("undeclared function identifier " ^ s))
			in
			let el = List.map (fun x ->
				let e = check_expr env x in
				(match e with
					Sast.Expr(_, t) ->
						if t=Ast.Num then e else raise (Error("Non num argument supplied to fcall " ^ s ^ "!")))
			) el in
            if vdecl.var_type=Ast.Func then
				Sast.Expr(Sast.FCall(s, depth, el), Ast.Num)
			else
				raise (Error(vdecl.name ^ " not a Func!"))
				
and check_ffcall l fcall env =
	match fcall with
		Ast.KeyFuncCall(f, e) ->
			let e = check_fexpr l e env in
			let e = (match e with
				Sast.Expr(_, typ) -> if typ = Ast.Num then e else raise (Error("NonNum Func Arg!"))) in
			(match f with
				Ast.Flog -> Sast.Expr(Sast.FCall("log", 0, [e]), Ast.Num)
				| Ast.Fln -> Sast.Expr(Sast.FCall("ln", 0, [e]), Ast.Num)
				| Ast.Fcos -> Sast.Expr(Sast.FCall("cos", 0, [e]), Ast.Num)
				| Ast.Fsin -> Sast.Expr(Sast.FCall("sin", 0, [e]), Ast.Num))
		| Ast.FuncCall(s, el) ->
			let (vdecl, depth) = try
				find_variable env.scope s
			with Not_found ->
				raise (Error("undeclared function identifier " ^ s))
			in
			let el = List.map (fun x ->
				let e = check_fexpr l x env in
				(match e with
					Sast.Expr(_, t) ->
						if t=Ast.Num then e else raise (Error("Non num argument supplied to fcall " ^ s ^ "!")))
			) el in
            if vdecl.var_type=Ast.Func then
				Sast.Expr(Sast.FCall(s, depth, el), Ast.Num)
			else
				raise (Error(vdecl.name ^ " not a Func!"))

and convert_fexpr = function
	Ast.FLitnum(s) -> Ast.Litnum(s)
	| Ast.FId(s) -> Ast.Id(s)
	| Ast.FBinop(e1, fb, e2) -> Ast.Binop((convert_fexpr e1), (convert_ops fb), (convert_fexpr e2))
	| Ast.FUnop(op, fe) -> Ast.Unop(op, (convert_fexpr fe))
	| Ast.FFCall(ffcall) -> Ast.FCall(convert_ffunc_call ffcall)

and convert_ffunc_call = function
	Ast.FKeyFuncCall(key, e) -> Ast.KeyFuncCall(key, convert_fexpr e)
	| Ast.FFuncCall(s, el) -> Ast.FuncCall(s, List.map (fun x -> convert_fexpr x) el)

and check_expr env = function
	Ast.Litnum(s) -> Sast.Expr(Sast.Litnum(s), Ast.Num)
	| Ast.Litstring(s) -> Sast.Expr(Sast.Litstring(s), Ast.String)
	| Ast.Litfunc(l, f_expr) -> Sast.Expr(Sast.Litfunc(l, check_fexpr l (convert_fexpr f_expr) env), Ast.Func)
	| Ast.Litlist(l) -> check_list l env
	| Ast.Litmatrix(l) -> check_matrix l env
	| Ast.Access(s, el) -> check_access s el env
	| Ast.Id(name) -> check_id name env
	| Ast.Binop(e1, op, e2) -> check_binop e1 op e2 env
	| Ast.Unop(op, e) -> check_unop op e env
	| Ast.Call(name, args) -> check_scall name args env
	| Ast.FCall(fcall) -> check_fcall fcall env
	| _ -> raise (Error("Weird Error!"))

and check_match ms env =
    let top = check_expr env ms.match_top_expr in
    match top with
    Sast.Expr(top_e, top_t) ->
    let ml = List.map (fun x->
        let me = check_expr env x.match_expr in
        match me with
            Sast.Expr(e, t) ->
                if (t=top_t||x.match_cmp=Ast.Any) && (t=Ast.String || t=Ast.Num) then
                    {sf_type=x.f_type;smatch_cmp=x.match_cmp;smatch_expr=me;smatch_stmt=(check_stmt env x.match_stmt)}
                else
                raise (Error("Match stmt type error!"))
    ) ms.match_list in
    {smatch_top_expr=top;smatch_list=ml}

and check_assign name l e env =
	try
		let (vdecl, depth) = find_local_variable env.scope name in
		if vdecl.const = true then
			raise (Error("Variable " ^ name ^ " is const!"))
		else
			let se = check_expr env e in
			match se with Sast.Expr(_, etyp) ->
				let typ = vdecl.var_type in
				let sl = List.map (fun x -> check_expr env x) l in
				if (List.length sl) = 0 then
					if etyp=vdecl.var_type then
						Sast.Assign(name, depth, sl, se)
					else raise (Error("Cannot reassign " ^ name ^ " a new type!"))
				else
					(match List.hd sl with Sast.Expr(_, vartype) ->
						let nums = List.fold_left (fun valid e -> match e with
							Sast.Expr(_, vtype) -> if vartype <> vtype then 0*valid else 1*valid) 1 sl in
						let length = List.length sl in
						(match typ with
							
							Ast.Matrix ->
								if length <> 2 || nums <> 1 then
									raise (Error("Invalid Matrix element assignment!"))
								else
									if etyp=Ast.Num then
										Sast.Assign(name, depth, sl, se)
									else raise (Error("Cannot reassign " ^ name ^ " a new type!"))
							| Ast.List(typ) ->
								if length > (1 + (num_nested_lists typ)) || nums <> 1 then
									raise (Error("Invalid List element assignment!"))
								else
									let typ = list_access_type typ (length - 1) in
									if etyp=typ then
										Sast.Assign(name, depth, sl, se)
									else raise (Error("Cannot reassign " ^ name ^ " a new type!"))
							| _ -> raise (Error("Invalid element assignment!"))))
	with Not_found ->
		let se = check_expr env e in
		match se with
			Sast.Expr(_, typ) ->
				(match l with
					x::x2 -> raise (Error("List " ^ name ^ " undefined!"))
					| [] ->
						let vdecl = {name=name;const=false;var_type=typ;return_type=None;args=None} in
						env.scope.variables <- vdecl::env.scope.variables;
						Sast.Vdecl(name, env.scope.depth, se))
    	

and check_cassign name e env =
	try
		let (vdecl, depth) = find_local_variable env.scope name in
		if vdecl.const = true then
	        raise (Error("Local variable " ^ name ^ " already const!"))
	    else
			let se = check_expr env e in
			match se with
			Sast.Expr(_, typ) ->
				if typ=vdecl.var_type then
					let vdecl = {vdecl with const=true} in
					env.scope.variables <- vdecl::env.scope.variables;
					Sast.Assign(name, env.scope.depth, [], se)
				else raise (Error("Cannot const reassign " ^ name ^ " with a new type!"))
	with Not_found ->
		let se = check_expr env e in
		match se with
			Sast.Expr(_, typ) ->
				let vdecl = {name=name;const=true;var_type=typ;return_type=None;args=None} in
				env.scope.variables <- vdecl::env.scope.variables;
				Sast.Cdecl(name, env.scope.depth, se)
		
	
and check_eassign name l e env =
	try
		let (vdecl, depth) = find_nonlocal_variable env.scope name in
		if vdecl.const = true then
			raise (Error("Extern variable " ^ name ^ " is const!"))
		else
			let se = check_expr env e in
			match se with Sast.Expr(_, etyp) ->
				let typ = vdecl.var_type in
				let sl = List.map (fun x -> check_expr env x) l in
				if (List.length sl) = 0 then
					if etyp=vdecl.var_type then
						Sast.Assign(name, depth, sl, se)
					else raise (Error("Cannot extern reassign " ^ name ^ " a new type!"))
				else
					(match List.hd sl with Sast.Expr(_, vartype) ->
						let nums = List.fold_left (fun valid e -> match e with
							Sast.Expr(_, vtype) -> if vartype <> vtype then 0*valid else 1*valid) 1 sl in
						let length = List.length sl in
						(match typ with

							Ast.Matrix ->
								if length <> 2 || nums <> 1 then
									raise (Error("Invalid extern Matrix element assignment!"))
								else
									if etyp=Ast.Num then
										Sast.Assign(name, depth, sl, se)
									else raise (Error("Cannot extern reassign " ^ name ^ " a new type!"))
							| Ast.List(typ) ->
								if length > (1 + (num_nested_lists typ)) || nums <> 1 then
									raise (Error("Invalid extern List element assignment!"))
								else
									let typ = list_access_type typ (length - 1) in
									if etyp=typ then
										Sast.Assign(name, depth, sl, se)
									else raise (Error("Cannot extern reassign " ^ name ^ " a new type!"))
							| _ -> raise (Error("Invalid extern element assignment!"))))
	with Not_found ->
		raise (Error("External variable " ^ name ^ " has not been declared!"))
       
and check_block l env =
    let new_scope = new_symbol_table env.scope [] in
    let new_env = { env with scope = new_scope } in
    let sl = List.map (fun s -> check_stmt new_env s) l in
    Sast.Block(sl)

and check_sub s vdcll stml env =
    match env.scope.parent with
		Some(parent) -> raise (Error("Cannot declare sub " ^ s ^ " in nested scope"))
		| _ ->
			try
				ignore (find_local_variable env.scope s);raise (Error("Sub " ^ s ^ " already declared!"))
			with Not_found ->
				let converted_vdecl = (List.map convert_vdecl vdcll) in
				let new_scope = new_symbol_table env.scope converted_vdecl in
				let new_env = { env with scope = new_scope } in
				let sl = List.map (fun s -> check_stmt new_env s) (head_list stml) in
				let last = check_stmt new_env (List.nth stml ((List.length stml)-1)) in
				match last with
					Sast.Exprstmt(Sast.Expr(e, t)) ->
						let vdecl = {name=s;const=true;var_type=Ast.Subr;return_type=Some(t);args=Some(converted_vdecl)} in
						env.scope.variables <- vdecl::env.scope.variables;
						Sast.Subdecl(s, vdcll, sl@[last])
					| _ -> raise (Error("No return value in  " ^ s ^ "!"))

and check_stmt env = function
    Ast.Block(l) -> check_block l env
    | Ast.Match(ms) -> Sast.Match(check_match ms env)
    | Ast.Assign(name, l, e) -> check_assign name l e env
    | Ast.Constassign(name, e) -> check_cassign name e env
    | Ast.Externassign(name, l, e) -> check_eassign name l e env
    | Ast.Expr(e) -> Sast.Exprstmt(check_expr env e)
    | Ast.Pass -> Sast.Pass
    | Ast.Subdecl(s, vdcll, stmtl) -> check_sub s vdcll stmtl env

let check_program stmtl =
	let env = {scope = root_symbol_table} in
	List.map (fun stmt ->
		check_stmt env stmt
	) stmtl

(*let _ =
	let lexbuf = Lexing.from_channel stdin in
	let prog = Parser.program Scanner.token lexbuf in
	let checked_program = check_program prog in
	Printf.printf "done\n"*)
