open Ast
open Sast
open Ssc

(* Java Syntax example: (new FuncValue(no of parameters, new Func() )) *)
let rec c_litfunc args e =
    let c_evalparams expr_list =
        let vartype ex_list =
			match List.fold_left (fun a b ->
				match b with
					Sast.Expr(_, typ) -> if (typ = Ast.Func) then Ast.Func else a
			) Ast.Num expr_list with
				Ast.Func -> "new Func[]{"
				| _ -> "new NumValue[]{"
        in
        vartype expr_list ^ List.fold_left (fun a b -> a ^ ", " ^ c_sexpr b) (c_sexpr (List.hd expr_list)) (List.tl expr_list) ^ "}"
	in
    let c_ffcall name depth exprlist = 
		match name with
			"sin" -> "(new SpecialFunc(SpecialType.SIN, new Func("
				^ c_sexpr (List.hd exprlist) ^ ")))"
			| "cos" -> "(new SpecialFunc(SpecialType.COS, new Func("
				^ c_sexpr (List.hd exprlist) ^ ")))"
			| "log" -> "(new SpecialFunc(SpecialType.LOG, new Func("
				^ c_sexpr (List.hd exprlist) ^ ")))"
			| "ln" -> "(new SpecialFunc(SpecialType.LN, new Func("
				^ c_sexpr (List.hd exprlist) ^ ")))"
			| "floor" -> "(new SpecialFunc(SpecialType.FLOOR, new Func("
				^ c_sexpr (List.hd exprlist) ^ ")))"
			| "ceil" -> "(new SpecialFunc(SpecialType.CEIL, new Func("
				^ c_sexpr (List.hd exprlist) ^ ")))"
			| _ -> "(" ^ (depth_to_us depth) ^ name ^ ".evaluate("
				^ c_evalparams exprlist ^ "))"
	in
	let sargs = String.concat ", " (List.map (fun arg -> 
		match arg with
		Sast.Expr(e, _) ->
			match e with
				Sast.Funarg(i) -> "(new Func(" ^ string_of_int i ^ "))"
				| Sast.Litnum(s) -> "(new Func(" ^ c_litnum s ^ "))"
				| Sast.Binop(e1, op, e2) -> c_fbinop e1 op e2
				| Sast.Unop(op, e) -> c_funop op e
				| Sast.FCall(s, depth, el) -> c_ffcall s depth el
				| _ -> ""
	) args)
	in "(new FuncValue (" ^ string_of_int (List.length args) ^ "," ^ sargs ^ "))"

and c_binop e1 bop e2  =
	let e1 = c_sexpr  e1 in
	let e2 = c_sexpr  e2 in
	match bop with
		Ast.Add -> "(" ^ e1 ^ ".add(" ^ e2 ^ "))"
		| Ast.Sub -> "(" ^ e1 ^ ".subtract(" ^ e2 ^ "))"
		| Ast.Mult -> "(" ^ e1 ^ ".multiply(" ^ e2 ^ "))"
		| Ast.Div -> "(" ^ e1 ^ ".divide(" ^ e2 ^ "))"
		| Ast.Mod -> "(" ^ e1 ^ ".mod(" ^ e2 ^ "))"
		| Ast.Exp -> "(" ^ e1 ^ ".exp(" ^ e2 ^ "))"
		| Ast.MatMult -> "(" ^ e1 ^ ".matmult(" ^ e2 ^ "))"
		| Ast.Lt -> "(" ^ e1 ^ ".lt(" ^ e2 ^ "))"
		| Ast.Leq -> "(" ^ e1 ^ ".leq(" ^ e2 ^ "))"
		| Ast.Gt  -> "(" ^ e1 ^ ".gt(" ^ e2 ^ "))"
		| Ast.Geq -> "(" ^ e1 ^ ".geq(" ^ e2 ^ "))"
		| Ast.Eq -> "(" ^ e1 ^ ".eq(" ^ e2 ^ "))"
		| Ast.Neq -> "(" ^ e1 ^ ".neq(" ^ e2 ^ "))"
		| Ast.Concat -> "(" ^ e1 ^ ".concat(" ^ e2 ^ "))"

and c_fbinop e1 bop e2  =
	let e1 = c_sfexpr  e1 in
	let e2 = c_sfexpr  e2 in
	match bop with
		Ast.Add -> "(new Func(" ^ e1 ^ ", BinOp.ADD, " ^ e2 ^ "))"
		| Ast.Sub -> "(new Func(" ^ e1 ^ ", BinOp.SUB, " ^ e2 ^ "))"
		| Ast.Mult -> "(new Func(" ^ e1 ^ ", BinOp.MULT, " ^ e2 ^ "))"
		| Ast.Div -> "(new Func(" ^ e1 ^ ", BinOp.DIV, " ^ e2 ^ "))"
		| Ast.Mod -> "(new Func(" ^ e1 ^ ", BinOp.EXP, " ^ e2 ^ "))"
		| Ast.Exp -> "(new Func(" ^ e1 ^ ", BinOp.MOD, " ^ e2 ^ "))"
		| Ast.Lt -> "(new Func(" ^ e1 ^ ", BinOp.LT, " ^ e2 ^ "))"
		| Ast.Leq -> "(new Func(" ^ e1 ^ ", BinOp.LEQ, " ^ e2 ^ "))"
		| Ast.Gt  -> "(new Func(" ^ e1 ^ ", BinOp.GT, " ^ e2 ^ "))"
		| Ast.Geq -> "(new Func(" ^ e1 ^ ", BinOp.GEQ, " ^ e2 ^ "))"
		| Ast.Eq -> "(new Func(" ^ e1 ^ ", BinOp.EQ, " ^ e2 ^ "))"
		| Ast.Neq -> "(new Func(" ^ e1 ^ ", BinOp.NEQ, " ^ e2 ^ "))"
		| _ -> ""

and c_unop uop e  =
	let e = c_sexpr  e in
	match uop with
		Ast.Uminus -> "(" ^ e ^ ".neg())"
		| Ast.Not -> "(" ^ e ^ ".not())"

and c_funop uop e  =
	let e = c_sfexpr  e in
	match uop with
		Ast.Uminus -> "(new Func(UnOp.UMINUS, " ^ e ^ "))"
		| Ast.Not -> "(new Func(UnOp.NOT, " ^ e ^ "))"

and c_litnum s  =
	"(new NumValue(new BigRational(\"" ^ s ^ "\")))"

and drop_Ast = function
	Ast.Num -> "NumValue"
	| Ast.String -> "StringValue"
	| Ast.Func -> "FuncValue"
	| Ast.Matrix -> "MatrixValue"
	| Ast.List(typ) -> "ListValue<" ^ drop_Ast typ ^ ">"
	| _ -> ""

and c_list el =
	let vtype = match List.hd el with Sast.Expr(_, vtype) -> vtype
	and el = List.map (fun e -> c_sexpr e) el in
	"(new ListValue<" ^ drop_Ast vtype ^ ">({" ^ String.concat "," el ^ "})"

and c_matrix ell =
	let cell = List.map (fun el -> List.map (fun e -> c_sexpr e) el) ell in
	let cel = List.map (fun el -> "{" ^ String.concat "," el ^ "}") cell in
	"(new MatrixValue(new NumValue[][]{" ^ String.concat "," cel ^ "})"

and depth_to_us depth =
	if depth = 0 then "" else "_" ^ depth_to_us (depth-1)

and c_id name depth  =
	depth_to_us depth ^ name

and c_scall name el  =
	match el with
		hd::tl ->
			name ^ ".invoke(" ^ c_sexpr  hd ^ List.fold_left (fun r e -> r ^ ", " ^ c_sexpr  e) "" tl ^ ")"
		| _ -> ""

and c_fcall name depth el  =
    "func.evaluate"

and c_sfexpr  expression =
	match expression with
		Sast.Expr(ex, _) ->
			match ex with
				Sast.Litnum(s) -> c_litnum s 
				| Sast.Litstring(s) -> "(new StringValue(\"" ^ s ^ "\"))"
				| Sast.Litfunc(args, e) -> "c_litfunc args e "
				| Sast.Litlist(el) -> c_list el 
				| Sast.Litmatrix(ell) -> c_matrix ell 
				| Sast.Id(name, depth) -> c_id name depth 
				| Sast.Binop(e1, bop, e2) -> c_binop e1 bop e2 
				| Sast.Unop(uop, e) -> c_unop uop e 
				| Sast.Call(name, el) -> c_scall name el 
				| Sast.FCall(name, depth, el) -> c_fcall name depth el 
				| Sast.Funarg(i) -> ""
				| _ -> ""

and c_sexpr  expression =
	match expression with
		Sast.Expr(ex, _) ->
			match ex with
				Sast.Litnum(s) -> c_litnum s 
				| Sast.Litstring(s) -> "(new StringValue(\"" ^ s ^ "\"))"
				| Sast.Litfunc(args, e) -> "c_litfunc args e "
				| Sast.Litlist(el) -> c_list el 
				| Sast.Litmatrix(ell) -> c_matrix ell 
				| Sast.Id(name, depth) -> c_id name depth ^ ".value()" 
				| Sast.Binop(e1, bop, e2) -> c_binop e1 bop e2 
				| Sast.Unop(uop, e) -> c_unop uop e 
				| Sast.Call(name, el) -> c_scall name el 
				| Sast.FCall(name, depth, el) -> c_fcall name depth el 
				| Sast.Funarg(i) -> ""
				| Sast.Listaccess(s, el) -> ""
				| Sast.Mataccess(s, el) -> ""
				| _ -> ""

and c_block sl  =
	let sl = List.fold_left (fun result element ->
		let stmt = c_sstmt  element in
		result ^ stmt) "" sl in
		"{" ^ sl ^ "}"

and c_assign name depth il e  =
	let cil = List.map (fun x -> c_sexpr  x) il in
	match cil with
		[] -> depth_to_us depth ^ name ^ ".assign(" ^ c_sexpr  e ^ ");\n"
		| hd::[] -> depth_to_us depth ^ name ^ ".set(" ^ hd ^ ", " ^ c_sexpr  e ^ ");\n"
		| hd::tl ->  depth_to_us depth ^ name ^ mdl_assign cil e

and mdl_assign il e  =
	match il with
		hd::tl -> ".get(" ^ hd ^ ")" ^
			(match tl with
				h::[] -> ".set(" ^ h ^ ", " ^ c_sexpr  e ^ ");\n"
				| h::t -> mdl_assign tl e 
				| _ -> "")
		| _ -> ""

and c_vdecl name depth e  =
	match e with Sast.Expr(_, typ) ->
		"final Var<" ^ drop_Ast typ ^ "> " ^ depth_to_us depth ^ name ^ " = " ^ "new Var<" ^ drop_Ast typ ^ ">(" ^ c_sexpr  e ^ ");\n"

and c_match_command topexpr matchcommand  =
	let c_match_flow matchcommand =
		match matchcommand.sf_type with
			Ast.Cont -> ""
			| Ast.Done -> "break;"
			| Ast.Loop -> "continue;"
	in
    let c_match_expr top_expr cmp mtccommand  =
		"if(" ^ c_sexpr  top_expr ^ cmp ^ "("
		^ c_sexpr  mtccommand.smatch_expr ^ "){\n"
		^ c_sstmt  mtccommand.smatch_stmt
		^ c_match_flow mtccommand ^ "}\n"
	in
	match matchcommand.smatch_cmp with
		Ast.Meq -> c_match_expr topexpr ".eq" matchcommand 
		| Ast.Mneq -> c_match_expr topexpr ".neq" matchcommand 
		| Ast.Mlt -> c_match_expr topexpr ".lt" matchcommand 
		| Ast.Mleq -> c_match_expr topexpr ".leq" matchcommand 
		| Ast.Mgt -> c_match_expr topexpr ".gt" matchcommand 
		| Ast.Mgeq -> c_match_expr topexpr ".geq" matchcommand 
		| Ast.Any -> "{" ^ c_sstmt  matchcommand.smatch_stmt ^ c_match_flow matchcommand ^ "}\n"

and c_match smatch_statement  =
    "while(true){\n" ^ List.fold_left (fun a b  -> a ^ b) "" (List.map (fun x -> c_match_command smatch_statement.smatch_top_expr x ) smatch_statement.smatch_list) ^ "break;\n}\n"

and c_sub name args stmtl =
	let (decls, _) = List.fold_left (fun result el ->
		(fst result ^ "final Var<" ^ drop_Ast el.vtype ^ "> _" ^ el.vname ^ " = " ^ "new Var<" ^ drop_Ast el.vtype ^ ">((" ^ drop_Ast el.vtype ^ ") args[" ^ string_of_int (snd result) ^ "] );\n", (snd result+1))
	) ("", 0) args in
	let stmts = List.fold_left (fun result el -> result ^ c_sstmt el) "" (head_list stmtl) in
	let stmts = stmts ^ "return " ^ c_sstmt (List.nth stmtl ((List.length stmtl)-1)) in
	"final " ^ name ^ " = new Subroutine() {\npublic Object run() {\n" ^ decls ^ stmts ^ "}\n}\n"

and c_sstmt  sstmt = match sstmt with
	Sast.Block(sl) -> c_block sl 
	| Sast.Match(smatch_statement) -> c_match smatch_statement 
	| Sast.Assign(name, depth, il, e) -> c_assign name depth il e 
	| Sast.Vdecl(name, depth, e) -> c_vdecl name depth e 
	| Sast.Cdecl(name, depth, e) -> c_vdecl name depth e 
	| Sast.Externassign(name, depth, il, e) -> c_assign name depth il e 
	| Sast.Exprstmt(e) -> (c_sexpr  e) ^ ";\n"  (* do we need \\ here? *)
	| Sast.Pass -> ""
	| Sast.Subdecl(name, vars, stmtl) -> c_sub name vars stmtl

let c_prog sstmtl =
	List.fold_left (fun result sstmt -> result ^ c_sstmt sstmt) "" 	sstmtl

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.program Scanner.token lexbuf in
	let checked_prog = Ssc.check_program prog in
    let compiled_prog = c_prog checked_prog in
    let header = "import com.numlang;\n\npublic class Runner\n{\npublic static void main(String[] args)\n{\n" in
    let footer = "}\n}\n" in
    print_string (header ^ compiled_prog ^ footer)
