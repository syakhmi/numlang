open Ast

type expr_wrapper =
    Expr of sexpr * Ast.v_type

and func_call =
   | FuncCall of string * sexpr list

and sexpr =
     Litnum of string
   | Litstring of string
   | Litfunc of string list * sexpr
   | Litlist of expr_wrapper list
   | Litmatrix of expr_wrapper list list
   | Id of string
   | Ref of expr_wrapper * expr_wrapper
   | Slice of expr_wrapper * expr_wrapper * expr_wrapper
   | Binop of expr_wrapper * bop * expr_wrapper
   | Unop of Ast.uop * expr_wrapper
   | Call of string * expr_wrapper list
   | FCall of func_call
   | Funarg of int

and sstmt =
     Block of sstmt list
   | Match of smatch_statement
   | Assign of string * expr_wrapper list  * expr_wrapper
   | Vdecl of string * expr_wrapper
   | Cdecl of string * expr_wrapper
   | Externassign of string * expr_wrapper list * expr_wrapper
   | Exprstmt of expr_wrapper
   | Pass
   | Subdecl of string * Ast.var_decl list * sstmt list

and smatch_command = {
   f_type : cftype;
   match_cmp : matchcmptype;
   match_expr : expr_wrapper;
   match_stmt : sstmt;
}

and smatch_statement = {
   match_top_expr : expr_wrapper;
   match_list : smatch_command list;
}

type sprogram = sstmt list