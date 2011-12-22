open Ast

type expr_wrapper =
    Expr of sexpr * Ast.v_type

and sexpr =
     Litnum of string
   | Litstring of string
   | Litfunc of string list * expr_wrapper
   | Litlist of expr_wrapper list
   | Litmatrix of expr_wrapper list list
   | Mataccess of string * expr_wrapper list
   | Listaccess of string * expr_wrapper list
   | Id of string * int
   | Ref of expr_wrapper * expr_wrapper
   | Slice of expr_wrapper * expr_wrapper * expr_wrapper
   | Binop of expr_wrapper * bop * expr_wrapper
   | Unop of Ast.uop * expr_wrapper
   | Call of string * expr_wrapper list
   | FCall of string * expr_wrapper list
   | Funarg of int

and sstmt =
     Block of sstmt list
   | Match of smatch_statement
   | Assign of string * int * expr_wrapper list  * expr_wrapper
   | Vdecl of string * int * expr_wrapper
   | Cdecl of string * int * expr_wrapper
   | Externassign of string * int * expr_wrapper list * expr_wrapper
   | Exprstmt of expr_wrapper
   | Pass
   | Subdecl of string * Ast.var_decl list * sstmt list

and smatch_command = {
   sf_type : cftype;
   smatch_cmp : matchcmptype;
   smatch_expr : expr_wrapper;
   smatch_stmt : sstmt;
}

and smatch_statement = {
   smatch_top_expr : expr_wrapper;
   smatch_list : smatch_command list;
}

type sprogram = sstmt list