package com.numlang;

import java.math.*;
import com.numlang.*;


public class FuncValue{
	public int m_params;
	public Func m_function;
	
	public static enum BinOp {	ADD, SUB, MULT, DIV, EXP, MOD,
		EQ, NEQ, LT, LEQ, GT, GEQ};
	public static enum UnOp  {	UMINUS, NOT};

	public FuncValue()
	{
		m_params = 0;
		m_function = new Func(new NumValue(new BigRational("0")));
	}
	public FuncValue(int params, Func function)
	{
		m_params = params;
		m_function = function.copy();
	}

	public NumValue evaluate(NumValue[] paramlist)
	{
		return m_function.evaluate(paramlist);
	}
	public Func evaluate(Func[] paramList)
	{
		return m_function.nest(paramList);
	}


	public FuncValue bin_front(FuncValue other, BinOp op)
	{
		int tparams = other.m_params + this.m_params;
		Func lfunc = other.m_function;
		Func rfunc = this.m_function.shift(other.m_params);
		return new FuncValue(tparams, new Func(lfunc, op, rfunc));
	}
	public FuncValue bin_back(BinOp op, FuncValue other)
	{
		int tparams = this.m_params + other.m_params;
		Func lfunc = this.m_function;
		Func rfunc = other.m_function.shift(this.m_params);
		return new FuncValue(tparams, new Func(lfunc, op, rfunc));
	}

	public FuncValue unary(UnOp op)
	{
		return new FuncValue(	this.m_params, 
					new Func(op, this.m_function.copy()));
	}
/*
	public FuncValue nest(FuncValue[] inputs)
	{
		Func[] funcs  = new Func[inputs.length];
		for(int i = 0; i < inputs.length; i++)
		{
			funcs[i] = inputs[i].m_function;
		}

		int shift = inputs[0].m_params;
		for(int i = 1; i < inputs.length; i++)
		{
			funcs[i] = funcs[i].shift(shift);
			shift += inputs[i].m_params;
		}
		return new FuncValue(shift, m_function.nest(funcs));
		
	}
*/
	public String toString()
	{
		String output = "|in[0]";
		for(int i = 1; i < m_params; i++)
			output += ", in[" + i + "]";
		output += "| -> |" + m_function.toString() + "|";
		return output;
	}

	public ListValue<FuncValue> concat(ListValue<FuncValue> other)
	{
		return other.concatFront(this);
	}

	public FuncValue add(FuncValue other){ return bin_back(BinOp.ADD, other);}
	public FuncValue subtract(FuncValue other){ return bin_back(BinOp.MULT, other);}
	public FuncValue multiply(FuncValue other){ return bin_back(BinOp.MULT, other);}
	public FuncValue divide(FuncValue other){ return bin_back(BinOp.DIV, other);}
	public FuncValue exp(FuncValue other){ return bin_back(BinOp.EXP, other);}
	public FuncValue mod(FuncValue other){ return bin_back(BinOp.MOD, other);}
	public FuncValue eq(FuncValue other){ return bin_back(BinOp.EQ, other);}
	public FuncValue neq(FuncValue other){ return bin_back(BinOp.NEQ, other);}
	public FuncValue lt(FuncValue other){ return bin_back(BinOp.LT, other);}
	public FuncValue leq(FuncValue other){ return bin_back(BinOp.LEQ, other);}
	public FuncValue gt(FuncValue other){ return bin_back(BinOp.GT, other);}
	public FuncValue geq(FuncValue other){ return bin_back(BinOp.GEQ, other);}

	public FuncValue add(NumValue other){ return new FuncValue(m_params, m_function.add(other));}
	public FuncValue subtract(NumValue other){ return new FuncValue(m_params, m_function.subtract(other));}
	public FuncValue multiply(NumValue other){ return new FuncValue(m_params, m_function.multiply(other));}
	public FuncValue divide(NumValue other){ return new FuncValue(m_params, m_function.divide(other));}
	public FuncValue exp(NumValue other){ return new FuncValue(m_params, m_function.exp(other));}
	public FuncValue mod(NumValue other){ return new FuncValue(m_params, m_function.mod(other));}
	public FuncValue eq(NumValue other){ return new FuncValue(m_params, m_function.eq(other));}
	public FuncValue neq(NumValue other){ return new FuncValue(m_params, m_function.neq(other));}
	public FuncValue lt(NumValue other){ return new FuncValue(m_params, m_function.lt(other));}
	public FuncValue leq(NumValue other){ return new FuncValue(m_params, m_function.leq(other));}
	public FuncValue gt(NumValue other){ return new FuncValue(m_params, m_function.gt(other));}
	public FuncValue geq(NumValue other){ return new FuncValue(m_params, m_function.geq(other));}
	public FuncValue neg(){return new FuncValue(m_params, m_function.neg());}
	public FuncValue not(){return new FuncValue(m_params, m_function.not());}


	public static class Func{
		public static enum FuncType {	BINOP, UNOP, CONST, VAR};

		public	int 		m_index;
		public  NumValue 	m_value;
		public  Func 		m_left;
		public  Func	 	m_right;
		public  BinOp 		m_bop;
		public  UnOp		m_uop;
		public  FuncType 	m_type;


		public Func(Func left, BinOp op, Func right)
		{
			m_type = FuncType.BINOP;
			m_left = left;
			m_right = right;
			m_bop = op;
		}
		public Func(UnOp op, Func value)
		{
			m_type = FuncType.UNOP;
			m_right = value;
			m_uop = op;
		}
		public Func(int index)
		{
			m_type = FuncType.VAR;
			m_index = index;
		}
		public Func(NumValue value)
		{
			m_type =  FuncType.CONST;
			m_value = value;
		}
		public Func(Func other)
		{
			m_type = other.m_type;
			m_value = other.m_value;
			m_left = other.m_left;
			m_right = other.m_right;
			m_index = other.m_index;
			m_bop = other.m_bop;
			m_uop = other.m_uop;
		}
		public Func copy()
		{
			switch(m_type)
			{
				case VAR:
					return new Func(m_index);
				case CONST:
					return new Func(m_value);
				case UNOP:
					return new Func(m_uop, m_right);
				case BINOP:
					return new Func(m_left, m_bop, m_right);
				default:
					return null;
			}
		}


		public NumValue evaluate(NumValue[] params)
		{
			switch(m_type)
			{
				case VAR:
					return params[m_index];
				case CONST:
					return m_value;
				case UNOP:
					return this.unary(params);
				case BINOP:
					return this.binary(params);
				default:
					return null;
			}
		}

		private NumValue unary(NumValue[] params)
		{
			switch(m_uop)
			{
				case UMINUS:
					return m_right.evaluate(params).neg();
				case NOT:
					return m_right.evaluate(params).not();
				default:
					return null;
			}
		}

		private NumValue binary(NumValue[] params)
		{
			switch(m_bop)
			{
				case ADD:
					return m_left.evaluate(params).add(m_right.evaluate(params));
				case SUB:
					return m_left.evaluate(params).subtract(m_right.evaluate(params));
				case MULT:
					return m_left.evaluate(params).multiply(m_right.evaluate(params));
				case DIV:
					return m_left.evaluate(params).divide(m_right.evaluate(params));
				case EXP:
					return m_left.evaluate(params).exp(m_right.evaluate(params));
				case MOD:
					return m_left.evaluate(params).mod(m_right.evaluate(params));
				case EQ:
					return m_left.evaluate(params).eq(m_right.evaluate(params));
				case NEQ:
					return m_left.evaluate(params).neq(m_right.evaluate(params));
				case LT:
					return m_left.evaluate(params).lt(m_right.evaluate(params));
				case LEQ:
					return m_left.evaluate(params).leq(m_right.evaluate(params));
				case GT:
					return m_left.evaluate(params).gt(m_right.evaluate(params));
				case GEQ:
					return m_left.evaluate(params).geq(m_right.evaluate(params));
				default:
					return null;
			}
		}

		public Func shift(int shift)
		{
			switch(m_type)
			{
				case VAR:
				 	return new Func(m_index + shift);
				case CONST:
					return this;
				case UNOP:
					return new Func(m_uop, m_right.shift(shift));
				case BINOP:
					return new Func(m_left.shift(shift),
							m_bop, m_right.shift(shift));
				default: 	return null;
			}
		}

		public Func nest(Func[] params)
		{
			switch(m_type)
			{
				case VAR:
				 	return params[m_index].copy();
				case CONST:
					return this;
				case UNOP:
					return new Func(m_uop, m_right.nest(params));
				case BINOP:
					return new Func(m_left.nest(params),
							m_bop, m_right.nest(params));
				default: 	return null;
			}
		}

		private String bopString()
		{
			switch(m_bop)
			{
				case ADD:	return "+";
				case SUB:	return "-";
				case MULT:	return "*";
				case DIV:	return "/";
				case EXP:	return "^";
				case MOD:	return "%";
				case EQ:	return "==";
				case NEQ:	return "!=";
				case LT:	return "<";
				case LEQ:	return "<=";
				case GT:	return ">";
				case GEQ:	return ">=";
				default:	return "";
			}
		}
		public Func add(NumValue other){return new Func(this, BinOp.ADD, new Func(other));}
		public Func subtract(NumValue other){return new Func(this, BinOp.SUB, new Func(other));}
		public Func multiply(NumValue other){return new Func(this, BinOp.MULT, new Func(other));}
		public Func divide(NumValue other){return new Func(this, BinOp.DIV, new Func(other));}
		public Func exp(NumValue other){return new Func(this, BinOp.EXP, new Func(other));}
		public Func mod(NumValue other){return new Func(this, BinOp.MOD, new Func(other));}
		public Func eq(NumValue other){return new Func(this, BinOp.EQ, new Func(other));}
		public Func neq(NumValue other){return new Func(this, BinOp.NEQ, new Func(other));}
		public Func lt(NumValue other){return new Func(this, BinOp.LT, new Func(other));}
		public Func leq(NumValue other){return new Func(this, BinOp.LEQ, new Func(other));}
		public Func gt(NumValue other){return new Func(this, BinOp.GT, new Func(other));}
		public Func geq(NumValue other){return new Func(this, BinOp.GEQ, new Func(other));}
		public Func neg(){return new Func(UnOp.UMINUS, this);}
		public Func not(){return new Func(UnOp.NOT, this);}

		private String uopString()
		{
			switch(m_uop)
			{
				case UMINUS: 	return "-";
				case NOT:	return "!";
				default:	return "";
			}
		}

		public String toString()
		{
			switch(m_type)
			{
				case VAR: 	return "in[" + m_index + "]";
				case CONST:	return m_value.toString();
				case UNOP: 	return uopString() + "(" + m_right.toString() + ")";
				case BINOP:	return 	"(" + m_left.toString() + " " + bopString() +
							" " + m_right.toString() + ")";
				default: 	return "";
			}
		}
	}

	public static class SpecialFunc extends Func
	{
		public static enum SpecialType {SIN, COS, LN, LOG, CEIL, FLOOR};

		public Func		 m_func;
		public SpecialType	 m_utype;

		public SpecialFunc(SpecialType type, Func input)
		{
			super(new NumValue(new BigRational(0)));
			m_func = input;
			m_utype = type;
		}

		public SpecialFunc copy()
		{
			return new SpecialFunc(m_utype, m_func.copy());
		}

		public NumValue evaluate(NumValue[] params)
		{
			switch(m_utype)
			{
				case SIN:
					return NumLang.Func.sin(m_func.evaluate(params));
				case COS:
					return NumLang.Func.cos(m_func.evaluate(params));
				case LN:
					return NumLang.Func.ln(m_func.evaluate(params));
				case LOG:
					return NumLang.Func.log(m_func.evaluate(params));
				case CEIL:
					return NumLang.Func.ceil(m_func.evaluate(params));
				case FLOOR:
					return NumLang.Func.floor(m_func.evaluate(params));
				default:
					return null;
			}
		}
		public SpecialFunc shift(int shift)
		{
			return new SpecialFunc(m_utype, m_func.shift(shift));
		}
		public SpecialFunc nest(Func[] params)
		{
			return new SpecialFunc(m_utype, m_func.nest(params));
		}

		public String toString()
		{
			switch(m_utype)
			{
				case SIN:
					return "sin(" + m_func.toString() + ")";
				case COS:
					return "cos(" + m_func.toString() + ")";
				case LN:
					return "ln(" + m_func.toString() + ")";
				case LOG:
					return "log(" + m_func.toString() + ")";
				case CEIL:
					return "ceil(" + m_func.toString() + ")";
				case FLOOR:
					return "floor(" + m_func.toString() + ")";
				default:
					return null;
			}
		}
	}



}


