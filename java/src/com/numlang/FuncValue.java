package com.numlang

import java.Math.*;
import com.numlang.NumValue;

public class FuncValue{
	private int m_params;
	private Func m_function;

	public FuncValue()
	{
		m_params = 0;
		m_function = new Func(new NumValue("0"));
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

	public FuncValue bin_front(Func_value other, BinOp op)
	{
		int tparams = other.m_params + this.m_params;
		Func lfunc = other.m_function.copy();
		Func rfunc = this.m_function.shiftparams(other.m_params);
		return new FuncValue(tparams, new Func(lfunc, op, rfunc);
	}
	public FuncValue bin_back(BinOp op, Func_value other)
	{
		int tparams = this.m_params + other.m_params;
		Func lfunc = this.m_function.copy();
		Func rfunc = other.m_function.shiftparams(this.m_params);
		return new FuncValue(tparams, new Func(lfunc, op, rfunc);
	}

	public FuncValue unary(UnOp op)
	{
		return new FuncValue(op, this.m_function.copy());
	}

	public FuncValue nest(FuncValue[] inputs)
	{
		FuncValue[] local_inputs = new FuncValue[inputs.length];
		int shift = local_inputs[0].m_params;
		for(int i = 1; i < local_inputs.length; i++)
		{
			local_inputs[i].shift(shift);
			shift +=local_inputs[i].m_params;
		}
		return new FuncValue(shift, m_function.nest(local_inputs));
		
	}

}

public enum BinOp {	ADD, SUB, MULT, DIV, EXP, MOD,
			MX, EQ, NEQ, LT, LEQ, GT, GEQ};
public enum UnOp  {	UMINUS, NOT};



class Func{
	private enum FuncType {	BINOP, UNOP, CONST, VAR}

	private int 		m_index;
	private NumValue 	m_value;
	private Func 		m_left;
	private Func	 	m_right;
	private BinOp		m_bop;
	private UnOp		m_uop;
	private FuncType 	m_type;


	public Func(Func left, BinOp op, Func right)
	{
		m_type = BINOP;
		m_left = left;
		m_right = right;
		m_bop = op;
	}
	public Func(UnOp op, Func right)
	{
		m_type = UNOP;
		m_right = value;
		m_uop = op;
	}
	public Func(int index)
	{
		m_type = VAR;
		m_index = index;
	}
	public Func(NumValue value)
	{
		m_type =  CONST;
		m_value = value
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
				return new Func(m_left, m_uop, m_right);
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
		}
	}

	public Func shift(int shift)
	{
		switch(m_type)
		{
			case VAR:
			 	return new Func(m_index + shift);
			case CONST:
				return this.copy();
			case UNOP:
				return new Func(m_op, m_right.shift(shift));
			case BINOP:
				return new Func(m_left.shift(shift),
						m_op, m_right.shift(shift));
			default: 	return null
		}
	}

	public Func nest(Func[] params)
	{
		switch(m_type)
		{
			case VAR:
			 	return params[m_index].copy();
			case CONST:
				return this.copy();
			case UNOP:
				return new Func(m_op, m_right.nest(params));
			case BINOP:
				return new Func(m_left.nest(params),
						m_op, m_right.nest(params));
			default: 	return null
		}
	}
}


class KeyFuncs{
	public static NumValue func_sin(NumValue value){}
	public static NumValue func_cos(NumValue value){}
	public static NumValue func_ln(NumValue value){}
	public static NumValue func_log(NumValue value, NumValue exp){}
	public static NumValue func_ceil(NumValue value){}
	public static NumValue func_floor(NumValue value){}

}
