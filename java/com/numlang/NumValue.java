package com.numlang;

import com.numlang.*;

public class NumValue {
	private BigRational value;

	public NumValue(BigRational p_value) {
		this.value = p_value;
	}

	public BigRational getValue() {
		return value;
	}

	public String toString(){
		return value.toString();
	}

	/*Num Operations*/
	public NumValue add(NumValue other){
		return new NumValue(value.add(other.value));
	}
	public NumValue subtract(NumValue other){
		return new NumValue(value.subtract(other.value));
	}

	public NumValue multiply(NumValue other){
		return new NumValue(value.multiply(other.value));
	}

	public NumValue divide(NumValue other){
		if(other.getValue().isZero())
			NumLang.Exception.DivideByZero(this);
		return new NumValue(value.divide(other.value));
	}

	public NumValue exp(NumValue other){
		return new NumValue(value.pow(other.value.intValue()));
	}

	public NumValue mod(NumValue other){
		return new NumValue(value.modulus(other.value));
	}

	public NumValue eq(NumValue other){
		if(value.compareTo(other.value) == 0)
			return new NumValue(new BigRational("1"));
		return new NumValue(new BigRational("0"));
	}

	public NumValue neq(NumValue other){
		if(value.compareTo(other.value) != 0)
			return new NumValue(new BigRational("1"));
		return new NumValue(new BigRational("0"));
	}

	public NumValue lt(NumValue other){
		if(value.compareTo(other.value) < 0)
			return new NumValue(new BigRational("1"));
		return new NumValue(new BigRational("0"));
	}

	public NumValue leq(NumValue other){
		if(value.compareTo(other.value) <= 0)
			return new NumValue(new BigRational("1"));
		return new NumValue(new BigRational("0"));
	}

	public NumValue gt(NumValue other){
		if(value.compareTo(other.value) > 0)
			return new NumValue(new BigRational("1"));
		return new NumValue(new BigRational("0"));
	}

	public NumValue geq(NumValue other){
		if(value.compareTo(other.value) >= 0)
			return new NumValue(new BigRational("1"));
		return new NumValue(new BigRational("0"));
	}

	public NumValue not(){
		if(value.isZero())
			return new NumValue(new BigRational("1"));
		return new NumValue(new BigRational("0"));
	}

	public NumValue neg(){
		return new NumValue(value.neg());
	}

	/*String Operations*/
	public NumValue add(String other){
		return this.add(new NumValue(new BigRational(other.length())));
	}


	/*Func Operations*/
	public FuncValue add(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.ADD);
	}
	public FuncValue subtract(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.SUB);
	}
	public FuncValue multiply(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.MULT);
	}
	public FuncValue divide(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.DIV);
	}
	public FuncValue exp(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.EXP);
	}
	public FuncValue mod(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.MOD);
	}
	public FuncValue eq(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.EQ);
	}
	public FuncValue neq(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.NEQ);
	}
	public FuncValue lt(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.LT);
	}
	public FuncValue leq(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.LEQ);
	}
	public FuncValue gt(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.GT);
	}
	public FuncValue geq(FuncValue other){
		FuncValue temp = new FuncValue(0, new FuncValue.Func(this));
		return other.bin_front(temp, FuncValue.BinOp.GEQ);
	}


	/*Matrix Operations*/
	public MatrixValue add(MatrixValue other){
		return other.add(this);
	}

	public MatrixValue subtract(MatrixValue other){
		return other.neg().add(this);
	}

	public MatrixValue multiply(MatrixValue other){
		return other.multiply(this);
	}

	public MatrixValue divide(MatrixValue other){
		return other.divide_front(this);
	}

	public MatrixValue exp(MatrixValue other){
		return other.exp_front(this);
	}

	public MatrixValue mod(MatrixValue other){
		return other.mod_front(this);
	}

	public MatrixValue eq(MatrixValue other){
		return other.eq(this);
	}

	public MatrixValue neq(MatrixValue other){
		return other.neq(this);
	}

	public MatrixValue lt(MatrixValue other){
		return other.geq(this);
	}

	public MatrixValue leq(MatrixValue other){
		return other.gt(this);
	}

	public MatrixValue gt(MatrixValue other){
		return other.leq(this);
	}

	public MatrixValue geq(MatrixValue other){
		return other.lt(this);
	}

	/*List operations*/
	public ListValue<NumValue> concat(ListValue<NumValue> other)
	{
		return other.concatFront(this);
	}
}
