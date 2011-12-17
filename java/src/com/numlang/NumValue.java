package com.numlang;

import com.numlang.BigRational;
import com.numlang.FuncValue;

public class NumValue {
	private BigRational value;

	public NumValue(BigRational value) {
		this.value = value;
	}

	public BigRational getValue() {
		return value;
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
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.ADD);
	}
	public FuncValue subtract(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.SUB);
	}
	public FuncValue multiply(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.MULT);
	}
	public FuncValue divide(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.DIV);
	}
	public FuncValue exp(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.EXP);
	}
	public FuncValue mod(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.MOD);
	}
	public FuncValue eq(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.EQ);
	}
	public FuncValue neq(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.NEQ);
	}
	public FuncValue lt(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.LT);
	}
	public FuncValue leq(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.LEQ);
	}
	public FuncValue gt(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.GT);
	}
	public FuncValue geq(FuncValue other){
		FuncValue temp = new FuncValue(0, new Func(this));
		return other.bin_front(temp, BinOp.GEQ);
	}
}
