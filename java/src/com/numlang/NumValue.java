package com.numlang;

import com.numlang.BigRational;

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
	public NumValue add(StringValue other){
	}


	/*Func Operations*/
	public NumLangFunc add(NumLangFunc other){
	}
	

}
