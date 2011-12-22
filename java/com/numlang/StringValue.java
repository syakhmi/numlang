package com.numlang;

import com.numlang.*;

public class StringValue {
	private String value;

	public StringValue(String value) 
	{
		this.value = value;
	}

	public StringValue(NumValue value)
	{
		this.value = value.getValue().toString();
	}

	public StringValue(FuncValue.Func value)
	{
		this.value = value.toString();
	}
	
	public StringValue(FuncValue value)
	{
		this.value = value.toString();
	}

	public String getValue()
	{
		return value;
	}
	
	public StringValue concat(StringValue other)
	{
		return new StringValue(this.getValue() + other.getValue());
	}

	public ListValue<StringValue> concat(ListValue<StringValue> other)
	{
		return other.concatFront(this);
	}
	
	public StringValue get(NumValue index)
	{
		int i = c_index(index);
		return new StringValue(this.value.substring(i, i+1));
	}

	public NumValue length()
	{
		return new NumValue(new BigRational(this.value.length()));
	}

	public StringValue slice(NumValue a, NumValue b)
	{
		int i = c_index(a);
		int j = c_index(b);
		return new StringValue(this.value.substring(i, j));
	}
	public NumValue toNum()
	{
		return new NumValue(new BigRational(this.value));
	}
	public String toString(){ return value.toString();}

	private int c_index(NumValue index)
	{
		int i = NumLang.Func.floor(index).getValue().intValue();
		if(i < 0 || i >= value.length())
			NumLang.Exception.InvalidStringIndex(i);
		return i;
	}

	
}
