package com.numlang;

import com.numlang.*;

public class Var<T>
{
	private T value;
	public Var(){value = null;}
	public Var(T val){value = val;}
	public void assign(T val){value = val;}
	public T value(){return value;}
}
