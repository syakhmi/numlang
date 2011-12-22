package com.numlang;


import com.numlang.*;

public class testmain
{
	public static void main(String[] args)
	{
		NumLang.Stack.Call("__PROGRAM__");

		NumValue x;
		NumValue y;
		NumValue z;
		NumValue w;
		NumValue v;


		for(int i = 1; i < 10; i++)
		{
		for(int j = 1; j < 10; j++)
		{

		x = new NumValue(new BigRational(i));
		y = new NumValue(new BigRational(j));

		NumLang.IO.println(" x = " + x + ", y = " + y);
		NumLang.IO.println("------------------------------");
		
		NumLang.IO.println("Add:  " + x + " + " + y + " = " + x.add(y));
		NumLang.IO.println("Sub:  " + x + " - " + y + " = "+ x.subtract(y));
		NumLang.IO.println("Mult: " + x + " * " + y + " = "+ x.multiply(y));
		NumLang.IO.println("Div:  " + x + " / " + y + " = "+ x.divide(y));
		NumLang.IO.println("Exp:  " + x + " ^ " + y + " = " + x.exp(y));
		NumLang.IO.println("Mod:  " + x + " % " + y + " = " + x.mod(y));
		NumLang.IO.println("Eq:   " + x + " == " + y + " = " + x.eq(y));
		NumLang.IO.println("Neq:  " + x + " != " + y + " = " + x.neq(y));
		NumLang.IO.println("Gt:   " + x + " > " + y + " = " + x.gt(y));
		NumLang.IO.println("Geq:  " + x + " >= " + y + " = " + x.geq(y));
		NumLang.IO.println("Lt:   " + x + " < " + y + " = " + x.lt(y));
		NumLang.IO.println("Leq:  " + x + " <= " + y + " = " + x.leq(y));
		NumLang.IO.println("Not:  !" + x + " = " + x.not());
		NumLang.IO.println("Neg: -" + x + " = " + x.neg());
		NumLang.IO.print("Done---");
		NumLang.IO.println("!!!\n>>>");
		NumLang.IO.println("");
		}
		}

		/*Divide by zero test*/
		w = new NumValue(new BigRational("4"));
		v = new NumValue(new BigRational(0));
		z = w.divide(v);
		NumLang.IO.println("Passed w/v");
		z = v .divide(w);
		NumLang.IO.println("Passed v/w");

		NumLang.Stack.Return();
	}
}
