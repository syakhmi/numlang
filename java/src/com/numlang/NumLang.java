package com.numlang;

import java.util.Scanner;
import java.util.LinkedList;
import java.util.Iterator;
import com.numlang.*;



public class NumLang
{
	public static NumLangStack	Stack = new NumLangStack();
	public static NumLangIO		IO = new NumLangIO();		
	public static NumLangBuiltin	Builtin = new NumLangBuiltin();
	public static NumLangException 	Exception = new NumLangException();
	public static NumLangKeyFuncs	Func = new NumLangKeyFuncs();
}

class NumLangStack
{
	private LinkedList<String> Stack;
	
	public NumLangStack()
	{
		Stack = new LinkedList<String>();
	}
	public void Call(String subname){Stack.add(subname);}
	public void Return(){Stack.remove();}
	public Iterator<String> iter(){return Stack.iterator();}
}

class NumLangException
{
	public NumLangException(){};

	public void DivideByZero(NumValue dividend)
	{
		System.err.println("NumLang Error: Divide By Zero");
		System.err.println("\tAttempted to divide " + dividend.toString() + " by 0");
		printstack(2);
		System.exit(1);
	}
	public void InvalidArrayIndex(int index)
	{
		System.err.println("NumLang Error: Invalid Array Index");
		System.err.println("\tAttempted to reference array element " + index);
		printstack(2);
		System.exit(2);
	}

	private void printstack(int tab)
	{
		String tabbage = "";
		for(int i = 0; i < tab; i++)
			tabbage += "\t";

		Iterator<String> iter = NumLang.Stack.iter();
		String stack = "";
		while(iter.hasNext())
		{
			stack = tabbage + "In: " + iter.next() + "\n" + stack;
		}
		System.err.println(stack);
	}

}

class NumLangBuiltin {
	public NumLangBuiltin(){}

	public String str(NumValue num) {
		return num.toString();
	}

	public NumValue num(String str) {
		return new NumValue(new BigRational(str));
	}
}

class NumLangIO
{
	private Scanner input;

	public NumLangIO(){input = new Scanner(System.in);}
	
	public void print(String str) {
		System.out.print(str);
	}

	public void println(String str) {
		System.out.println(str);
	}
}


class NumLangKeyFuncs{
	public NumLangKeyFuncs(){}

        public NumValue sin(NumValue value)
        {   
                double temp = Math.sin(value.getValue().doubleValue());
                return new NumValue(new BigRational(temp));
        }   
        public NumValue cos(NumValue value)
        {   
                double temp = Math.cos(value.getValue().doubleValue());
                return new NumValue(new BigRational(temp));
        }   

        public NumValue ln(NumValue value)
        {   
                double temp = Math.log(value.getValue().doubleValue());
                return new NumValue(new BigRational(temp));
        }   
        public NumValue log(NumValue value)
        {   
                double temp = Math.log10(value.getValue().doubleValue());
                return new NumValue(new BigRational(temp));
        }   

        public NumValue ceil(NumValue value)
        {   
                double temp = Math.ceil(value.getValue().doubleValue());
                return new NumValue(new BigRational(temp));
        }   

        public NumValue floor(NumValue value)
        {   
                double temp = Math.floor(value.getValue().doubleValue());
                return new NumValue(new BigRational(temp));
        }   
}

