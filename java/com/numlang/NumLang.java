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
	public void InvalidStringIndex(int index)
	{
		System.err.println("NumLang Error: Invalid String Index");
		System.err.println("\tAttempted to reference string element " + index);
		printstack(2);
		System.exit(2);
	}
	public void InvalidArrayIndex(int index)
	{
		System.err.println("NumLang Error: Invalid Array Index");
		System.err.println("\tAttempted to reference array element " + index);
		printstack(2);
		System.exit(3);
	}
	public void InvalidMatrixIndex(int index)
	{
		System.err.println("NumLang Error: Invalid Matrix Index");
		System.err.println("\tAttempted to reference matrix element " + index);
		printstack(2);
		System.exit(3);
	}
	public void MatrixSizeMismatch(int r1, int c1, int r2, int c2)
	{
		System.err.println("NumLang Error: Matrix Size Mismatch");
		System.err.println("\tAttempted to perform elementwise operation on:");
		System.err.println("\tMatrix[" + r1 + ", " + c1 + "] and Matrix[" + r2 + ", " + c2 + "]");
		printstack(2);
		System.exit(4);
	}

	
	public void MatrixMultiplicationSizeMismatch(int r1, int c1, int r2, int c2)
	{
		System.err.println("NumLang Error: Matrix Multiplication Size Mismatch");
		System.err.println("\tAttempted to perform matrix multiplication operation on:");
		System.err.println("\tMatrix[" + r1 + ", " + c1 + "] and Matrix[" + r2 + ", " + c2 + "]");
		printstack(2);
		System.exit(5);
	}
	public void ListToMatrixException(int rows, int columns)
	{
		System.err.println("NumLangError: List to Matrix Exception");
		System.err.println("\tAttempted to make a matrix out of a list of dimension[" + rows + ", " + columns + "]");
		printstack(2);
		System.exit(6);
	}
	public void ListToMatrixJaggedException(int right, int wrong)
	{
		System.err.println("NumLangError: List to Matrix Jagged Exception");
		System.err.println("\tAttempted to make a matrix out of a list with different sized columns " + right + ", " + wrong);
		printstack(2);
		System.exit(7);
	}
	public void WrongFuncInputNum(int right, int wrong)
	{
		System.err.println("NumLangError: Wrong Function Input Number");
		System.err.println("\tAttempted to call a function of " + right +
					" inputs with " + wrong + "inputs");
		printstack(2);
		System.exit(8);
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

	public StringValue scanln()
	{
		return new StringValue(input.nextLine());
	}
	public StringValue scan()
	{
		return new StringValue(input.next());
	}
	
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

