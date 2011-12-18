package com.numlang;

import java.util.Scanner;
import java.util.LinkedList;
import com.numlang.*;



public class NumLang
{
	private static LinkedList<String> Stack = new LinkedList<String>();

	public static NumLangIO		IO = new NumLangIO();		
	public static NumLangBuiltin	Builtin = new Builtin()l
	public static NumLangException 	Exception = new NumLangException();
}

class NumLangException
{
	public NumLangException(){};

	public void DivideByZero(NumValue dividend, NumValue divisor)
	{
		System.err.println("NumLang Error: Divide By Zero");
		System.exit(1);
	}

}

class NumLangBuiltin {
	public NumLangBuiltin(){}

	public String str(NumValue num) {
		return num.getValue().toString();
	}

	public NumValue num(String str) {
		return new NumValue(new BigRational(str));
	}
}

class NumLangIO
{
	private Scanner input;

	public NumLangIo(){input = new Scanner(System.in)}
	
	public void print(String str) {
		System.out.print(str);
	}

	public void println(String str) {
		System.out.println(str);
	}
}
