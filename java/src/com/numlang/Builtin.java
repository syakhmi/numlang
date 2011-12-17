package com.numlang;

public class Builtin {
	public static void print(String str) {
		System.out.print(str);
	}

	public static void println(String str) {
		System.out.println(str);
	}

	public static String str(BigRational num) {
		return num.toString();
	}

	public static BigRational num(String str) {
		return new BigRational(str);
	}
}
