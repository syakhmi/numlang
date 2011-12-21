 package com.numlang;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import com.numlang.*;

public class ListValue<T>
{
	private LinkedList<T> m_list;

	public ListValue()
	{
		m_list = new LinkedList<T>();
	}

/*
	public ListValue(ListValue other)
	{
		m_list = other.copy();
	}
	*/

	public void push(T element)
	{
		m_list.add(0, element);
	}

	public void add(T element)
	{
		m_list.addLast(element);
	}

	public T get(NumValue index)
	{
		return m_list.get(c_index(index));
	}

	public T pop()
	{
		return m_list.removeFirst();
	}
	public T remove()
	{
		return m_list.removeLast();
	}
	public T remove(NumValue index)
	{
		return m_list.remove(c_index(index));
	}
	public void set(NumValue index, T value)
	{
		int i = c_index(index);
		m_list.set(i, value);
	}
	public NumValue length()
	{
		NumValue x = new NumValue(new BigRational(m_list.size()));
		return x;
	}

	public ListValue slice(NumValue a, NumValue b)
	{
		int i = c_index(a);
		int j = c_index(b);

		ListValue<T> list = new ListValue<T>();

		for(int k = i; k < j; k++)
		{
			list.add(m_list.get(k));
		}
		return list;
	}

	public ListValue concat(ListValue other)
	{
		ListValue<T> list = new ListValue<T>();
		Iterator<T> iter = this.m_list.iterator();
		while(iter.hasNext())
			list.add(iter.next());
		iter = other.m_list.iterator();
		while(iter.hasNext())
			list.add(iter.next());

		return list;
	}

	private int c_index(NumValue index)
	{
		int i = NumLang.Func.floor(index).getValue().intValue();
		if(i < 0 || i >= m_list.size())
			NumLang.Exception.InvalidArrayIndex(i);
		return i;
	}
}
