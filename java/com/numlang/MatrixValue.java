package com.numlang;

import com.numlang.*;

public class MatrixValue
{
	private int m_rows;
	private int m_cols;
	private NumValue[][] m_elements;

	public MatrixValue(NumValue[][] elements)
	{
		m_rows = elements.length;
		m_cols = elements[0].length;
		m_elements = elements;
	}
	public MatrixValue(int rows, int cols)
		m_rows = rows;
		m_cols = cols;
		m_elements = new NumValue[rows][cols];
		for(int i = 0; i < m_rows; i++)
			for(int j = 0; j < m_cols; j++)
				m_elements[i][j] = new NumValue(new BigRational(0));

	public MatrixValue(ListValue<ListValue<NumValue>> elements)
	{
		ListValue<NumValue> temp[] = elements.convToArray();
		if(temp == null)
			NumLang.Exception.ListToMatrixException(0, 0);
		m_rows = temp.length;

		if(temp[0].length().getValue().intValue() <= 0)
			NumLang.Exception.ListToMatrixException(m_rows, 0);

		m_cols = temp[0].length().getValue().intValue();
		m_elements = new NumValue[m_rows][m_cols];
		m_elements[0] = temp[0].convToArray();

		for(int i = 1; i < m_rows; i++)
		{
			int tmpcol = temp[i].length().getValue().intValue();
			if (tmpcol != m_cols)
				NumLang.Exception.ListToMatrixJaggedException(m_cols, tmpcol);
			m_elements[i] = temp[i].convToArray();
		}
	}

	public NumValue get(NumValue row, NumValue col)
	{
		int i = c_index(row, false);
		int j = c_index(col, true);
		
		return m_elements[i][j];
	}
	public void set(NumValue row, NumValue col, NumValue value)
	{
		int i = c_index(row, false);
		int j = c_index(col, true);
		m_elements[i][j] = value;
	}

	/*Matrix operations*/

	public MatrixValue add(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].add(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue subtract(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].subtract(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue multiply(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].multiply(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue divide(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].divide(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue exp(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].exp(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue mod(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].mod(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue eq(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].eq(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue neq(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].neq(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue lt(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].lt(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue leq(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].leq(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue gt(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].gt(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue geq(MatrixValue other)
	{
		sizecheck(other);

		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].geq(
					other.m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);

	}

	public MatrixValue neg()
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].neg();
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue not()
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].not();
			}
		}
		return new MatrixValue(elements);
	}

	/*Matrix Multiplication*/
	public MatrixValue matmult(MatrixValue other)
	{
		if (m_cols != other.m_rows)
			NumLang.Exception.MatrixMultiplicationSizeMismatch(
				m_rows, m_cols, other.m_rows, other.m_cols);

		NumValue[][] elements = new NumValue[m_rows][other.m_cols];
		NumValue temp;
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < other.m_cols; j++)
			{
				temp = new NumValue(new BigRational(0));
				for(int k = 0; k < m_cols; k++)
				{
					temp = temp.add(
						m_elements[i][k].multiply(
							other.m_elements[k][j]));
				}
				elements[i][j] = temp;
			}
		}

		return new MatrixValue(elements);
		
	}

	/*NumValue Operations*/
	public MatrixValue add(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].add(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue subtract(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].subtract(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue multiply(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].multiply(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue divide(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].divide(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue divide_front(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = other.divide(m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);
	}


	public MatrixValue exp(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].exp(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue exp_front(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = other.exp(m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue mod(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].mod(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue mod_front(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = other.mod(m_elements[i][j]);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue eq(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].eq(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue neq(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].neq(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue lt(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].lt(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue leq(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].leq(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue gt(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].gt(other);
			}
		}
		return new MatrixValue(elements);
	}

	public MatrixValue geq(NumValue other)
	{
		NumValue[][] elements = new NumValue[m_rows][m_cols];
		for(int i = 0; i < m_rows; i++)
		{
			for(int j = 0; j < m_cols; j++)
			{
				elements[i][j] = m_elements[i][j].geq(other);
			}
		}
		return new MatrixValue(elements);
	}


	private void sizecheck(MatrixValue other)
	{
		if(	(m_rows != other.m_rows) ||
			(m_cols != other.m_cols))
		{
			NumLang.Exception.MatrixSizeMismatch(
				m_rows, m_cols, other.m_rows, other.m_cols);
		}
	}

	/*List operations*/
        public ListValue<MatrixValue> concat(ListValue<MatrixValue> other)
        {   
                return other.concatFront(this);
        } 
	private int c_index(NumValue index, boolean bcol)
        {   
                int i = NumLang.Func.floor(index).getValue().intValue();
                
		if(bcol && (i < 0 || i >= m_cols)
			NumLang.Exception.InvalidMatrixIndex(i);
		else if (i < 0 || i >= m_rows)
                        NumLang.Exception.InvalidMatrixIndex(i);
                return i;
        }
}
