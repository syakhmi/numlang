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
}
