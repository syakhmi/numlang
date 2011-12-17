import com.numlang.*;

//This is a sample compiled file for the following program:
//
//myNum = 56.2;
//other = myNum + 54;
//sub process(num x) {
//	x + 1 + myNum;
//}
//print(process(other));
//

public class Runner {
	public static void main(String[] args) {
		final NumValue myNum = new NumValue(new BigRational("56.2"));
		final NumValue other = new NumValue(myNum.getValue().add(new BigRational("54")));
		final Subroutine process = new Subroutine() {
			public Object invoke(Object... parameters) {
				final NumValue x = new NumValue((BigRational)parameters[0]);
				return x.getValue().add(new BigRational("1")).add(myNum.getValue());
			}
		}
		Builtin.print(((BigRational)process.invoke(other.getValue())));
	}
}
