import org.junit.Assert;
import org.junit.Test;

public class SECalcTest {

	@Test
	public void test() {
		
		//Declare the input variables to test
		double output = GenericStdErrCalUtility.calculateSE(20, 200, 15, 200);
		Assert.assertEquals(1.76, output, 2);
		
		
	}

}
