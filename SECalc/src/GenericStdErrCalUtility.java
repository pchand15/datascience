

/* This class deals with the calculation of Standard Error of the difference of the two population/sample means.           
 * Getter Setter for sd1 and sd2 has been defined if the values are required to calculate t-statistic and p-value in future.
 * Enhancement: A generic class for getting and setting the SD and SE values.
 * 
 */

//import classes
import java.util.Scanner;

public class GenericStdErrCalUtility {
	public static double sd1;
	public static double sd2;
	public static int obs1;
	public static int obs2;
	
	//Declare the variables calculated based on input to be private. For usage of these vars use Getter Setter method.
	private static Scanner scanInput;
	private static double squaredSD1 = 0.0;
	private static double squaredSD2 = 0.0;
	private static double stdErr = 0.0;
	private static double stdErr1=0.0;
	private static double stdErr2=0.0;

	public static void main(String[] args) {
		scanInput = new Scanner(System.in);
		/* Enter the inputs required for calculating SE. */
		// Enter the values for population mean 1. SD, number of observations
		System.out.println("Enter the SD for population mean 1:");
		sd1 = scanInput.nextDouble();
		System.out.println("Enter the number of observations for population mean 1:");
		obs1 = scanInput.nextInt();
		System.out.println("Enter the SD for population mean 2:");
		sd2 = scanInput.nextDouble();
		System.out.println("Enter the number of observations for population mean 2:");
		obs2 = scanInput.nextInt();

		try {
			// GenericStdErrCalUtility stdErrUtilObj = new
			// GenericStdErrCalUtility();
			GenericStdErrCalUtility.calculateSE(sd1, obs1, sd2, obs2);
			System.out.println("Calculated Standard Error for two population means is: "+ stdErr);

		} catch (Exception e) {
			System.err.println("Error in main: " + e.getStackTrace());
		}

	}

	public static double calculateSE(double sd1, int obs1, double sd2, int obs2) {

		try {
			squaredSD1 = Math.pow(sd1, 2);
			squaredSD2 = Math.pow(sd2, 2);
			stdErr1 = squaredSD1/obs1;
			stdErr2 = squaredSD2/obs2;
			stdErr = Math.sqrt(stdErr1 + stdErr2);
		} catch (Exception e) {
			System.out.println("Exception in calculateSE(): " + e.getStackTrace());
		}
		return (stdErr);
	}

	//Getter Setter for SD1 and SD2 values
	public static double getSquaredSD1() {
		return squaredSD1;
	}

	public static double setSquaredSD1(double squaredSD1) {
		return GenericStdErrCalUtility.squaredSD1 = squaredSD1;
	}

	public static double getSquaredSD2() {
		return squaredSD2;
	}

	public static double setSquaredSD2(double squaredSD2) {
		return GenericStdErrCalUtility.squaredSD2 = squaredSD2;
	}

}
