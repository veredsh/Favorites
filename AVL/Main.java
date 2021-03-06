import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.StringTokenizer;

public class Main
{
	/**
	 * Notice that this method changes the standard output to a file!
	 */
	public static AVLSearchTree readFromFile(String fileName)
	{
		try
		{

			File outFile = new File("Results-" + fileName);
			PrintStream ofw = new PrintStream(outFile);

			System.setOut(ofw);

			AVLSearchTree tree = new AVLSearchTree();
			BufferedReader br = new BufferedReader(new FileReader(new File(fileName)));
			String line = br.readLine();
			while (line != null)
			{
				StringTokenizer st = new StringTokenizer(line, " ");
				String act = st.nextToken();
				if (act.equals("insert"))
				{
					String name = st.nextToken();
					String begin = st.nextToken();
					String end = st.nextToken();
					tree.insert(new Task(Integer.valueOf(begin), Integer.valueOf(end), name));

				}
				if (act.equals("delete"))
				{
					String begin = st.nextToken();
					tree.remove(new Task(Integer.valueOf(begin), Integer.valueOf(begin) + 1, "DONTCARE"));

				}
				if (act.equals("overLap"))
				{
					String begin = st.nextToken();
					String end = st.nextToken();
					MyObject answer = tree.OverlapSearch(Integer.valueOf(begin), Integer.valueOf(end));
					System.out.println(answer == null ? "No Overlap!" : answer.toString());
				}
				if (act.equals("inOrder"))
				{
					tree.printInOrder();
					System.out.println();
				}
				if (act.equals("byLevels"))
				{
					tree.printByLevels();
					System.out.println();
				}
				line = br.readLine();
			}
			return tree;
		}
		catch (IOException e)
		{
			System.err.println("Problems with the file");
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * @param args
	 */
	public static void main(String[] args)
	{
		//
		//your tests here...
		//for example:
		//readFromFile("example.txt")
		//
		//IMPORTANT!!!!! - Remember to remove your code from here before submitting the assignment!!!!
		

		//These are our tests - DON'T TOUCH!!!
		//You may comment these out while working on the assignment, but be sure to uncomment before submitting!!!
		for(int i = 1 ; i <= 10 ; i++)
			readFromFile("test" + i + ".txt");
	}
}
