import java.io.*;
import org.jfugue.*; 
 
public class BytecodeTranslator {

	/*** 
	*  Written by the best, the greatest, and the fifty-first best-looking, 
	   Mister smarty pants,
	   The guy with the heart of gold,

	   Andrew Langdon the Third


	   To compile: javac -classpath ./jfugue-4.0.3.jar BytecodeTranslator.java
	   To run: java -cp jfugue-4.0.3.jar:. BytecodeTranslator [filename]
	*
    */
 
	public static void main(String[] args) {

		if (args.length != 1){
			System.out.println("must give a bytecode filename");
			System.exit(1);
		}

		String fileName = args[0];

		BufferedReader br = null;

 		Player player = new Player(); 

		try {

			String sCurrentLine;
 
			br = new BufferedReader(new FileReader(fileName));
 			Pattern p = new Pattern();

			while ((sCurrentLine = br.readLine()) != null) {
				p.add(sCurrentLine);
			}

			player.saveMidi(p, new File("music-file.mid")); 
			player.play(p);
 			
		} catch (IOException e) {

			e.printStackTrace();

		} finally {

			try {
				if (br != null)br.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}

		}
	}
}