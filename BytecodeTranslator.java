import java.io.*;
import org.jfugue.*; 
 
public class BytecodeTranslator {
	/*** 
	Andrew is not that cool
	   To compile: javac -classpath ./jfugue-4.0.3.jar BytecodeTranslator.java
	   To run: java -cp jfugue-4.0.3.jar:. BytecodeTranslator [filename]
    ***/
	public static void main(String[] args) {
		if (args.length != 1){
			System.out.println("must give a bytecode filename");
			System.exit(1);
		}
		String fileName = args[0];
		BufferedReader br = null;
 		Player player = new Player(); 
		Pattern p = new Pattern();
		String currentLine;
		String byteCode = "";
		String[] tracks;

		try {

			br = new BufferedReader(new FileReader(fileName));
			while ((currentLine = br.readLine()) != null) {
				byteCode += currentLine.substring(1, currentLine.length()-1) + "\n";	
			}

			tracks = byteCode.split("\n");

			for (int i=0;i<1/*tracks.length*/;i++){
				String track="";
				String chord="";
				//System.out.println(tracks[i]);
				//System.out.println("break");
				track += "V" + i + " ";

				String[] sounds = tracks[i].split("\\[");
				
				for (int j=1;j<sounds.length;j++){
					String[] chordsDurAmp = sounds[j].split(":");

					String amp = "";
					if(chordsDurAmp[2].endsWith(",")){
						amp = chordsDurAmp[2].replace(",", "");
					}else{
						amp = chordsDurAmp[2];
					}

					// changed this so that it supports doubles inside of the sound too
					double durr;
					if (chordsDurAmp[1].indexOf(".") == -1){
						System.out.println("no /");
						String[] rat = chordsDurAmp[1].split("/");
        				durr = Double.parseDouble(rat[0]) / Double.parseDouble(rat[1]);
					} else {
        				durr = Double.parseDouble(chordsDurAmp[1]);	
					}
					

					String[] chords = chordsDurAmp[0].split(" ");
					for(int l=0;l<chords.length-1;l++){
						chord += chords[l].substring(0, chords[l].length()-1) + "/" + durr + "a" + amp + "+";
					}
						chord += chords[chords.length-1].substring(0, chords[chords.length-1].length()-1) + "/" + durr + "a" + amp + " ";
				}
				chord = chord.substring(0, chord.length()-1);
				track += chord;
				p.add(track);
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