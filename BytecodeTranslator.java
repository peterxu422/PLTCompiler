import java.io.*;
import org.jfugue.*; 
 
public class BytecodeTranslator {
	/*** 
	   To compile: javac -classpath ./jfugue-4.0.3.jar BytecodeTranslator.java
	   To run: java -cp jfugue-4.0.3.jar:. BytecodeTranslator [filename]
    ***/
	public static void main(String[] args) {
		if (args.length != 2){
			System.out.println("must give a bytecode filename and a midifile name");
			System.out.println(args.length);
			System.exit(1);
		}
		String byteCodeFileName = args[0];
		String midiFileName = args[1];
		BufferedReader br = null;
 		Player player = new Player(); 
		Pattern p = new Pattern();
		String currentLine;
		String byteCode = "";
		String[] tracks = new String[16];
		String[] mixDownWrites;
		String[] firstLine;
		String option = "";

		try {

			String tempo = "T";
			int first = 1;
			br = new BufferedReader(new FileReader(byteCodeFileName));
			while ((currentLine = br.readLine()) != null) {
				if(first ==1){
					if(currentLine.charAt(0) == 'x'){
						//System.err.println("Lullabyte file failed or mixdown was not called.");
						System.exit(0);
					}
					firstLine = currentLine.split(" ");
					tempo = tempo + firstLine[0] + " ";
					option = firstLine[1];
				} else {
					byteCode += currentLine.charAt(0) + currentLine.substring(2, currentLine.length()-1) + "\n";	
				}
				first --;
			}

			mixDownWrites = byteCode.split("\n");

			for(int i = 0; i<mixDownWrites.length; i++){
				int trackNum = Integer.parseInt(String.valueOf(mixDownWrites[i].charAt(0)));
				if(tracks[trackNum] == null){
					tracks[trackNum] = mixDownWrites[i].substring(1);
				} else{
					tracks[trackNum] += mixDownWrites[i].substring(1);
				}
			}

			String midiWrite = "";

			for (int i=0;i<tracks.length;i++){
				if(tracks[i] != null){
					String track="";
					String chord="";
					if(i > 0){
						track += " " + "V" + i + " ";
					} else {
						track += "V" + i + " ";
					}

					String[] sounds = tracks[i].split("\\[");
					
					for (int j=1;j<sounds.length;j++){
						String[] chordsDurAmp = sounds[j].split(":");

						String amp = "";
						if(chordsDurAmp[2].endsWith(",")){
							amp = chordsDurAmp[2].replace(",", "");
						}else{
							amp = chordsDurAmp[2];
						}

						double durr;
						if (chordsDurAmp[1].indexOf(".") == -1){
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
					try {
						chord = chord.substring(0, chord.length()-1);	
					} catch (StringIndexOutOfBoundsException e){
						System.out.println("Mixdown must be called on an array of sounds");
						System.exit(1);
					}
					
					track += chord;
					midiWrite += track;
				}
			}

			midiWrite = tempo + midiWrite;
			p.add(midiWrite);
			if(option.equals("w")){
				System.out.println("Writing tracks to midi file");
				player.saveMidi(p, new File(midiFileName)); 
			}
			if(option.equals("p")){
				System.out.println("Playing llb tracks");
				player.play(p);
			}
			if(option.equals("b")){
				System.out.println("Writing and playing midi file");
				player.saveMidi(p, new File(midiFileName)); 
				player.play(p);
			}
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