package org.example;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

public class Imperativ {
	static Boolean is_in_album = false;
	static Boolean is_in_track = false;
	static int current_album = 0;
	static int current_track = 0;
	static int current_character = 0;
	
	static String file_path = ("\\src\\main\\resources\\alben.xml");
	static ArrayList<Album> albums = new ArrayList<>();
	
	public static void main(String[] args) throws IOException {
		//liest aus der alben.xml die Daten als byte Array ein
		Path currentRelativePath = Paths.get("");
		String currentDir = currentRelativePath.toAbsolutePath().toString();
		Path fullPath = Paths.get(currentDir, file_path);
		byte[] file_contents = Files.readAllBytes(fullPath);

		while(current_character < file_contents.length){
			//überspringt unwichtige zeichen
			if(file_contents[current_character] == '\n' || file_contents[current_character] == '\r' || file_contents[current_character] == '\t'){
				current_character++;
			}
			//<album>: erstellt neues album und setzt album flag
			else if(new String(file_contents, current_character, 7, StandardCharsets.UTF_8).equals("<album>")){
				current_album = albums.size();
				albums.add(new Album());
				is_in_album = true;
				current_character += 7;
			}
			//</album>: löscht album flag
			else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals("</album>")){
				is_in_album = false;
				current_character += 8;
			}
			//<title>: liest den Namen, in dem er den Titel bis zum nächsten < Zeichen liest
			//und hier kann der title für ein Album oder ein Track sein, daher die Flags nutzen
			else if(new String(file_contents, current_character, 7, StandardCharsets.UTF_8).equals("<title>")){
				current_character += 7;
				int title_length = 0;
				while(file_contents[current_character + title_length] != '<')
					title_length++;
				String title = new String(file_contents, current_character, title_length, StandardCharsets.UTF_8);
				current_character += title_length;
				current_character += 8;

				if(is_in_track)
					albums.get(current_album).tracks.get(current_track).title = title;
				else if(is_in_album)
					albums.get(current_album).title = title;

			}
			//<artist>: liest den Namen über die gleiche idee aus und setzt ihn für das momentane Album
			else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals("<artist>")){
				current_character += 8;
				int artist_length = 0;
				while(file_contents[current_character + artist_length] != '<')
					artist_length++;
				String artist = new String(file_contents, current_character, artist_length, StandardCharsets.UTF_8);
				current_character += artist_length + 9;
				albums.get(current_album).artist = artist;
			}
			//<rating>: liest das Rating über die gleiche idee aus und setzt ihn für den momentanen Track
			else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals("<rating>")){
				current_character += 8;
				int rating_length = 0;
				while(file_contents[current_character + rating_length] != '<')
					rating_length++;
				String rating = new String(file_contents, current_character, rating_length, StandardCharsets.UTF_8);
				albums.get(current_album).tracks.get(current_track).rating = Integer.parseInt(rating);
				current_character += rating_length + 9;
			}
			//<track>: eintritt in Track
			else if(new String(file_contents, current_character, 7, StandardCharsets.UTF_8).equals("<track>")){
				is_in_track = true;
				current_character += 8;
				current_track = albums.get(current_album).tracks.size();
				albums.get(current_album).tracks.add(new Track());
			}
			//</track>: austritt aus Track
			else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals("</track>")){
				is_in_track = false;
				current_character += 9;
			}
			//<feature>: fügt dem momentanen Track das Feature hinzu
			else if(new String(file_contents, current_character, 9, StandardCharsets.UTF_8).equals("<feature>")){
				current_character += 9;
				int feature_length = 0;
				while(file_contents[current_character + feature_length] != '<')
					feature_length++;
				String feature = new String(file_contents, current_character, feature_length, StandardCharsets.UTF_8);
				albums.get(current_album).tracks.get(current_track).features.add(feature);
				current_character += feature_length + 10;
			}
			//<length>: fügt dem momentanen Track die Length hinzu
			else if(new String(file_contents, current_character, 8, StandardCharsets.UTF_8).equals("<length>")){
				current_character += 8;
				int length_length = 0;
				while(file_contents[current_character + length_length] != '<')
					length_length++;
				String length = new String(file_contents, current_character, length_length, StandardCharsets.UTF_8);
				albums.get(current_album).tracks.get(current_track).length = length;
				current_character += length_length + 9;
			}
			//<writing>: fügt dem momentanen Track das Writing hinzu
			else if(new String(file_contents, current_character, 9, StandardCharsets.UTF_8).equals("<writing>")){
				current_character += 9;
				int writing_length =  0;
				while(file_contents[current_character + writing_length] != '<')
					writing_length++;
				String writing = new String(file_contents, current_character, writing_length, StandardCharsets.UTF_8);
				albums.get(current_album).tracks.get(current_track).writers.add(writing);
				current_character += writing_length + 10;
			}
			//<date>: fügt dem momentanen Album das Datum hinzu
			else if(new String(file_contents, current_character, 6, StandardCharsets.UTF_8).equals("<date>")){
				current_character += 6;
				int date_length =  0;
				while(file_contents[current_character + date_length] != '<')
					date_length++;
				String date = new String(file_contents, current_character, date_length, StandardCharsets.UTF_8);
				albums.get(current_album).date = date;
				current_character += date_length + 7;
			}
		}

		//ruft print auf allen Alben auf, welche dann selber die prints auf den Tracks aufrufen
        for (Album album : albums) {
            System.out.println(album);
        }
	}

}
