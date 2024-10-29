package org.example;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

public class Funktional {
    public static int findNameBounds(byte[] array, int len){
        if(array[len] == '<'){
            return len-1;
        }
        else {
            len++;
            return findNameBounds(array, len);
        }
    }

    //rekursionsstarter
    public static ArrayList<String> createTokenList(byte[] xmlContent){
        ArrayList<String> tokenList = new ArrayList<>();
        return createTokenListHelper(xmlContent, tokenList, 0);
    }

    private static ArrayList<String> createTokenListHelper(byte[] xmlContent, ArrayList<String> tokenList, int current_character){
        //REKURSIONSANKER: wenn xmlContent leer ist, dann returned der einfach die tokenList
        if(current_character >= xmlContent.length){
            return tokenList;
        }
        //unnötige zeichen überspringen
        else if(xmlContent[current_character] == '\n' || xmlContent[current_character] == '\r' || xmlContent[current_character] == '\t'){
            current_character++;
            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 7, StandardCharsets.UTF_8).equals("<album>")){
            tokenList.add("album");
            current_character += 7;
            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 8, StandardCharsets.UTF_8).equals("</album>")){
            tokenList.add("/album");
            current_character += 8;
            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 7, StandardCharsets.UTF_8).equals("<title>")){
            tokenList.add("title");
            current_character += 7;

            int name_length = findNameBounds(xmlContent, current_character);
            String name = new String(xmlContent, current_character, name_length - current_character + 1, StandardCharsets.UTF_8);
            tokenList.add(name);
            current_character += name_length - current_character + 1;
            tokenList.add("/title");
            current_character += 8;

            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 8, StandardCharsets.UTF_8).equals("<artist>")){
            tokenList.add("artist");
            current_character += 8;

            int name_length = findNameBounds(xmlContent, current_character);
            String name = new String(xmlContent, current_character, name_length - current_character + 1, StandardCharsets.UTF_8);
            tokenList.add(name);
            current_character += name_length - current_character + 1;
            tokenList.add("/artist");
            current_character += 9;

            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 8, StandardCharsets.UTF_8).equals("<rating>")){
            tokenList.add("rating");
            current_character += 8;

            int name_length = findNameBounds(xmlContent, current_character);
            String name = new String(xmlContent, current_character, name_length - current_character + 1, StandardCharsets.UTF_8);
            tokenList.add(name);
            current_character += name_length - current_character + 1;
            tokenList.add("/rating");
            current_character += 9;

            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 7, StandardCharsets.UTF_8).equals("<track>")){
            tokenList.add("track");
            current_character += 8;
            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 8, StandardCharsets.UTF_8).equals("</track>")){
            tokenList.add("/track");
            current_character += 9;
            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 9, StandardCharsets.UTF_8).equals("<feature>")){
            tokenList.add("feature");
            current_character += 9;

            int name_length = findNameBounds(xmlContent, current_character);
            String name = new String(xmlContent, current_character, name_length - current_character + 1, StandardCharsets.UTF_8);
            tokenList.add(name);
            current_character += name_length - current_character + 1;
            tokenList.add("/feature");
            current_character += 10;

            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 8, StandardCharsets.UTF_8).equals("<length>")){
            tokenList.add("length");
            current_character += 8;

            int name_length = findNameBounds(xmlContent, current_character);
            String name = new String(xmlContent, current_character, name_length - current_character + 1, StandardCharsets.UTF_8);
            tokenList.add(name);
            current_character += name_length - current_character + 1;
            tokenList.add("/length");
            current_character += 9;

            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 9, StandardCharsets.UTF_8).equals("<writing>")){
            tokenList.add("writing");
            current_character += 9;

            int name_length = findNameBounds(xmlContent, current_character);
            String name = new String(xmlContent, current_character, name_length - current_character + 1, StandardCharsets.UTF_8);
            tokenList.add(name);
            current_character += name_length - current_character + 1;

            tokenList.add("/writing");
            current_character += 10;

            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        else if(new String(xmlContent, current_character, 6, StandardCharsets.UTF_8).equals("<date>")){
            tokenList.add("date");
            current_character += 6;

            int name_length = findNameBounds(xmlContent, current_character);
            String name = new String(xmlContent, current_character, name_length - current_character + 1, StandardCharsets.UTF_8);
            tokenList.add(name);
            current_character += name_length - current_character + 1;
            tokenList.add("/date");
            current_character += 7;

            return createTokenListHelper(xmlContent, tokenList, current_character);
        }
        return tokenList;
    }

    public static ArrayList<Album> parseFile(ArrayList<String> tokens) {
        return parseFileHelper(tokens, new ArrayList<>(), 0);
    }

    private static ArrayList<Album> parseFileHelper(ArrayList<String> tokens, ArrayList<Album> albums, int index) {
        //REKURSIONSANKER: index ist größer als die anzahl tokens, mman ist also alles durchgegangen
        if (index >= tokens.size()) {
            return albums;
        }
        //album gelesen, ruft parseAlbum auf den weiteren elementen auf. der neue index wird von der Methode festgelegt
        if (tokens.get(index).equals("album")) {
            Object[] result = parseAlbum(tokens, index + 1);
            albums.add((Album) result[0]);
            index = (int) result[1];
        }
        return parseFileHelper(tokens, albums, index);
    }

    private static Object[] parseAlbum(ArrayList<String> tokens, int index) {
        Album album = new Album();
        return parseAlbumHelper(tokens, album, index);
    }

    private static Object[] parseAlbumHelper(ArrayList<String> tokens, Album album, int index) {
        //REKURSIONSANKER: schließendes tag gelesen
        if (tokens.get(index).equals("/album")) {
            return new Object[]{album, index + 1};
        }
        //Liest hier die einzelnen Attribute aus, es wird +3 gerechnet damit schließends tag auch übersprungen wird
        switch (tokens.get(index)) {
            case "title":
                album.title = tokens.get(index + 1);
                index += 3;
                break;
            case "artist":
                album.artist = tokens.get(index + 1);
                index += 3;
                break;
            case "date":
                album.date = tokens.get(index + 1);
                index += 3;
                break;
            //Fall track: ruft parseTrack auf
            case "track":
                Object[] result = parseTrack(tokens, index + 1);
                album.tracks.add((Track) result[0]);
                index = (int) result[1];
                break;
            default:
                index++;
                break;
        }
        //REKURSIVER AUFRUF
        return parseAlbumHelper(tokens, album, index);
    }

    private static Object[] parseTrack(ArrayList<String> tokens, int index) {
        Track track = new Track();
        return parseTrackHelper(tokens, track, index);
    }

    //Methode von der Funktionsweise genau wie parseAlbum
    private static Object[] parseTrackHelper(ArrayList<String> tokens, Track track, int index) {
        if (tokens.get(index).equals("/track")) {
            return new Object[]{track, index + 1};
        }
        switch (tokens.get(index)) {
            case "title":
                track.title = tokens.get(index + 1);
                index += 3;
                break;
            case "length":
                track.length = tokens.get(index + 1);
                index += 3;
                break;
            case "rating":
                track.rating = Integer.parseInt(tokens.get(index + 1));
                index += 3;
                break;
            case "feature":
                track.features.add(tokens.get(index + 1));
                index += 3;
                break;
            case "writing":
                track.writers.add(tokens.get(index + 1));
                index += 3;
                break;
            default:
                index++;
                break;
        }
        return parseTrackHelper(tokens, track, index);
    }

    public static void main(String[] args) throws IOException {
        //liest aus der alben.xml die Daten als byte Array ein
        String file_path = ("\\src\\main\\resources\\alben.xml");
        String currentDir = Paths.get("").toAbsolutePath().toString();
        Path fullPath = Paths.get(currentDir, file_path);
        byte[] file_contents = Files.readAllBytes(fullPath);
        ArrayList<String> tokens = createTokenList(file_contents);
        ArrayList<Album> albums = parseFile(tokens);
        for (Album album : albums) {
            System.out.println(album);
        }
    }
}
