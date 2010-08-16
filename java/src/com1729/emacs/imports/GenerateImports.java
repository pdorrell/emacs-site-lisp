// Copyright (C) 2002 Philip John Dorrell

package com1729.emacs.imports;

import java.io.*;
import java.util.*;
/**
 * 
 * @author Philip Dorrell
 */

public class GenerateImports {
  
  ImportsMap importsMap;
  
  public GenerateImports() {
  }
  
  public void generateImports (File scriptFile, File outputDir) throws IOException {
    importsMap = new ImportsMap();
    FileReader reader = new FileReader (scriptFile);
    BufferedReader br = new BufferedReader (reader);
    String line;
    while ((line = br.readLine()) != null) {
      line = line.trim();
      if (line.length() > 0) {
        processLine (line);
      }
    }
    importsMap.writeFiles (outputDir);
  }
  
  public void processLine (String line) throws IOException {
    StringTokenizer tokens = new StringTokenizer (line, " \t");
    String fileName = tokens.nextToken();
    String filterString = null;
    if (tokens.hasMoreTokens()) {
      filterString = tokens.nextToken();
    }
    process (new File (fileName), filterString);
  }
  
  public void process (File file, String filterString) throws IOException {
    if (file.isDirectory()) {
      System.out.println ("Processing directory " + file + " with filter " + filterString);
      new SourceDirectoryImports (file, filterString).generateImports (importsMap);
    }
    else {
      System.out.println ("File " + file + " is not a directory");
    }
  }
  
  public static void main (String args[]) throws IOException {
    if (args.length < 2) {
      main (new String[]{"c:/dev/work/java/src/com1729/emacs/imports/imports.script", "c:/temp/imports"});
      // System.out.println ("Useage: GenerateImports <script-file> <output-dir>");
    }
    else {
      new GenerateImports().generateImports (new File (args[0]), new File (args[1]));
    }
  }

}
