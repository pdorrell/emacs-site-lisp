// Copyright (C) 2001 Philip John Dorrell

package com1729.emacs.search;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;

public class StringFileFilterSearcher {

  String string;
  FilenameFilter filenameFilter;
  
  public StringFileFilterSearcher (String string, FilenameFilter filenameFilter) {
    this.string = string;
    this.filenameFilter = filenameFilter;
  }
  
  public void searchInDir (File dir, String[] excludeDirs) throws IOException {
    File files[] = dir.listFiles();
    if (files != null) {
      for (int i=0; i<files.length; i++) {
        File file = files[i];
        if (file.isDirectory() && !file.getName().equals (".git")) {
          String dirName = file.toString();
          boolean excluded = false;
          for (int j=0; j<excludeDirs.length; j++) {
            if (dirName.startsWith(excludeDirs[j])) {
              excluded = true;
            }
          }
          if (!excluded) {
            searchInDir (file, excludeDirs);
          }
        }
        else if (filenameFilter.accept (file.getParentFile(), file.getName())) {
          searchInFile (file);
        }
      }
    }
  }
  
  public boolean fileHasString (File file) throws IOException {
    FileReader fr = new FileReader (file);
    int fileLength = (int)file.length();
    char data[] = new char[fileLength];
    int numCharsRead = fr.read (data);
    fr.close();
    String fileString = new String (data, 0, numCharsRead);
    return fileString.indexOf (string) != -1;
  }
  
  public void searchInFile (File file) throws IOException {
    String fileName = file.getName();
    if (fileName.startsWith(string) || fileName.startsWith(string + ".")) {
        System.out.println (file + ":1:<file name>");
    }
    if (fileHasString (file)) {
      FileReader fr = new FileReader (file);
      BufferedReader br = new BufferedReader (fr);
      String line;
      int count = 0;
      while ((line = br.readLine()) != null) {
        count++;
        if (line.indexOf (string) != -1) {
          System.out.println (file + ":" + count + ":" + line);
        }
      }
      br.close();
      fr.close();
    }
  }
  
}
