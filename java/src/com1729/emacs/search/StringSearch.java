// Copyright (C) 2001 Philip John Dorrell

package com1729.emacs.search;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;
import java.util.*;
import com1729.emacs.*;
import com1729.utility.*;

public class StringSearch extends LineScriptable {

  public void script_search (String dirsArg, String fileExtensionsArg, String string) 
      throws IOException {
    System.out.println ("  SEARCH for string \"" + string + "\" in directories " +
            dirsArg + " in files with extensions " + fileExtensionsArg);
    String fileExtensions[] = new SeparatedValues (fileExtensionsArg, ";").getValues();
    FilenameFilter filenameFilter = new FileExtensionsFilter (fileExtensions);
    StringFileFilterSearcher search = new StringFileFilterSearcher (string, filenameFilter);
    String dirs[] = new SeparatedValues (dirsArg, ";").getValues();
    for (int i=0; i<dirs.length; i++) {
      String searchDirSpec = dirs[i];
      String searchDirSpecParts[] = new SeparatedValues(searchDirSpec, "::").getValues();
      String searchDir = searchDirSpecParts[0];
      
      String[] excludeDirs = new String[searchDirSpecParts.length-1];
      for (int j=0; j<excludeDirs.length; j++) {
        excludeDirs[j] = searchDir + searchDirSpecParts[j+1];
      }
      search.searchInDir (new File (searchDir), excludeDirs);
    }
    System.out.println ("  DONE");
  }
  
  public static void main (String args[]) throws IOException {
    new StringSearch().processLines();
  }

}
