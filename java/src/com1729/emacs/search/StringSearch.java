// Copyright (C) 2001 Philip John Dorrell

package com1729.emacs.search;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;
import com1729.emacs.*;
import com1729.utility.*;

public class StringSearch extends LineScriptable {

  public void script_search (String dirsArg, String fileExtensionsArg, String string) 
      throws IOException {
    System.out.println ("  SEARCH for string \"" + string + "\" in directories " +
            dirsArg + " in files with extensions " + fileExtensionsArg);
    String fileExtensions[] = new SeparatedValues (fileExtensionsArg, ';').getValues();
    FilenameFilter filenameFilter = new FileExtensionsFilter (fileExtensions);
    StringFileFilterSearcher search = new StringFileFilterSearcher (string, filenameFilter);
    String dirs[] = new SeparatedValues (dirsArg, ';').getValues();
    for (int i=0; i<dirs.length; i++) {
      search.searchInDir (new File (dirs[i]));
    }
    System.out.println ("  DONE");
  }
  
  public static void main (String args[]) throws IOException {
    new StringSearch().processLines();
  }

}
