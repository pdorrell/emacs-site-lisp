// Copyright (C) 2000 Philip John Dorrell

package com1729.utility;

/**
 * 
 * @author Philip Dorrell
 */
import java.io.*;
import java.util.*;

public class FileExtensionsFilter implements FilenameFilter {

  HashSet extensionsSet = new HashSet();
  
  public FileExtensionsFilter (String extensions[]) {
    for (int i=0; i<extensions.length; i++) {
      extensionsSet.add (extensions[i]);
    }
  }
  
  public FileExtensionsFilter (String extension) {
    this (new String[]{extension});
  }
  
  public boolean accept(File dir, String name) {
    int dotPos = name.lastIndexOf (".");
    if (dotPos == -1) {
      return false;
    }
    else {
      String extension = name.substring (dotPos);
      return extensionsSet.contains (extension);
    }
  }
  
};
