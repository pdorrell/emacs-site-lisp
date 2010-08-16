// Copyright (C) 2002 Philip John Dorrell

package com1729.utility;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;

public class DirectoriesFilter implements FilenameFilter {

  public boolean accept(File dir, String name) {
    return new File (dir, name).isDirectory();
  }
}
