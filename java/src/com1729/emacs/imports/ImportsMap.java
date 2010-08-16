// Copyright (C) 2002 Philip John Dorrell

package com1729.emacs.imports;

import java.io.*;
import java.util.*;
/**
 * 
 * @author Philip Dorrell
 */

public class ImportsMap {

  Map baseToClassesMap = new HashMap();
  
  public String getBaseName (String className) {
    int dotPos = className.lastIndexOf (".");
    if (dotPos == -1) {
      return className;
    }
    else {
      return className.substring (dotPos+1);
    }
  }
  
  public void addClass (String className) {
    String baseName = getBaseName (className);
    System.out.println ("Class: " + baseName + ", fullname = " + className);
    Set packagesSet = (Set)baseToClassesMap.get (baseName);
    if (packagesSet == null) {
      packagesSet = new HashSet();
      baseToClassesMap.put (baseName, packagesSet);
    }
    packagesSet.add (className);
  }
  
  public void writeImports (File file, Set classes, boolean wildcarded) throws IOException {
    System.out.println ("  writing " + file + " ...");
    file.delete();
    FileWriter fw = new FileWriter (file);
    PrintWriter pw = new PrintWriter (fw);
    Iterator iter = classes.iterator();
    while (iter.hasNext()) {
      String className = (String)iter.next();
      String importSpec;
      if (wildcarded) {
        int dotPos = className.lastIndexOf (".");
        if (dotPos == -1) {
        }
        else {
          pw.println ("import " + className.substring (0, dotPos) + ".*;");
        }
      }
      else {
        pw.println ("import " + className + ";");
      }
    }
    pw.close();
    fw.close();
  }
  
  public void writeFiles (File outputDir) throws IOException {
    System.out.println ("Writing import files to " + outputDir + " ...");
    outputDir.mkdirs();
    Collection classNames = baseToClassesMap.keySet();
    Iterator iter = classNames.iterator();
    while (iter.hasNext()) {
      String baseName = (String)iter.next();
      System.out.println (" imports for " + baseName);
      File importsFile = new File (outputDir, baseName + ".import");
      Set classes = (Set)baseToClassesMap.get (baseName);
      writeImports (importsFile, classes, false);
      File wildcardsFile = new File (outputDir, baseName + ".wildcarded");
      writeImports (wildcardsFile, classes, true);
    }
  }
}
