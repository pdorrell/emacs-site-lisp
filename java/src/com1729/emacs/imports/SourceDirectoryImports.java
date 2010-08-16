// Copyright (C) 2002 Philip John Dorrell

package com1729.emacs.imports;

import java.io.*;
/**
 * 
 * @author Philip Dorrell
 */

public class SourceDirectoryImports {
  
  File dir;
  String dirName;
  
  File subDir;
  String subDirName;
  
  public SourceDirectoryImports (File dir, String filterString) throws IOException {
    this.dir = dir.getCanonicalFile();
    dirName = dir.getCanonicalPath();
    if (filterString == null) {
      subDir = null;
      subDirName = null;
    }
    else {
      subDir = new File (filterString).getCanonicalFile();
      subDirName = subDir.getCanonicalPath();
      if (!subDirName.startsWith (dirName)) {
        throw new RuntimeException ("Filter sub-dir " + subDirName + " is not in " + dirName);
      }
    }
    System.out.println ("dir = " + dir + ", subdir = " + subDir);
  }
  
  public void generateImports (ImportsMap importsMap) {
    processDir (dir, "", importsMap);
  }
  
  public static String prefixed (String prefix, String name) {
    if (prefix.length() == 0) {
      return name;
    }
    else {
      return prefix + "." + name;
    }
  }
  
  public boolean filterAllows (File file) {
    return subDirName == null || file.toString().startsWith (subDirName);
  }
  
  public boolean filterMightAllow (File file) {
    String fileName = file.toString();
    return subDirName == null || fileName.startsWith (subDirName) 
            || subDirName.startsWith (fileName);
  }
  
  public void processDir (File dir, String packagePrefix, ImportsMap importsMap) {
    System.out.println ("Processing directory " + dir + " with prefix \"" + packagePrefix + "\" ...");
    File[] files = dir.listFiles();
    for (int i=0; i<files.length; i++) {
      File file = files[i];
      if (file.isDirectory()) {
        if (!file.getName().equals ("CVS") && filterMightAllow (file)) {
          processDir (file, prefixed (packagePrefix, file.getName()), importsMap);
        }
      }
      else {
        if (filterAllows (file)) {
          String name = file.getName();
          if (name.endsWith (".java")) {
            String className = name.substring (0, name.length() - 5);
            importsMap.addClass (prefixed (packagePrefix, className));
          }
        }
      }
    }
  }
}
