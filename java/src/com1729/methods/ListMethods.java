/*Copyright (C) 2000 Philip John Dorrell*/

package com1729.methods;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;
import java.lang.reflect.*;
import java.util.*;

import com1729.utility.*;

public class ListMethods implements JavaHelperCommand {
  
  public class MethodLine implements Comparable {
    private String line;
    
    public MethodLine (String line) {
      this.line = line;
    }
    
    public int compareTo (Object o) {
      MethodLine otherLine = (MethodLine)o;
      return line.compareToIgnoreCase (otherLine.line);
    }
    
    public String getLine() {
      return line;
    }
    
    public boolean equals (Object o) {
      return line.equals (((MethodLine)o).line);
    }
    
    public int hashCode() {
      return line.hashCode ();
    }
    
    public String toString () {
      return line;
    }
  }
  
  public MethodLine getMethodLine (Method method) {
    Class returnClass = method.getReturnType();
    Class paramTypes[] = method.getParameterTypes();
    StringBuffer lineBuffer = new StringBuffer();
    lineBuffer.append (method.getName());
    for (int i=0; i<paramTypes.length; i++) {
      if (i == 0) {
        lineBuffer.append (" (");
      }
      else {
        lineBuffer.append(", ");
      }
      lineBuffer.append ("#( ");
      lineBuffer.append (className (paramTypes[i]));
      lineBuffer.append (" )# ");
    }
    if (paramTypes.length == 0) {
      lineBuffer.append ("(");
    }
    lineBuffer.append (")    ! ");
    
    lineBuffer.append (className (returnClass));
    return new MethodLine (new String (lineBuffer));
  }
  
  public boolean isSelectedType (Method method) {
    return true;
  }
  
  public void listMethods (Class clazz, String methodStart) {
    Method[] methods = clazz.getMethods();
    SortedSet methodLinesSet = new TreeSet();
    for (int i=0; i<methods.length; i++) {
      Method method = methods[i];
      if (isSelectedType (method)) {
        String methodName = method.getName();
        if (methodName.startsWith (methodStart)) {
          MethodLine methodLine = getMethodLine (method);
          methodLinesSet.add (methodLine);
        }
      }
    }
    Iterator methodLines = methodLinesSet.iterator();
    while (methodLines.hasNext()) {
      System.out.println (methodLines.next());
    }
  }
  
  public static String className (Class clazz) {
    if (clazz.isArray()) {
      return className (clazz.getComponentType()) + "[]";
    }
    else {
      String name = clazz.getName();
      if (name.startsWith ("java.lang.") && name.lastIndexOf ('.') == 9) {
        name = name.substring (10);
      }
      return name;
    }
  }
  
  private static ClassLoader currentLoader = ListMethods.class.getClassLoader();
  
  public Class findClass (String className) throws ClassNotFoundException {
    return Class.forName (className, false, currentLoader);
  }
  
  public void listClassMethods (String methodSpec) {
    String args[] = new SeparatedValues (methodSpec, ':').getValues();
    if (args.length != 3 && args.length != 2) {
      throw new RuntimeException ("Expect 2 or 3 colon separated arguments: " + methodSpec);
    }
    String className = args[0];
    String methodStart = args[1];
    System.out.println (" METHODS " + className + "." + methodStart + "*");
    Class clazz = null;
    if (args.length == 2) {
      try {
        clazz = findClass (className);
        listMethods (clazz, methodStart);
      }
      catch (ClassNotFoundException e) {
        System.out.println ("CLASS " + className + " NOT FOUND");
      }
    }
    else {
      String packageNames[] = new SeparatedValues (args[2], ',').getValues();
      boolean classFound = false;
      for (int i=0; i<packageNames.length && !classFound; i++) {
        String fullClassName = packageNames[i] + "." + className;
        try {
          clazz = findClass (fullClassName);
          classFound = true;
          listMethods (clazz, methodStart);
        }
        catch (ClassNotFoundException e) {
        }
      }
      if (!classFound) {
        System.out.println ("CLASS " + className + " NOT FOUND in any package");
      }
    }
  }

  public void execute (String arg) {
    listClassMethods (arg);
  }

};
