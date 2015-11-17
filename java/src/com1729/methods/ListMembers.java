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

public class ListMembers implements JavaHelperCommand {
  
  public class MemberLine implements Comparable {
    private String line;
    
    public MemberLine (String line) {
      this.line = line;
    }
    
    public int compareTo (Object o) {
      MemberLine otherLine = (MemberLine)o;
      return line.compareToIgnoreCase (otherLine.line);
    }
    
    public String getLine() {
      return line;
    }
    
    public boolean equals (Object o) {
      return line.equals (((MemberLine)o).line);
    }
    
    public int hashCode() {
      return line.hashCode ();
    }
    
    public String toString () {
      return line;
    }
  }
  
  public MemberLine getMemberLine (Method method) {
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
    return new MemberLine (new String (lineBuffer));
  }
  
  public MemberLine getMemberLine (Field field) {
    Class type = field.getType();
    StringBuffer lineBuffer = new StringBuffer();
    lineBuffer.append (field.getName());
    lineBuffer.append ("    ! ");
    lineBuffer.append (className (type));
    return new MemberLine (new String (lineBuffer));
  }    
  
  public boolean isSelectedType (Method method) {
    return true;
  }
  
  public boolean isSelectedType (Field field) {
    return true;
  }
  
  public void enumerateFields (Class clazz, String memberStart, Set memberLinesSet) {
    Field[] fields = clazz.getFields();
    for (int i=0; i<fields.length; i++) {
      Field field = fields[i];
      if (isSelectedType (field)) {
        String fieldName = field.getName();
        if (fieldName.startsWith (memberStart)) {
          MemberLine memberLine = getMemberLine (field);
          memberLinesSet.add (memberLine);
        }
      }
    }
  }
  
  public void enumerateMethods (Class clazz, String memberStart, Set memberLinesSet) {
    Method[] methods = clazz.getMethods();
    for (int i=0; i<methods.length; i++) {
      Method method = methods[i];
      if (isSelectedType (method)) {
        String methodName = method.getName();
        if (methodName.startsWith (memberStart)) {
          MemberLine memberLine = getMemberLine (method);
          memberLinesSet.add (memberLine);
        }
      }
    }
  }
  
  public void listMembers (Class clazz, String memberStart) {
    boolean any = false;
    SortedSet memberLinesSet = new TreeSet();
    enumerateFields (clazz, memberStart, memberLinesSet);
    Iterator memberLines = memberLinesSet.iterator();
    while (memberLines.hasNext()) {
      any = true;
      System.out.println (memberLines.next());
    }
    memberLinesSet = new TreeSet();
    enumerateMethods (clazz, memberStart, memberLinesSet);
    memberLines = memberLinesSet.iterator();
    while (memberLines.hasNext()) {
      any = true;
      System.out.println (memberLines.next());
    }
    if (!any) {
      System.out.println ("   ! (none)");
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
  
  public void listClassMembers (String memberSpec) {
    String args[] = new SeparatedValues (memberSpec, ":").getValues();
    if (args.length != 3 && args.length != 2) {
      throw new RuntimeException ("Expect 2 or 3 colon separated arguments: " + memberSpec);
    }
    String className = args[0];
    String memberStart = args[1];
    System.out.println (" FIELDS & METHODS " + className + "." + memberStart + "*");
    Class clazz = null;
    if (args.length == 2) {
      try {
        clazz = findClass (className);
        listMembers (clazz, memberStart);
      }
      catch (ClassNotFoundException e) {
        System.out.println ("CLASS " + className + " NOT FOUND");
      }
    }
    else {
      String packageNames[] = new SeparatedValues (args[2], ",").getValues();
      boolean classFound = false;
      for (int i=0; i<packageNames.length && !classFound; i++) {
        String fullClassName = packageNames[i] + "." + className;
        try {
          clazz = findClass (fullClassName);
          classFound = true;
          listMembers (clazz, memberStart);
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
    listClassMembers (arg);
  }

};
