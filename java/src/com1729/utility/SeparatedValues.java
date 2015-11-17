// Copyright (C) 2000 Philip John Dorrell

package com1729.utility;

/**
 * 
 * @author Philip Dorrell
 */

import java.util.*;

public class SeparatedValues {
  
  private String values[];
  
  public String[] getValues() {
    return values;
  }
  
  public SeparatedValues (String valuesString, String separator) {
    int pos = 0;
    List list = new ArrayList();
    int len = valuesString.length();
    while (pos < len+1) {
      int separatorPos = valuesString.indexOf (separator, pos);
      if (separatorPos == -1) {
        separatorPos = len;
      }
      String separatedValue = valuesString.substring (pos, separatorPos);
      list.add (separatedValue);
      pos = separatorPos+separator.length();
    }
    values = (String[])list.toArray (new String[0]);
    if (values.length == 1 && values[0].length() == 0)
      values = new String[0];
  }
  
  public static void main (String args[]) {
    String values[] = new SeparatedValues (args[0], args[1]).getValues();
    for (int i=0; i<values.length; i++) {
      System.out.println ("value #" + values[i] + "#");
    }
  }
  
  public static String valuesAsString (String values[], String separator) {
    StringBuffer out = new StringBuffer();
    for (int i=0; i<values.length; i++) {
      if (i>0) {
        out.append (separator);
      }
      out.append (values[i]);
    }
    return out.toString();
  }
  

};
