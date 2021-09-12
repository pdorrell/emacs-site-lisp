// Copyright (C) 2001 Philip John Dorrell

package thinkinghard.emacs.sql;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;
import java.util.*;

public class TableFormatter {

  int numColumns;
  int widths[];
  
  String headings[];
  ArrayList rows = new ArrayList();
  
  public TableFormatter (int numColumns) {
    this.numColumns = numColumns;
    widths = new int[numColumns];
    for (int i=0; i<numColumns; i++) {
      widths[i] = 0;
    }
  }
  
  public void adjustWidths (String values[]) {
    for (int i=0; i<numColumns; i++) {
      int length = values[i].length();
      if (length> widths[i]) {
        widths[i] = length;
      }
    }
  }
  
  public void setHeadings (String headings[]) {
    adjustWidths (headings);
    this.headings = headings;
  }
  
  public void addRow (String row[]) {
    adjustWidths (row);
    rows.add (row);
  }
  
  private static String divider = "  ";
  
  public static void setDivider (String aDivider) {
    divider = aDivider;
  }
  
  public static String getPadding (int n) {
    StringBuffer buffer = new StringBuffer();
    for (int i=0; i<n; i++) {
      buffer.append (' ');
    }
    return buffer.toString();
  }
  
  public void printRow (String row[]) {
    for (int i=0; i<numColumns; i++) {
      if (i > 0) {
        System.out.print (divider);
      }
      String value = row[i];
      System.out.print (value);
      int padding = Math.max (0, widths[i] - value.length());
      if (padding > 0) {
        System.out.print (getPadding (padding));
      }
    }
    System.out.println();
  }
                                           
  public void writePage() {
    printRow (headings);
    System.out.println();
    Iterator rowIterator = rows.iterator();
    while (rowIterator.hasNext()) {
      String row[] = (String[])rowIterator.next();
      printRow (row);
    }
    System.out.println();
    rows.clear();
  }

}
