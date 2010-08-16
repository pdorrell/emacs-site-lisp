// Copyright (C) 2001 Philip John Dorrell

package com1729.emacs.sql;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;
import java.sql.*;
import java.util.*;
import com1729.emacs.*;

public class Sql extends LineScriptable {

  Connection connection = null;
  
  int pageLength = 50;
  int maxRows = 2000;
  boolean isJdbcWorkaround = false;
  
  boolean showTypes = false;
  boolean showQuotes = false;
  
  public void script_setPageLength (String pageLengthString) {
    if (pageLength < 1) {
      throw new RuntimeException ("Page length must be >= 1");
    }
    this.pageLength = Integer.parseInt (pageLengthString);
    System.out.println ("Page length set to " + pageLength);
    System.out.println (THICK_LINE);
  }
  
  public void script_setDivider (String divider) {
    TableFormatter.setDivider (divider);
    System.out.println ("Set divider to \"" + divider + "\"");
    System.out.println (THICK_LINE);
  }
  
  public void script_setShowTypes (String showTypesString) {
    showTypes = Boolean.valueOf (showTypesString).booleanValue();
    System.out.println ("Types are " + (showTypes ? "" : "not ") + "showing");
    System.out.println (THICK_LINE);
  }
  
  public void script_setShowQuotes (String showQuotesString) {
    showQuotes = Boolean.valueOf (showQuotesString).booleanValue();
    System.out.println ("Quotes are " + (showQuotes ? "" : "not ") + "showing");
    System.out.println (THICK_LINE);
  }    
  
  public void script_setMaxRows (String maxRowsString) {
    this.maxRows = Integer.parseInt (maxRowsString);
    System.out.println ("Max rows set to " + maxRows);
    System.out.println (THICK_LINE);
  }
  
  public void script_setWorkaround() {
    isJdbcWorkaround = true;
    System.out.println ("Workaround");
    System.out.println (THICK_LINE);
  }
  
  public void script_disconnect() {
    try {
      if (connection != null) {
        connection.close();
        connection = null;
        System.out.println ("Disconnected");
      }
      else {
        System.out.println ("Not currently connected");
      }
    }
    catch (Exception e) {
      System.out.println ("Error instantiating driver class: " + e.getMessage());
    }
    System.out.println (THICK_LINE);
  }
  
  public void script_setConnection (String url, String driverClassName) {
    try {
      Class.forName(driverClassName).newInstance();
    }
    catch (Exception e) {
      System.out.println ("Error instantiating driver class: " + e.getMessage());
    }
    try {
      if (connection != null) {
        connection.close();
      }
    }
    catch (SQLException e) {
      System.out.println ("Error closing previous connection");
      showError (e);
    }
    try {
      connection = DriverManager.getConnection (url);
      System.out.println ("Connection set to URL: " + url + ", driver = " + driverClassName);
    }
    catch (SQLException e) {
      System.out.println ("Error setting connection");
      showError (e);
    }
    System.out.println (THICK_LINE);
  }

  public void script_setConnectionWithUserPassword (String url, String driverClassName, 
          String user, String password) 
      throws ClassNotFoundException, InstantiationException, IllegalAccessException, SQLException {
    System.out.println ("Connection set to URL: " + url + ", driver = " + driverClassName + 
                        ", user = " + user + ", password = " + password);
    Class.forName(driverClassName).newInstance();
    connection = DriverManager.getConnection (url, user, password);
    System.out.println (THICK_LINE);
  }
  
  public static final String THICK_LINE = 
    "================================================================================";
  
  public static final String THIN_LINE = 
    "------------------------------------------------------------";
  
  public boolean showingUpdates = false;
  
  public void finishShowingAnyUpdates() {
    if (showingUpdates) {
      System.out.println();
      showingUpdates = false;
    }
  }
  
  public void showUpdateCount (int updateCount) {
    if (!showingUpdates) {
      System.out.print ("Updated rows:");
      showingUpdates = true;
    }
    System.out.print (" " + updateCount);
  }
  
  public String displayedValue (Object value) {
    String displayString = value.toString();
    if (showQuotes && value instanceof String) {
      displayString = "'" + displayString + "'";
    }
    if (showTypes) {
      displayString = value.getClass().getName() + ":" + displayString;
    }
    return displayString;
  }
  
  public void showResultSet (ResultSet resultSet) throws SQLException {
    ResultSetMetaData metaData = resultSet.getMetaData();
    int numColumns = metaData.getColumnCount();
    TableFormatter formatter = new TableFormatter (numColumns);
    String columnNames[] = new String[numColumns];
    for (int i=0; i<numColumns; i++) {
      columnNames[i] = metaData.getColumnName (i+1);
    }
    formatter.setHeadings (columnNames);
    
    List rows = new ArrayList();
    int pageRowCount = 0;
    int totalRowCount = 0;
    int pageCount = 0;
    while (resultSet.next() && totalRowCount < maxRows) {
      String row[] = new String[numColumns];
      for (int i=0; i<numColumns; i++) {
        Object value = resultSet.getObject (i+1);
        row[i] = value == null ? "null" : displayedValue (value);
      }
      formatter.addRow (row);
      pageRowCount++;
      totalRowCount++;
      if (pageRowCount == pageLength) {
        formatter.writePage();
        pageRowCount = 0;
        pageCount++;
      }
    }
    if (pageRowCount > 0 || pageCount == 0) {
      formatter.writePage();
    }      
    resultSet.close();
  }
  
  public static void showError (SQLException e) {
    SQLException error = e;
    while (error != null) {
      System.out.println ("SQL ERROR: (" + error.getErrorCode() + ":" + 
              error.getSQLState() + ") " + error.getMessage());
      error = error.getNextException();
    }
  }
  
  public void script_executeQuery (String query) {
    if (connection == null) {
      System.out.println ("ERROR: Not currently connected");
    }
    else {
      System.out.println ("QUERY: " + query);
      try {
        Statement statement = connection.createStatement();
        boolean isResultSet = statement.execute (query);
        int updateCount = 0;
        if (!isResultSet) {
          updateCount = statement.getUpdateCount();
        }
        while (isResultSet || updateCount != -1) {
          if (isResultSet) {
            finishShowingAnyUpdates();
            System.out.println (THIN_LINE);
            showResultSet (statement.getResultSet());
          }
          else {
            showUpdateCount (updateCount);
          }
          isResultSet = statement.getMoreResults();
          if (!isResultSet) {
            updateCount = statement.getUpdateCount();
            if (isJdbcWorkaround) {
              updateCount = -1;
            }
          }
        }
        finishShowingAnyUpdates();
      }
      catch (SQLException e) {
        finishShowingAnyUpdates();
        showError (e);
      }
    }
    System.out.println (THICK_LINE);
  }
  
  public static void test() throws Exception {
    Sql sql = new Sql();
    sql.script_setConnection ("jdbc:mysql://localhost/womcat", "org.gjt.mm.mysql.Driver");
    sql.script_executeQuery ("DROP table if exists dummy");
    sql.script_executeQuery ("select * from my_subjects");
  }
  
  public static void main (String args[]) throws Exception {
    System.out.println (THICK_LINE);
    new Sql().processLines();
  }
  
}
