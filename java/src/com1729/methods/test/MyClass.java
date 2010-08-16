// Copyright (C) 2001 Philip John Dorrell

package com1729.methods.test;

/**
 * 
 * @author Philip Dorrell
 */

public class MyClass {

  static { 
    System.out.println ("MyClass initialised");
    int x = 3/0;
  }
  
  public static void doSomething (int x, String y) {
  }
  
  public void doSomethingOnInstance (int x, String y) {
  }
}
