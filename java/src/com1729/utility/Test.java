// Copyright (C) 2002 Philip John Dorrell

package com1729.utility;

import java.util.*;

/**
 * 
 * @author Philip Dorrell
 */

public class Test {

  /** @param x param
   * @return y
   * @exception */
  public static void main (String args[]) {
    String name = new Test().getClass().getName();
    System.out.println ("Test");
    List list = Arrays.asList (new String[]{"jim", "fred"});
    Iterator iter = list.iterator();
    while (iter.hasNext()) {
      Object object=(Object)iter.next();
      System.out.println ("item = " + object);
    }
  }

}
