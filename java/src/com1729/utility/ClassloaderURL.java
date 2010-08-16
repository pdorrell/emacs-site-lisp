// Copyright (C) 2002 Philip John Dorrell

package com1729.utility;

import java.net.*;
/**
 * 
 * @author Philip Dorrell
 */

public class ClassloaderURL {

  public static void main (String args[]) {
    Class thisClass = ClassloaderURL.class;
    URL url = thisClass.getClassLoader().getSystemResource (thisClass.getName() + ".class");
    System.out.println ("class URL = " + url);
    System.out.println ("user dir = " + System.getProperty ("user.dir"));
  }

}
