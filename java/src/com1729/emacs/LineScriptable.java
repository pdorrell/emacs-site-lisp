// Copyright (C) 2001 Philip John Dorrell

package com1729.emacs;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;
import java.lang.reflect.*;

public class LineScriptable {

  public void processLines (BufferedReader br) 
      throws IOException {
    String line;
    while ((line = br.readLine()) != null) {
      int numParams = Integer.parseInt (line)-1;
      String methodName = "script_" + br.readLine();
      String params[] = new String[numParams];
      for (int i=0; i<numParams; i++) {
        params[i] = br.readLine();
      }
      Class clazz = this.getClass();
      Method methods[] = clazz.getDeclaredMethods();
      Method method = null;
      for (int i=0; i<methods.length && method == null; i++) {
        if (methods[i].getName().equals (methodName)) {
          method = methods[i];
        }
      }
      try {
        if (method == null) {
          throw new RuntimeException ("No method found named \"" + methodName + "\"");
        }
        method.invoke (this, (Object[])params);
      }
      catch (Exception e) {
        e.printStackTrace();
      }
    }
    br.close();
  }
  
  public void processLines() throws IOException {
    BufferedReader br = new BufferedReader (new InputStreamReader (System.in));
    processLines (br);
  }
}
