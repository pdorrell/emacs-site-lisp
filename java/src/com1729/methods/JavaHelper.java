// Copyright (C) 2000 Philip John Dorrell

package com1729.methods;

import java.io.*;
import java.util.*;

/**
 * 
 * @author Philip Dorrell
 */

public class JavaHelper {
  
  public static void init() {
    registerCommand ("list_methods", new ListMethods());
    registerCommand ("list_instance_methods", new ListInstanceMethods());
    registerCommand ("list_static_methods", new ListStaticMethods());
    registerCommand ("list_members", new ListMembers());
    registerCommand ("list_instance_members", new ListInstanceMembers());
    registerCommand ("list_static_members", new ListStaticMembers());
    registerCommand ("exit", new Exit());
  }
  
  public static class Exit implements JavaHelperCommand {
    public void execute (String arg) {
      System.out.println ("Java Helper Process EXITED.");
      System.exit (0);
    }
  }

  private static HashMap commands = new HashMap();
  
  public static void registerCommand (String name, JavaHelperCommand command) {
    commands.put (name, command);
  }
  
  public static void processCommand (String name, String arg) {
    JavaHelperCommand command = (JavaHelperCommand)commands.get (name);
    if (command == null) {
      throw new RuntimeException ("No such command as " + name);
    }
    command.execute (arg);
  }
  
  public static void processInputLines() throws IOException {
    BufferedReader br = new BufferedReader (new InputStreamReader (System.in));
    String line;
    while ((line = br.readLine()) != null) {
      if (line.length() > 0) {
        int hashPos = line.indexOf ("#");
        if (hashPos != -1) {
          String name = line.substring (0, hashPos);
          String arg = line.substring (hashPos+1);
          processCommand (name, arg);
        }
      }
    }
  }
  
  public static void main (String args[]) throws IOException {
    init();
    if (args.length == 0) {
      processInputLines();
    }
    else if (args.length == 2) {
      processCommand (args[0], args[1]);
    }
    else {
      throw new RuntimeException ("Zero or two arguments required");
    }
  }
}
