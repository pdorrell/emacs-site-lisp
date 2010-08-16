// Copyright (C) 2001 Philip John Dorrell

package com1729.emacs.browser;

/**
 * 
 * @author Philip Dorrell
 */

import java.io.*;
import java.net.*;
import com1729.emacs.*;

public class Browser extends LineScriptable {
  
  public void script_browseUrl (String urlString) 
      throws IOException, MalformedURLException {
    URL url = new URL (urlString);
    browseUrl (url);
  }
  
  public void script_browseRelativeUrl (String baseUrl, String relativeUrl) 
      throws IOException, MalformedURLException {
    URL url = new URL (new URL (baseUrl), relativeUrl);
    browseUrl (url);
  }

  public void browseUrl (URL url) throws IOException {
    try {
      System.out.println ("URL = " + url);
      URLConnection connection = url.openConnection();
      String key;
      int n = 1;
      while ((key = connection.getHeaderFieldKey (n)) != null) {
        String value = connection.getHeaderField (n);
        System.out.println (key + ": " + value);
        n++;
      }
      System.out.println();
      InputStream inputStream = connection.getInputStream();
      Reader reader = new InputStreamReader (inputStream);
      char buffer[] = new char[1024];
      int numRead;
      while ((numRead = reader.read (buffer)) >= 0) {
        System.out.print (new String (buffer, 0, numRead));
      }
      System.out.println();
      reader.close();
      inputStream.close();
    }
    catch (MalformedURLException e) {
      System.out.println ("Malformed URL: " + e.getMessage());
    }
    catch (IOException e) {
      System.out.println ("IO Error: " + e.getMessage());
    }
  }
  
  public static void main (String args[]) throws IOException {
    new Browser().processLines();
  }

}
