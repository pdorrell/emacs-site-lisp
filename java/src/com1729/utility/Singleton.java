// Copyright (C) 2001 Philip John Dorrell

package com1729.utility;

/**
 * 
 * @author Philip Dorrell
 */

public abstract class Singleton {
  
  private Object singleton;

  public abstract Object createSingleton() 
      throws CreateSingletonException;
  
  public Object getSingleton() throws CreateSingletonException {
    if (singleton == null) {
      synchronized (this) {
        if (singleton == null) {
          singleton = createSingleton();
        }
      }
    }
    return singleton;
  }
}
