// Copyright (C) 2000 Philip John Dorrell

package com1729.utility;

/**
 * 
 * @author Philip Dorrell
 */

import java.util.*;

public abstract class SingletonsMap extends HashMap {

  public abstract Object createSingleton (Object key) 
      throws CreateSingletonException;
  
  public Object getSingleton (Object key) 
      throws CreateSingletonException {
    Object singleton = get (key);
    if (singleton == null) {
      synchronized (this) {
        singleton = get (key);
        if (singleton == null) {
          singleton = createSingleton (key);
          put (key, singleton);
        }
      }
    }
    return singleton;
  }
}
