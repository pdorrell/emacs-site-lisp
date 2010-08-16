// Copyright (C) 2000 Philip John Dorrell

package com1729.methods;

import java.lang.reflect.*;

/**
 * 
 * @author Philip Dorrell
 */

public class ListInstanceMethods extends ListMethods {

  public boolean isSelectedType (Method method) {
    return (!Modifier.isStatic (method.getModifiers()));
  }
}
