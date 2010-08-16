// Copyright (C) 2000 Philip John Dorrell

package com1729.methods;

import java.lang.reflect.*;

/**
 * 
 * @author Philip Dorrell
 */

public class ListInstanceMembers extends ListMembers {

  public boolean isSelectedType (Field field) {
    return (!Modifier.isStatic (field.getModifiers()));
  }
  public boolean isSelectedType (Method method) {
    return (!Modifier.isStatic (method.getModifiers()));
  }

}
