// Copyright (C) 2001 Philip John Dorrell

package com1729.utility;

/**
 * 
 * @author Philip Dorrell
 */

public class OrderedPair {

  private Object first;
  private Object second;
  
  public OrderedPair (Object first, Object second) {
    this.first = first;
    this.second = second;
  }
  
  public Object getFirst() {
    return first;
  }
  
  public Object getSecond() {
    return second;
  }
  
  public boolean equals (Object object) {
    if (object instanceof OrderedPair) {
      OrderedPair pair = (OrderedPair)object;
      return pair.first.equals (first) && pair.second.equals (second);
    }
    else {
      return false;
    }
  }
  
  public int hashCode() {
    return first.hashCode() ^ (second.hashCode() * 37);
  }
  
}
