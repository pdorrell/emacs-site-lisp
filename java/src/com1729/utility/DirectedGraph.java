// Copyright (C) 2001 Philip John Dorrell

package com1729.utility;

/**
 * 
 * @author Philip Dorrell
 */

import java.util.*;

public class DirectedGraph {
  
  HashSet nodes = new HashSet();

  HashMap successors = new HashMap();
  
  HashMap predecessors = new HashMap();
  
  public void addNode (Object node) {
    nodes.add (node);
  }
  
  public boolean containsNode (Object node) {
    return nodes.contains (node);
  }
  
}
