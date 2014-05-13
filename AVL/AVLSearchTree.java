public class AVLSearchTree extends BinarySearchTree {
    
    /**
     * construct a new AVLSearchTree
     */
    public AVLSearchTree(){
        super();
    }
    
    /**
     * Inserting a new node in the tree with given data
     * @param toAdd with MyObject data
     */
    public void insert(MyObject toAdd){
    	if (toAdd == null) throw new RuntimeException("No null allowed, Go away!");
        AVLSearchNode tmp = null;
        AVLSearchNode x = null;
        if (root == null){
            root = new AVLSearchNode(toAdd);
            return;
        }
        else{
            tmp = (AVLSearchNode)(root.insert(toAdd)) ;
            x = tmp;
            while (x.getParent() !=null)
                x = x.getParent();
        }
        root = x;
    }
    
    /**
     * Removing a node in the tree with given begin key
     * @param toRemove begin time key
     */
    public void remove(Comparable toRemove){
        if (root != null){
            if ((root.getHeight() == 1) && (toRemove.compareTo(root.getData().getKeyData()) == 0))
            //the root is a leaf and need to be removed.
                root = null;
            else {
                if (root.find(toRemove) != null){
                    AVLSearchNode current = ((AVLSearchNode)root.remove(toRemove));
                    if (current != null){  //if current == null then root not changed!
                        current.updateNode();
                        current.balance();
                        while (current.getParent() != null){
                            current = current.getParent();
                            current.updateNode();
                            current.balance();
                        } // end while
                        root = current;
                    } // end if
                    else{     //the root was removed but it had children.
                        root.updateNode();
                        AVLSearchNode tmp = ((AVLSearchNode)root.balance());
                        if (tmp != null) root = tmp;
                    }
                } // end if
            } // end else 
        } // end big if
    }
  
    
}