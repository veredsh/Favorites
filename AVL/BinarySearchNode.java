
public class BinarySearchNode extends BinaryNode {

    
	/**
	 *  constructor, creates new BinarySearchNode with given data
	 * @param data MyObject type
	 */
	public BinarySearchNode(MyObject data) {
        super(data);
    }

    /**
	 *  constructor, creates new BinarySearchNode with given data & parent
	 * @param data
	 * @param parent
	 */
    public BinarySearchNode(MyObject data,BinarySearchNode parent) {
        super(data,parent);
    }
    
	/**
	 *  copy constructor, creates new BinarySearchNode
	 * @param toCopy the node to copy
	 */
    public BinarySearchNode(BinarySearchNode toCopy){
        super(toCopy);
    } 
    
    /**
     * inserting a new object in the tree
     * which root is this
     * @param toAdd the data of the inserted node
     * @return inserted node in the tree
     * assumption: this node is not empty.
     */
    protected BinaryNode insert(MyObject toAdd) {
    	if (toAdd == null) throw new RuntimeException("No null allowed, Go away!");
        BinarySearchNode newNode = createNode(toAdd);
        BinarySearchNode current = this;
        while (current != null)
        {
            if (toAdd.getKeyData().compareTo(current.getData().getKeyData()) < 0){
                if (current.getLeft() == null){
                    current.setLeft(newNode);
                    newNode.setParent(current);
                    current.update(); //sending the new node to fix max, height up to root
                    return current.getLeft();
                }
                else
                    current = current.getLeft();
                }
            else{
               if (toAdd.getKeyData().compareTo(current.getData().getKeyData()) > 0){
                   if (current.getRight() == null){
                      current.setRight(newNode);
                      newNode.setParent(current);
                      current.update(); //sending the new node to fix max, height up to root
                      return current.getRight();
                    }
                    else
                        current = current.getRight();
                    }
                 else
                         throw new RuntimeException("You tried to insert existing key, Go away!");
                }
            }
            return null;
        }
       
    
    /**
     * Removing a node in the tree with given begin key
     * @param toRemove begin time key
     * @return parent of removed node, null if root removed
     */
    protected BinaryNode remove(Comparable toRemove){
        BinarySearchNode z = ((BinarySearchNode)(find(toRemove)));
        if (z == null) return z;
        BinarySearchNode y;
        BinarySearchNode x;
        if (z.getParent() == null) // checking z is root with no children
            if ((z.getLeft() == null) && (z.getRight() == null))
                return null;
                
        if ((z.getLeft() == null) || (z.getRight() == null))
            y = z;
        else
            y = z.succ();
        if (y.getLeft() != null) 
            x = y.getLeft();
        else
            x = y.getRight();
        if (x != null)
            x.setParent(y.getParent());     
        if (y.getParent() == null){
            if (x == null) return null;
            else{
                z.setData(x.getData());
                z.setHeight(x.getHeight());
                z.setMax(x.getMax());
                z.setLeft(x.getLeft());
                z.setRight(x.getRight());
            }
        }
        else{
            if (y == (y.getParent()).getLeft())
                (y.getParent()).setLeft(x);
            else
                (y.getParent()).setRight(x);
        }
        
        if (y != z){
            z.setData(y.getData());
        }       
        y.update();
        return z.getParent();
    }

    
    /** 
     * Retrieves the Successor of x
     * assuming the right sub tree isn't null
    */ 
    private BinarySearchNode succ(){
    	return this.getRight().minimum();
    }
    
    
    /** 
     * Finds the Min key in sub tree
    */ 
    private BinarySearchNode minimum(){
        BinarySearchNode x = this;
        while (x.getLeft() != null){
            x = x.getLeft();
        }
        return x;
    }
    

    /** 
     * Updating the correct height in the nodes after insert/remove
    */ 
    protected void update(){
            BinarySearchNode x = this;
            Comparable leftMax;
            Comparable rightMax;
            int leftHeight;
            int rightHeight;
            while (x != null){
                if (x.getLeft() != null){
                    leftHeight = x.getLeft().getHeight();
                    leftMax = x.getLeft().getMax();
                }
                else{
                    leftHeight = 0;
                    leftMax = 0;
                }
                    
                if (x.getRight() != null){
                    rightHeight = x.getRight().getHeight();
                    rightMax = x.getRight().getMax();
                }
                else{
                    rightHeight = 0;
                    rightMax = 0;
                }
                
                Comparable b;   
                if (leftMax.compareTo(rightMax) >= 0)
                    if (leftMax.compareTo(x.getData().getMaxData()) >=0)
                        b = leftMax;
                    else b = x.getData().getMaxData();
                else
                    if (rightMax.compareTo(x.getData().getMaxData()) >=0)
                        b = rightMax;
                    else b = x.getData().getMaxData();
                
                x.setMax(b);
                x.setHeight(Math.max(leftHeight, rightHeight) + 1);
                x = x.getParent();
        }
    }

    

    /** 
     * finds the node with the key toFind
     * @param toFind of type Comparable
    */ 
    protected BinaryNode find(Comparable toFind){
        int a = this.getData().compareTo(toFind);
        if (a == 0) return this;
        if ((a > 0) && (this.getLeft() != null)) 
            return this.getLeft().find(toFind);
        else
            if ((a < 0) && (this.getRight() != null))
                return this.getRight().find(toFind);
        return null;//the node not found.
    }
    

    /** 
     * Searches if there's an overlap in the tree
     * @return the data if so, null if not
     * @param start, end of type Comparable
    */ 
    protected MyObject OverlapSearch(Comparable start,Comparable end){
        BinarySearchNode t = this;
        if (this.getMax().compareTo(start) < 0)
            return null;
        while (t != null ){
        	if (t.getData().overlap(start, end)) return t.getData();
            if ((t.getLeft() != null) && (t.getLeft().getMax().compareTo(start) > 0))
                t = t.getLeft();
            else
                t = t.getRight();
        }
        if (t == null) return null;
        return t.getData();
    }
    


    /** 
     * creates a new node from data
     * @param data of type MyObject
    */ 
    protected BinarySearchNode createNode(MyObject data){
    	if (data == null) throw new RuntimeException("No null allowed, Go away!");
        return new BinarySearchNode(data);
    }
    
    /*
     * returns the parent as a BinarySearchNode(non-Javadoc)
     * @see BinaryNode#getParent()
     */
    protected BinarySearchNode getParent(){
        return (BinarySearchNode)(super.getParent());
    }

    /*
     * returns the left child as a BinarySearchNode(non-Javadoc)
     * @see BinaryNode#getParent()
     */
    protected BinarySearchNode getLeft(){  
        return (BinarySearchNode)(super.getLeft());
    }
    
    /*
     * returns the right child as a BinarySearchNode(non-Javadoc)
     * @see BinaryNode#getParent()
     */
    protected BinarySearchNode getRight(){
        return (BinarySearchNode)(super.getRight());
    }


}