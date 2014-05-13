public class AVLSearchNode extends BinarySearchNode {
    
	/**
	 *  constructor, creates new AVLSearchNode with given data
	 * @param data
	 */
	public AVLSearchNode(MyObject data) {
        super(data);
    }
    
	/**
	 *  copy constructor, creates new AVLSearchNode
	 * @param toCopy the node to copy
	 */
    public AVLSearchNode(AVLSearchNode toCopy){
    	super(toCopy);
    }
    
    
    /**
	 *  constructor, creates new AVLSearchNode with given data & parent
	 * @param data
	 * @param parent
	 */
    public AVLSearchNode(MyObject data,BinarySearchNode parent) {
        super(data,parent);
    }


    /**
     * inserting a new object in the sub tree
     * which root is this
     * @param toAdd the data of the inserted node
     * @return inserted node in the tree
     */
    protected BinaryNode insert(MyObject toAdd) {
    	if (toAdd == null) throw new RuntimeException("No null allowed, Go away!");
    	AVLSearchNode tmp = (AVLSearchNode)super.insert(toAdd);
        AVLSearchNode x = tmp;
        AVLSearchNode y;
        y = (AVLSearchNode)x.balance();
        while (y == null && x.getParent() != null){
            x = x.getParent();
            y = (AVLSearchNode)x.balance();
        }
        if (y != null)
            tmp.update();
        return tmp;
    }

    /**
     * fixing the balance of the tree after insert/delete
     */
    protected BinaryNode balance(){
        AVLSearchNode left = this.getLeft();
        AVLSearchNode right = this.getRight(); 
        int heightLeft;
        int heightRight;
        if (left != null)
            heightLeft = left.getHeight();
        else
            heightLeft = 0;
        if (right != null)
            heightRight = right.getHeight();
        else
            heightRight = 0;
        
        if ((heightRight - heightLeft) > 1){
                // checking if the right sub tree is right heavy
           if (right.getRight() != null){
              if (right.getLeft() == null){
                    return this.leftRotate();
               }
               else{ 
                        if (right.getRight().getHeight() >= right.getLeft().getHeight())
                               return this.leftRotate();
                          else{
                                right.rightRotate();
                                return this.leftRotate();
                            }
                        }
            }
            else{
                right.rightRotate();
                return this.leftRotate();
            }
        }
        if ((heightLeft - heightRight) > 1){
        	// checking if the left sub tree is left heavy
           if (left.getLeft() != null){
              if (left.getRight() == null){
                    return this.rightRotate();
               }
               else{ 
                        if (left.getLeft().getHeight() >= left.getRight().getHeight())
                               return this.rightRotate();
                          else{
                                left.leftRotate();
                                return this.rightRotate();
                            }
                        }
            }
            else{
               left.leftRotate();
               return this.rightRotate();
            }
        }
        return null;
    }
    
    
    /**
     * Performing a left rotation
     */
    protected AVLSearchNode leftRotate(){
        AVLSearchNode x = this;
        AVLSearchNode y = x.getRight();
        x.setRight(y.getLeft());
        if (y.getLeft() != null)
            (y.getLeft()).setParent(x);
        x.updateNode(); 
        y.setParent(x.getParent());
        y.setLeft(x);
        if (x.getParent() != null){
            if (x == x.getParent().getLeft())
                x.getParent().setLeft(y);
             else 
                x.getParent().setRight(y);
        }
        x.setParent(y);
        y.updateNode();
        return y;
    }
    
    /**
     * Performing a right rotation
     */
    protected AVLSearchNode rightRotate(){
        AVLSearchNode x = this;
        AVLSearchNode y = x.getLeft();
        x.setLeft(y.getRight());
        if (y.getRight() != null)
            (y.getRight()).setParent(x);
        x.updateNode();
        y.setParent(x.getParent());
        y.setRight(x);
        if (x.getParent() != null){
            if (x == x.getParent().getRight())
                x.getParent().setRight(y);
             else 
                x.getParent().setLeft(y);
        }
        x.setParent(y);
        y.updateNode();
        return y;
    }
    
    /**
     * updating the height and max fields of the node
     */
    protected void updateNode(){
        Comparable leftMax;
        Comparable rightMax;
        int leftHeight;
        int rightHeight;
        if (this.getLeft() != null){
            leftHeight = this.getLeft().getHeight();
            leftMax = this.getLeft().getMax();
        }
        else{
            leftHeight = 0;
            leftMax = 0;
        }           
        if (this.getRight() != null){
            rightHeight = this.getRight().getHeight();
            rightMax = this.getRight().getMax();
        }
        
        else{
            rightHeight = 0;
            rightMax = 0;
        }       
        Comparable b;   
        if (leftMax.compareTo(rightMax) >= 0){
            if (leftMax.compareTo(this.getData().getMaxData()) >=0)
                b = leftMax;
            else
                b = this.getData().getMaxData();
        }
        else{
            if (rightMax.compareTo(this.getData().getMaxData()) >=0)
                b = rightMax;
            else b = this.getData().getMaxData();
        }   
        this.setMax(b);
        this.setHeight(Math.max(leftHeight, rightHeight) + 1);
    
    }
    
    /**
     * creates a new node with given data
     * @param data
     */
    protected AVLSearchNode createNode(MyObject data){
    	if (data == null) throw new RuntimeException("No null allowed, Go away!");
        return new AVLSearchNode(data);
    }
    
    /**
     * retrieves the parent of current node
     * @return AVLSearchNode the parent
     */
    protected AVLSearchNode getParent(){
        return (AVLSearchNode)super.getParent();
    }

    /**
     * retrieves the left child of current node
     * @return AVLSearchNode the left child
     */
    protected AVLSearchNode getLeft(){
        return (AVLSearchNode)super.getLeft();
    }

    /**
     * retrieves the right child of current node
     * @return AVLSearchNode the right child
     */
    protected AVLSearchNode getRight(){
        return (AVLSearchNode)super.getRight();
    }
    
    /**
     * finds the node in the tree with the given keydata toFind
     * @param toFind given data
     * @return the node found or null else
     */
    protected BinaryNode find(Comparable toFind){
        return super.find(toFind);
    }
    
}
