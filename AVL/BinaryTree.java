
public class BinaryTree {
    protected BinaryNode root;
    
    /**
     * create an empty tree
     */
    public BinaryTree(){
        root = null;
    }

    /**
     * @return the tree height
     */
    public int height(){
        return root.getHeight();
    }

    /**
     * @return true iff the tree is empty
     */
    public boolean isEmpty(){
        return root == null;
    }

    /**
     * prints the tree inorder
     */
    public void printInOrder(){
    	if (!isEmpty())
    		root.printInOrder();
    }

    /**
     * print the tree by levels
     */
    public void printByLevels(){
        if (!isEmpty())
            root.printByLevels();
        else
            System.out.println("EMPTY TREE");
    }

    
    
    
    
    
    //NOT REALLY IMPLEMENTED
    public void remove(Comparable o){
    }

    
    
    
}