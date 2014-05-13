
public class BinarySearchTree extends BinaryTree{
    
    /**
     * create a tree
     */
    public BinarySearchTree(){
        super();
    }
    
    /**
     * Inserting a new node in the tree with given data
     * @param toAdd with MyObject data
     */
    public void insert(MyObject toAdd){
    	if (toAdd == null) throw new RuntimeException("No null allowed, Go away!");
        if (root == null){
            root = new BinarySearchNode(toAdd);
        }
        else
            root.insert(toAdd);
    }
    
    
    /**
     * removes the object from the search tree
     */
    public void remove(Comparable object){
            root.remove(object);
    }
    
    /**
     * Searches the search tree for a node that overlaps (defined in the assignment description) with the interval [start, end]
     * Returns the first overlapping node it encounters in the tree.
     */
    public MyObject OverlapSearch(Comparable start, Comparable end){
        if (root != null)
            return root.OverlapSearch(start, end);
        return null;
            
    }
}
