import java.util.LinkedList;
import java.util.Queue;


public class BinaryNode {
	// variables of each node in the tree
	private BinaryNode left;
	private BinaryNode right;
	private BinaryNode parent;
	private MyObject data;
	private int height;
	private Comparable max;
	
	/**
	 *  constructor, creates new BinaryNode with given data
	 * @param data MyObject type
	 */
	public BinaryNode(MyObject data) {
		if (data == null) throw new RuntimeException("No null allowed, Go away!");
		this.data = data;
		this.left = null;
		this.right = null;
		this.parent = null;
		this.height = 1;
		this.max = data.getMaxData();
	}

	/**
	 *  Copy constructor, copies a given BinaryNode type node
	 * @param toCopy BinaryNode type
	 * Presumes input is valid
	 */
	public BinaryNode(BinaryNode toCopy){
		this.data = toCopy.getData();
		this.left = toCopy.getLeft();
		this.right = toCopy.getRight();
		this.parent = toCopy.getParent();
		this.height = toCopy.getHeight();
		this.max = toCopy.getMax();
	}
	
	/**
	 *  constructor, creates new BinaryNode with given data and parent
	 * @param data MyObject type
	 * @param parent of BinaryNode type
	 */
	public BinaryNode(MyObject data, BinaryNode parent) {
		if (data == null) throw new RuntimeException("No null allowed, Go away!");
		this.data = data;
		this.left = null;
		this.right = null;
		this.parent = parent;
		this.height = 1;
		this.max = data.getMaxData();
	}
	
	 
	/**
	 *  Retrieves height of node
	 */
	protected int getHeight(){
		return height;
	}

	/**
	 *  Retrieves data of node
	 */
	protected MyObject getData(){
		return data;
	}

	/**
	 *  Retrieves max of node
	 */
	protected Comparable getMax(){
		return max;
	}
	
	/**
	 *  Retrieves the left Sub Tree Height of node
	 */
	protected int leftSubTreeHeight(){
		if (left==null) return 0;
		return left.getHeight();
	}

	/**
	 *  Retrieves the right Sub Tree Height of node
	 */
	protected int rightSubTreeHeight(){
		if (right==null) return 0;
		return right.getHeight();
	}
	
	
	/**
	 *  Sets the data of the node
	 *  @param o data of type MyObject
	 */
	protected void setData(MyObject o){
		if (o == null) throw new RuntimeException("No null allowed, Go away!");
		this.data = o;
	}

	/**
	 *  Sets the right pointer of a node
	 *  @param node of type BinaryNode
	 */
	protected void setRight(BinaryNode node){
		this.right = node;
	}

	/**
	 *  Sets the left pointer of a node
	 *  @param node of type BinaryNode
	 */
	protected void setLeft(BinaryNode node){
		this.left = node;
	}
	
	/**
	 *  Sets the parent pointer of a node
	 *  @param node of type BinaryNode
	 */
	protected void setParent(BinaryNode node){
		this.parent = node;
	}

	/**
	 *  Sets the height of a node
	 *  @param h of type integer
	 */
	protected void setHeight(int h){
		if (h < 0 ) throw new RuntimeException("No negative num allowed, Go away!");
		this.height = h;
	}

	/**
	 *  Sets the max of a node
	 *  @param max of type Comparable
	 */
	protected void setMax(Comparable max){
		this.max = max;
	}

	
	
	/*
	 * String rep for the object
	 * @see java.lang.Object#toString()
	 */
	public String toString(){
		return this.data.toString()+" max="+this.max.toString()+" height="+this.height;
	}


	/**
	 * @return left child of the node
	 */
	protected BinaryNode getLeft(){
		return this.left;
	}

	/**
	 * @return right child of the node
	 */
	protected BinaryNode getRight(){
		return this.right;
	}


	/**
	 * @return parent of the node
	 */
	protected BinaryNode getParent(){
		return this.parent;
	}


	/**
	 * prints the subtree by levels
	 */
	protected void printByLevels() {
		Queue<BinaryNode> q = new LinkedList<BinaryNode>();
		q.add(this);
		while(!q.isEmpty()){
			BinaryNode node = q.poll();
			if(node!=null){
				q.add(node.left);
				q.add(node.right);
				System.out.println(node);
			}
			else
				System.out.println("EMPTY NODE");
		}
	}

	/**
	 * prints the subtree inorder
	 */
	protected void printInOrder() {
		if(this.left!=null)
			this.left.printInOrder();

		System.out.println(this);

		if(this.right!=null)
			this.right.printInOrder();
	}
	

	/**
	 * insert a node to the subtree
	 */
	protected BinaryNode insert(MyObject toAdd){
		BinaryNode ans = null;
		if (Math.random() < 0.5) {
			if (left == null){
				left = new BinaryNode(toAdd);
				ans =  left;
			}
			else
				left.insert(toAdd);
		}
		else {
			if (right == null){
				right = new BinaryNode(toAdd);
				ans = right;
			}
			else
				right.insert(toAdd);
		}
		return ans;
	}

	protected void updateNode(){
		return;
	}
	//NOT REALLY IMPLEMENTED 
	protected BinaryNode remove(Comparable o){
		return null;
	}

	//NOT REALLY IMPLEMENTED 
	protected MyObject OverlapSearch(Comparable start,Comparable end){
		return null;
	}

	//NOT REALLY IMPLEMENTED
	protected BinaryNode balance(){ 
		return this;
	}
	
	protected BinaryNode find(Comparable toFind){
	    return null;
	   }
	
}
