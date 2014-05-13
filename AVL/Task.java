
public class Task implements MyObject{
	private Integer begin;
	private Integer end;
	private String name;
	
		
	/**
	 *  constructor, creates new Task with given data
	 * @param begin, end of type Integer
	 * @param name of type String
	 */
	public Task(Integer begin,Integer end, String name) {
		this.begin = begin;
		this.end = end;
		this.name = name;
	}

	/**
	 *  Retrieves begin time
	 */
	public Comparable getKeyData() {
		return this.begin;
	}

	/**
	 *  Retrieves end time
	 */
	public Comparable getMaxData() {
		return this.end;
	}

	@Override
	/** Compares two objects of MyObject type, their beginning time
	 * return a negative number if this begin time is smaller than the given
	 * begin time, 0 if equal and a positive number otherwise 
	 */
	public int compareTo(Object o) {
		if (!(o instanceof MyObject)) throw new RuntimeException("Me wants MyObject type only, Go away!");
		return begin.compareTo((Integer)((MyObject)o).getKeyData());
	}
	
	
	/** Checks if there's an overlap between this work time and given work time.
	 * @return true if overlap occurs 
	 */
	public boolean overlap(Comparable start, Comparable end){
		if (start.compareTo(this.begin) >= 0){
			return (start.compareTo(this.end) <= 0);
		}
		else{
			return (end.compareTo(this.begin) >= 0 );
		}
	}

	/**
	 * Prints data fields
	 */
	public String toString(){
		return "name="+this.name +" time=["+this.begin+","+this.end+"]";
	}

}
