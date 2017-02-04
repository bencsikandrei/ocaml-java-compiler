
public class LinkedList<T> {
    /* only need to store a single pointer to the node at the head
     * of the list.
     * The pointer is null if the list is empty.
     * Also record the size of the list.
     */
    protected Node<T> head;
    /* invariant: size is the number of nodes in the list pointed to by head */
    protected int size;

    /* no-arguments default constructor creates an empty list */
    public LinkedList() {
	head = null;		// start with an empty list
	size = 0;
    }

    /* accessor method */
    public int size() {
	return size;
    }

    /* @param	value to add to the end of the list
     */
    public void add(T value) {
	head = addAtEnd(head, value);
	size++;
    }

    /* @param	node of the list to which the value should be added
     * @param	value to add to the end of the list
     */
    private Node<T> addAtEnd(Node<T> node, T value) {
	if (node == null) {	// special case
	    return new Node<T>(value, null);
	} else if (node.getNext() == null) { // other special case
	    node.setNext(new Node<T>(value, null));
	} else {
	    addAtEnd(node.getNext(), value);
	}
	return node;
    }

    /* iterative implementation of the same method
     * @param	value to add to the end of the list
     */
    public void add2(T value) {
	if (head == null) {
	    head = new Node<T>(value, null);
	} else {
	    Node<T> node = head; // guaranteed not to be null initially
	    while (node.getNext() != null) {
		node = node.getNext(); // guaranteed not to be null here
	    }
	    // now, node.getNext() is guaranteed to be null
	    // similar to the second special case in addAtEnd
	    node.setNext(new Node<T>(value, null));
	}
	size++;
    }

    /* @param	position of item to be removed
     * @throws	BadItemCountException if this is not a valid position
     * 		position is 1-based, so position = 1 removes the head
     */
    public void remove(int position) throws BadItemCountException {
	if ((position < 1) || (position > size)) {
	    throw new 
		BadItemCountException("invalid position " + position +
				      ", only 1.." + size + " available");
	}
	if (position == 1) {
	    head = head.getNext();
	} else {
	    Node<T> node = head;
	    for (int i = 2; i < position; i++) {
		node = node.getNext();
	    }
	    // set this node's "next" pointer to refer to the
	    // node that is after the next
	    node.setNext(node.getNext().getNext());
	}
	size--;			// one less item
    }

    /* convert the list to a printable string
     * @return	a string representing the stack
     */
    public String toString() {
	return toString(head);
    }
    private String toString(Node<T> node) {
	if (node == null) {
	    return "";
	} else {
	    return node.getValue() + "\n" + toString(node.getNext());
	}
    }

    /* unit test -- test all the methods in this class
     * @param	ignored
     */
    public static void main(String[] args) {
	/* create two empty lists, make sure they print out correctly */
	LinkedList<String> list1 = new LinkedList<String>();
	LinkedList<String> list2 = new LinkedList<String>();
	System.out.println("list1 = '" + list1 + "', list2 = '" + list2 + "'");
	System.out.println("list1.size() = " + list1.size() +
			   ", list2.size() = " + list2.size());

	/* insert some items, keep checking */
	list1.add("hello");
	list1.add("world");
	list2.add("foo");
	list2.add("bar");
	list2.add("baz");
	System.out.println("list1 = '" + list1 + "', list2 = '" + list2 + "'");
	System.out.println("list1.size() = " + list1.size() +
			   ", list2.size() = " + list2.size());

	/* remove an item at an invalid position */
	boolean caught = false;
	try {
	    list2.remove(4);
	} catch (BadItemCountException e) {
	    caught = true;
	}
	if (! caught) {
	    System.out.println("error: no exception for invalid remove");
	    System.out.println("list1 = '" + list1 +
			       "', list2 = '" + list2 + "'");
	}
	System.out.println("list1 = '" + list1 + "', list2 = '" + list2 + "'");

	/* remove some items at valid positions */
	try {
	    list1.remove(1);
	    System.out.println("list1 = '" + list1 +
			       "', list2 = '" + list2 + "'");
	    list2.remove(2);
	    System.out.println("list1 = '" + list1 +
			       "', list2 = '" + list2 + "'");
	    list2.remove(2);
	    System.out.println("list1 = '" + list1 +
			       "', list2 = '" + list2 + "'");
	} catch (Exception e) {
	    System.out.println("caught unexpected exception " + e +
			       ", list1 = '" + list1 + ", list2 = " + list2);
	}
	System.out.println("list1.size() = " + list1.size() +
			   ", list2.size() = " + list2.size());
    }
}
