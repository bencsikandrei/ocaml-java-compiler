{	
	// tringBuffer buf = new StringBuffer(8);
	do {
		buf.append(Character.forDigit(i & 2, 16));
		i >>>= 4;
	} while (i != 0);
	
	do {
		myclass.callMethod();
		this.something();
	} while (i > 4);
	// return buf.reverse().toString();
	
}

