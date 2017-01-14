{
	if(!a) { ; }
	if (a < b) {}
	if (a instanceof b) {}
	
	if(false) 
		a += 2;
	
	if(true)
		if (false) 
			b = 1;
		else
			c = 1;
	
	
	
	if (a) {
		a = 1;
	} else if(b) { 
		a = 2; 
	} else {
		b = 3;
	}
	
	if( a && b ) {
		;
	}
	
	if( a || b ) {
		;
	}
	// intentional 
	if( a = b ) {
		;
	}
	
	if ( a != 2 ) {
		;
	}
}

