{

		if ( args.length < 2 ) {
			System.out.println("Please enter an address & port ");
			System.exit(-1);
		};
		// create socket
		try {
			System.out.println("Creating socket ..");
			Socket clSock = new Socket();
			//
			System.out.println("Creating socket ..");
			InetAddress servAddr = (new InetAddress()).getByAddress(args[0].toBytearray());
			//
			System.out.println("Creating socket ..");
			clSock.connect(servAddr, Integer.parseInt(args[1]));
		} catch (Exception ex) {
			System.out.println("There was a problem ..");
		}
	
}

