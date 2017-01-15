{
	try {
		//File inputFile = new File("menus.xml");
		SAXParserFactory saxParserFactory = SAXParserFactory.newInstance();
		SAXParser saxParser = saxParserFactory.newSAXParser();
		XMLHelper myXMLParser = new XMLHelper();
		//saxParser.parse(inputFile, myXMLParser);
		String url = "http://services.telecom-bretagne.eu/rak/rss/menus.xml";
		saxParser.parse(new InputSource(
		                    new StringReader(
		                            NetworkHelper.doGET(url, null).getResponse()
		                    )
		                ), myXMLParser);
		/* see if it worked */
		List<Menu> ml = MenuLab.getInstance().getMenus();
		for(Menu m :ml) {
		    System.out.println(m.toString());
		}
	} cacth (Exception ex) {
		ex.printStackTrace();
	}
}
