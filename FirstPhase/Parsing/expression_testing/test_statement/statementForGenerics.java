{

	Integer i = new Integer(19);
	//Map<String, Integer> histogram = a;
	double total = 0;
	for (int i : histogram.values())
		total += i;
	for (Map.Entry<String, Integer> e : histogram.entrySet())
		System.out.println(e.getKey() + "" + e.getValue() / total);
	
}

