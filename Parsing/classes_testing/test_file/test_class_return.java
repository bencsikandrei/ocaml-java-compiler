class Graph {

	public Graph loseEdges(int i, int j) {
		int n = edges.length;
		int[][] newedges = new int[n][];
		edgelists: for (int k = 0; k < n; ++k) {
			int z;
			search: {
			if (k == i) {

			} else if (k == j) {

			}
			newedges[k] = edges[k];
			continue edgelists;
			} // search

		} // edgelists
		return new Graph(newedges);
	}
}
