{

      for (int i = 0; i < hash.length; i++)
      {
         int v = hash[i] & 255;
         if (v < 16) d += "0";
         d += Integer.toString(v, 16).toUpperCase() + " ";
      }
	
}

