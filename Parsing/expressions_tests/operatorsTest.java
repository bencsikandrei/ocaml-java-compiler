      double d1 = 123.4;
      double d2 = 1.234e2;
      float f1  = 123.4f;
      int a, b, c;
      a = 10;
      b = (a == 1) ? 20: 30;
      System.out.println( "Value of b is : " +  b );

      b = (a == 10) ? 20: 30;
      System.out.println( "Value of b is : " + b );

      c = a + b;
      c = a - b;
      c = a * b;
      c = a / b;
      c = a % b;

      a += b;
      a -= b;
      a *= b;
      a /= b;
      a %= b;
      a++;
      b--;
      ++a;
      --b;

      /* Bitwise */
      a = 60;	/* 60 = 0011 1100 */
      b = 13;	/* 13 = 0000 1101 */
      c = 0;

      c = a & b;        /* 12 = 0000 1100 */
      System.out.println("a & b = " + c );

      c = a | b;        /* 61 = 0011 1101 */
      System.out.println("a | b = " + c );

      c = a ^ b;        /* 49 = 0011 0001 */
      System.out.println("a ^ b = " + c );

      c = ~a;           /*-61 = 1100 0011 */
      System.out.println("~a = " + c );

      c = a << 2;       /* 240 = 1111 0000 */
      System.out.println("a << 2 = " + c );

      c = a >> 2;       /* 15 = 1111 */
      System.out.println("a >> 2  = " + c );

      c = a >>> 2;      /* 15 = 0000 1111 */
      System.out.println("a >>> 2 = " + c );

      c &= b;
      c |= b;
      c ^= b;     
      c >>= 2;
      c <<= 2;
      c >>>= 2;

      /* booleans */
      boolean A = true;
      boolean B = false; 

      System.out.println("A|B = "+(A|B));
      System.out.println("A&B = "+(A&B));
      System.out.println("A||B = "+(A||B));
      System.out.println("A&&B = "+(A&&B));
      System.out.println("!A = "+(!A));
      System.out.println("A^B = "+(A^B));
      System.out.println("(A|B)&A = "+((A|B)&A));

