{
	"*** Testing combination of operators ***";
	"arithmetic operators";
	++a + b + c + 10 - 2 - 1 + 1;
	++(a + b) + c + 10 - 2 - 1 + 1;
	a * ++b * c / 10 * 2 % 1 % 1;
	a + b - --c * 10 / 2 % 1;
	a-- + b++ * c / 10 - 2 % 1;
	;
	"Booleans";
	!a && false || c && true || a ^ b;
	!(a && false || c) && true || a ^ b;
	a && false && !c || true ^ b || a;
	a && false || c ^ b || !true && !a; 
	;
	"Bitwise";
	~a & b | c ^ 1 << a >> 10 >> c;
	~(a & b | c ^ 1) << a >> 10 >> c;
	a << b | c ^ ~1 & a ^ 10 >> c;
	a << b >> c >>> ~1 & a ^ 10 >> c;
}
