
type primTypes =
	| PT_Float
	| PT_Boolean
	| PT_Byte
	| PT_Char
	| PT_Int
	| PT_Long
	| PT_Short
	| PT_Double



type allTypes =
	| AL_Types of types
	| AL_Array of types*int

and types=
	| T_Primitive of primTypes
	| T_Qualified of definedType list

and definedType =
	| DT_Id of string
	| DT_Generic of string*types