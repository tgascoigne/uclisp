package uclisp

type Type int

const (
	ListType Type = iota
	IntegerType
	StringType
	SymbolType
	ProcedureType
	ProgType
	UnknownType
)

func TypeOf(e Elem) Type {
	switch e.(type) {
	case List:
		return ListType
	case Integer:
		return IntegerType
	case String:
		return StringType
	case Symbol:
		return SymbolType
	case Procedure:
		return ProcedureType
	case Prog:
		return ProgType
	}
	return UnknownType
}
