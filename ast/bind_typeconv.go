package ast

func typeConvIn(v interface{}) Value {
	if i, ok := v.(int); ok {
		return Integer(i)
	}

	// Can't do any type conversion
	return BindType(v)
}

func typeConvOut(v Value) interface{} {
	if v.Type() == IntegerType {
		return int(v.(Integer))
	}

	// Can't do any type conversion
	return v
}
