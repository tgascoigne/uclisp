package uclisp

func Equal(env Env, v1, v2 Elem) bool {
	// are we comparing to t?
	switch {
	case v1 == True:
		return !IsNil(v2)
	case v2 == True:
		return !IsNil(v1)
	}

	// are the types equal?
	if TypeOf(v1) != TypeOf(v2) {
		return false
	}

	// perform the type specific comparison
	return v1.Equals(env, v2)
}
