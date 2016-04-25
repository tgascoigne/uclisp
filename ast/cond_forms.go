package ast

func init() {
	specialForms[Symbol("cond")] = SpecialForm{addForm}
}
