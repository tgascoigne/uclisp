// Code generated by "stringer -type=Op"; DO NOT EDIT

package uclisp

import "fmt"

const _Op_name = "OpNOPOpLOADOpLOOKUPOpLOOKUPCOpCONSOpCAROpCDROpSETCAROpSETCDROpAPPLYOpRETURNOpEVALOpCOMPILEOpSELECTOpJOINOpEQUALOpADDOpSUBOpMULOpDIVOpMODOpEND"

var _Op_index = [...]uint8{0, 5, 11, 19, 28, 34, 39, 44, 52, 60, 67, 75, 81, 90, 98, 104, 111, 116, 121, 126, 131, 136, 141}

func (i Op) String() string {
	if i < 0 || i >= Op(len(_Op_index)-1) {
		return fmt.Sprintf("Op(%d)", i)
	}
	return _Op_name[_Op_index[i]:_Op_index[i+1]]
}
