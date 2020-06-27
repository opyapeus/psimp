package Helper

type Any = interface{}
type Dict = map[string]Any
type Fn = func(Any) Any

func Apply(f Any, x Any) Any {
	fn, _ := f.(Fn)
	return fn(x)
}
