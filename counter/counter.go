package counter

// Counter returns a channel which produces sequential numbers
// Used for unique id generation
func New() <-chan int {
	c := make(chan int)
	go func() {
		i := 0
		for {
			c <- i
			i++
		}
	}()
	return c
}
