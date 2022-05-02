// experimenting with result-type return values for fallible operations
do_something := () -> {
    if math.rand() > 0.5 {
        return err "something went wrong"
    }

    return "woah"
}

value := do_something() // will panic() i.e. crash the program half the time
value := do_something().catch((e) -> {
    // handles the error in case it happens
})

value := do_something() catch (e) -> { /* handle */ }
value := do_something() or null // gives an alternate value
