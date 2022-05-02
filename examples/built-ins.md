# Language Built-Ins

LANG comes with a few built-ins to allow for interaction with the outside world (from within the interpreter). These built-in functions behave like regular LANG functions but are actually written in Rust and inserted into the interpreter scope. Included type annotations are for reference, built-in functions may silently fail or crash the program depending on the importance of the argument types.

- `len(arr: array): number`
- `enumerate(arr: array): array`

- `keys(obj: object): array`
- `values(obj: object): array`

- `type(value: any): string`
- `eval(code: string)`
- `rand(): number`
- `print(value: any)`
- `write_file(path: string, contents: string)`
- `read_file(path: string): string`
- `serve(callback: (request: object) -> string, options: object)`
