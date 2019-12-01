# templates-to-go

This tool allows for easy bootstrapping of common code patterns by filling out templates combined with lua scripting.

## Template file

`<template>.tmpl` is the main template file. Everything in the file is treated as verbose and returned as-is.
The only difference is anything contained inside `{{` and `}}`.
The contents between the brackets can either be a lua expression or a control structure.
If the content is a lua expression, the expression is evaluated and the result inserted into the template.

### Control structures

There exist only two control structures: Conditional template parts and loops.
A conditional structure has the form:

```
{{if expression}}
True branch
{{else if expression}}
Second true branch
{{else}}
false branch
{{end if}}
``` 

The `else if` and `else` blocks are optional.
`expression` is any lua expression. It has to evaluate to a boolean value. Any other value is treated like an error.

A loop has the following form:

```
{{for pattern in expression}}
Loop body
{{end for}}
```

`pattern` is a pattern assigning new variables which are only valid during the loop body.
`expression` is a lua expression. It has to evaluate to a table. If the table has numeric keys, it is treated like an array. 
If it has other keys and pattern matches for keys and values, it is treated like a map and we iterate over the keys and values.

A pattern is one of the following:

- name
- (name1, name2)
- (name1, (name2, name3))
- ...

The pattern has to match to the type of the value iterated over.

## Template code

The file `<template>.lua` can contain additional code which will be in scope while evaluating lua expressions.
You can use this to define additional helper functions, e.g., to capitalize strings or help with indention.

## Template Variables

`<template>.var` contains a list of input fields with their types (to help entering the data for template expansion).
Each line is of the form: `<name>: <type>`.
Possible types are:

- string
- number
- boolean
- (T)
- \[T]
- (T1, T2)
- (T1, T2, T3)
- ...