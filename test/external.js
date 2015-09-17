let x = print("external")

export module A = {
  export let x = print("external.A")
}

export module B = {
  let x = print("external.B")
}

// module X = E  // unbound variable `E'
