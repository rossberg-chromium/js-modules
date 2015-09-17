print("begin.")

export let x = print("0")

export module B = A.B

export module A = {
  export let x = print("1")
  export let f = function() { return B.x }
  export module B = {
    module BB = B
    export BB, x
    let x = print("2")
    let y = print("3")
    let Ax = A.x
    let ABx = A.B.x
    let Ay = A.y
    let BBx = BB.x
    let Af = A.f
    let f = function(x,y) { return x }
  }
  export let y = print("4")
  let Ax = A.x
  let Bx = B.x
  let ABx = A.B.x
  module C = {
    export let z = print("5")
    export module D = B
    // import C.z  // multiple declarations
    import B.x
  }
  module D = {
    // import A.*  // invalid forward import
  }
  module M = {}
  // import M.*  // invalid forward import
  let Cz = C.z
  let CDx = C.D.x
}

export module Imports = {
  module A1 = { 
    export module A2 = {}
  }
  module B = {
    import A1.*
//    import A2.*  // unbound variable A2
  }
}

export module E = {
  export let xx = x
  export y, B
  let Bx = B.x
  import A.*
}

export module M1 = {
  export module A2 = M2
}
export module M2 = {
  export module A1 = M1
}

// module W1 = W2.W
// module W2 = { export module W = W3 }
// module W3 = W1  // cyclic module definition

// module W1 = W2.W3
// module W2 = {
//   export module W3 = W4
//   export module W4 = W1
// }  // cyclic module definition

module M3B = M3.B
export module M3 = {
  export module B = { export let x = "" }
  module C1 = { import M3.* }
  module C2 = { import M3.B.* }
  module C3 = { import M3B.* }
  module C4 = { export x import B.* }
//  export module C5 = { import C5.* }  // invalid forward import
//  export module C6 = { import M3.C6.* }  // invalid forward import
}

export module External = from "external.js"
export module External1 = External
export module ExternalA = External.A
export module InnerExternal = {
  export module E = from "external.js"
}
export module External2 = InnerExternal.E
export let xxx = InnerExternal.E.A.x

print("end.")
