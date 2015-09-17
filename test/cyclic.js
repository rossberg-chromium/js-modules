
module C = A.B
let y = C.y
module A = {
  import C.y
  let x = y
  export module B = {
    let z = C.y
    export let y = x
    print(x)
  }
}


/*
module A = B.C.F
module D = { }
module B = {
  import A.D import A.G
  import G.x
  export module C = {
    export module E = D
    export module F = {
      export module G = E
      export module D = { export let x = "" }
    }
  }
}
*/
