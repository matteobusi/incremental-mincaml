open FunSpecification

let _ =
    let t = FunSpecification.Unit(()) in
        Printf.printf "%B" (FunSpecification.compat (FunContext.empty ()) (FunContext.empty ()) t)
