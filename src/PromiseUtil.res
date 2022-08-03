@val external hrnow: () => array<float> = "process.hrtime"

let hrnow = () => {
  let split = hrnow()
  split[0] +. split[1] /. 1e9
}

let timeWithResult = (p: Promise.t<'a>): Promise.t<('a, float)> => {
  let start = hrnow()
  p->Promise.then(result => {
    let end = hrnow()
    Promise.resolve((result, end -. start))
  })
}

let timeWithCallback = (p: Promise.t<'a>, cb: float => unit): Promise.t<'a> => {
  let start = hrnow()
  p->Promise.finally(_ => {
    let end = hrnow()
    cb(end -. start)
  })
}