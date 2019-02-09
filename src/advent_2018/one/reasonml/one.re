module A = Belt.Array;

let logl = l => l |> Array.of_list |> Js.log;
let convert: string => int = [%raw a => "{return +a}"];

let input =
  Node.Fs.readFileAsUtf8Sync("input.txt")
  |> Js.String.split("\n")
  |> Array.map(convert)
  |> Array.to_list;

/* Js.log(input); */


/* part 1 */
let testInput = [+1, -2, +3, +1]
List.reduce(testInput, 0, (+))->Js.log 

/* part 2 -- what is the first frequence your device reaches twice */
/*  */

let rec f = (seen, frequencies, currentFrequency) => {
  let head = List.hd(frequencies);
  /* Js.log("head"); */
  /* Js.log(head); */

  /* Js.log("seen"); */
  /* Js.log(seen); */

  Js.log("currentFrequency")
  Js.log(currentFrequency)
  let newFrequency = currentFrequency + head;
  /* Js.log("newFrequency"); */
  /* Js.log(newFrequency); */

  let exists = List.exists(x => x === newFrequency, seen);

  if (exists) {
    newFrequency
      Js.log("The end.")
  } else {

    let v = seen@[newFrequency]

    /* logl(v) */
    f(v, List.tl(frequencies), newFrequency);
  }
};

/* let result = f([0], input, 0); */

/* Js.log("result"); */
/* Js.log(result); */


