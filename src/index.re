let explode = (aString: string) => {
    let rec expl = (i: int, l: list(char)) =>
      if (i < 0) {
        l;
      } else {
        expl(i - 1, [aString.[i], ...l]);
      };
    expl(String.length(aString) - 1, []);
  };

let add = '+';
let eq = '=';
let code = "5 + 5 = 10";
let splitCode = List.filter(a => a != ' ', explode(code));

let charToInt = (aChar) => int_of_string (String.make(1, aChar));
let rec checkArith = (charStream) => {
    switch charStream {
    | [a, b, c, d, e] when b == add && d == eq 
        => charToInt(a) + charToInt(c) == charToInt(e);
    | [hd, ...tl] => checkArith(tl);
    | _ => false;
    };
};

let answer = checkArith(splitCode);
Js.log(answer);
