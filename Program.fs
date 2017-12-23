printfn "решение квадратного уравнения на F#"

type ResultOfSolve=
    None
    |Linear of float
    |Quadratic of float*float
 
let solve(a:float, b:float, c:float):ResultOfSolve = 
    let D = b*b-4.0*a*c
    if a=0.0 then 
        if b=0.0 then None else Linear(-c/b)  
    else
        if D<0.0 then None else Quadratic(((-b+sqrt(D))/(2.0*a),(-b-sqrt(D))/(2.0*a))) 
 ///Вывод корней 

let PrintRoots(a:float, b:float, c:float):unit = 
    printf "Коэффициенты: a=%A, b=%A, c=%A. " a  b  c
    let root = solve(a,b,c)
    //Оператор сопоставления с образцом
    let textResult = 
        match root with 
        None -> "Корней нет"  
        | Linear(rt) -> "Линейное уравнение, корень " + (-c,b).ToString()
        | Quadratic(rt1,rt2) -> "Квадратное уравнение, 2 корня " + ((-b+sqrt(b*b-4.0*a*c))/(2.0*a)).ToString() + " и " + ((-b-sqrt(b*b-4.0*a*c))/(2.0*a)).ToString()
    printfn "%s" textResult

[<EntryPoint>]
let main argv = 
let rec readFloat() = 
    match System.Double.TryParse(System.Console.ReadLine()) with
    | false, _ -> printfn "введите значения"; readFloat()
    | _, x -> x
 
let a = readFloat()
let b = readFloat()
let c = readFloat()
solve(a,b,c)
PrintRoots(a,b,c)
0