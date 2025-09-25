(*
 - sml "C:\TEC CODIGO\Tarea2\print.sml"
 - use "C:\\TEC CODIGO\\Tarea2\\print.sml";
*)

(*Creador*)


fun validInt (s : string): bool = Option.isSome(Int.fromString s)
fun validReal (s : string): bool = Option.isSome(Real.fromString s)


fun concatenarEntrada (cadena : string) (ciclo : int) = 
    let
    val _ = print("Ingrese informacion del producto: \n")
    val linea = TextIO.inputLine TextIO.stdIn;
    val info = Option.getOpt (linea, "")
    val texto = String.implode (List.filter (fn c => c <> #"\n" ) (String.explode info))
    val resultado = case texto of
        "" =>
            let
                val _ = print("Entrada vacia invalida, ingrese una nueva\n")
            in
                concatenarEntrada cadena (ciclo)
            end
        | _ => 
                case ciclo of 
                    0 =>  concatenarEntrada (texto ^ cadena) (ciclo+1)
                    | 3 => 
                        if validInt texto then
                            concatenarEntrada (cadena ^ ", " ^ texto) (ciclo+1)
                        else  
                                let
                                    val a = print("Entrada no decimal invalida, ingrese una nueva\n")
                                in
                                    concatenarEntrada cadena (ciclo)
                                end
                    | 4 => 
                        if validReal texto then
                            concatenarEntrada (cadena ^ ", " ^ texto) (ciclo+1)
                        else  
                                let
                                    val a = print("Entrada no decimal invalida, ingrese una nueva\n")
                                in
                                    concatenarEntrada cadena (ciclo) 
                                end
                    | 5 => cadena
                    | _ => concatenarEntrada (cadena ^ ", " ^ texto) (ciclo+1)
    in
        resultado
    end

fun guardarEntxt linea = (*https://stackoverflow.com/questions/66257455/how-to-read-a-file-in-sml-line-by-line*)
    let
        val archi = TextIO.openIn "Catalogo.csv"
        val done = ref false
    in 
        while not (!done) do
            case TextIO.inputLine archi of
                NONE =>
                    let 
                        TextIO.output (archi, linea) 
                    in 
                        done := true
                    end
                | SOME s => TextIO.output (archi, s)
        
    end

fun main i =
    let 
        val output = concatenarEntrada "" 0
        val mostrar = guardarEntxt output
    in
        mostrar
         (*then print("La informacion a sido guardada en el catalogo\n") else print("Hubo un error en el flujo\n");*)
    end

val ini = main 0;