(*
 - sml "C:\TEC CODIGO\Tarea2\print.sml"
 - use "C:\\TEC CODIGO\\Tarea2\\Verduleria.sml";
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


fun guardarEntxt linea = 
    let
        val archi = TextIO.openAppend "Catalogo.csv"
        val _ = TextIO.output (archi, linea ^ "\r\n") 
        val mensaje = print("La informacion ha sido guardada con exito\n")
    in 
        TextIO.closeOut
    end


fun limpiarRegistro () =
    let 
        val archi = TextIO.openOut "Catalogo.csv"
        val limpiar = TextIO.closeOut archi
    in 
        ()
    end



fun identificarCodigo codigo =
    let
        val archi = TextIO.openIn "Catalogo.csv"
        val done = ref false
        val lista : string list ref = ref []
        val _ = while (not (!done)) do (
            case TextIO.inputLine archi of
                SOME s => 
                    let 
                        val e = String.tokens (fn c => c = #",") s
                    in
                        if hd e = codigo then lista := e @ !lista
                        else
                            ()
                    end

                | NONE => done := true
        )
        val _ = TextIO.closeIn archi
    in 
        rev(!lista)
    end



fun main() =
    let 
        val output = concatenarEntrada "" 0
    in
        guardarEntxt output
         (*then print("La informacion a sido guardada en el catalogo\n") else print("Hubo un error en el flujo\n");*)
    end

val ini = main();


