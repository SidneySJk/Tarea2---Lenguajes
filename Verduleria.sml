(*
 - sml "C:\TEC CODIGO\Tarea2\print.sml"
 - use "C:\\TEC CODIGO\\Tarea2\\Verduleria.sml";
*)

(*Creador*)

fun validInt (s : string): bool = Option.isSome(Int.fromString s)
fun validReal (s : string): bool = Option.isSome(Real.fromString s)

fun validCode codigo = 
    let 
        val lista = List.map (fn c => Char.toString c) (String.explode codigo)
        val cola = tl lista

    in
        if hd lista = "F" andalso (List.all (fn c => validInt(c))(cola)) then true 
        else 
            false
    end


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
                    0 => 
                        if validCode texto then concatenarEntrada (texto ^ cadena) (ciclo+1) 
                        else
                            let
                                val a = print("Fomato de entrada invalida, ingrese una nueva\n")
                            in
                                concatenarEntrada cadena (ciclo)
                            end
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

fun mainC() =
    let 
        val _ = print("Elija alguna de las siguientes opciones: \n")
        val _ = print("1) Agregar producto al catalogo \n")
        val _ = print("2) Limpiar el catalogo \n")
        val _ = print("3) Volver atras\n")
        val opc = TextIO.inputLine TextIO.stdIn;
        val pasarString = (
                case opc of
                    SOME s => String.substring(s, 0, String.size s - 1)
                    | NONE => ""
        )
    in
        if pasarString = "3" then main()
        else 
            opcionesCreador pasarString 
    end


fun guardarEntxt linea = 
    let
        val archi = TextIO.openAppend "Catalogo.csv"
        val _ = TextIO.output (archi, linea ^ "\r\n") 
        val mensaje = print("La informacion ha sido guardada con exito\n")
        val _ = mainC()
    in 
        TextIO.closeOut
    end





fun limpiarRegistro () =
    let 
        val archi = TextIO.openOut "Catalogo.csv"
        val limpiar = TextIO.closeOut archi
        val _ = print("El registro fue limpiado con exito\n")
    in 
        mainC()
    end



fun redireccionarRegistro() = 
    let 
        val output = concatenarEntrada "" 0
    in
        guardarEntxt output
        
    end


fun opcionesCreador n = 
    case n of
        "1" => let val _ = redireccionarRegistro() in () end
        |"2" => let val _ = limpiarRegistro () in () end
        |_ => let val _ = mainC() in () end 
    



(*Analizador*)


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


fun identificarNombre nombre =
    let
        val archi = TextIO.openIn "Catalogo.csv"
        val done = ref false
        val lista : string list ref = ref []
        val _ = while (not (!done)) do (
            case TextIO.inputLine archi of
                SOME s => 
                    let 
                        val e = String.tokens (fn c => c = #",") s
                        val resto = case tl (rev e) of
                                        [] => ["Sin codigo", "Sin nombre", "Sin categoria"] 
                                        | x::xs => xs (*tl e;*)
                
                    in
                        if hd resto = nombre then lista := e @ !lista
                        else
                            ()
                    end

                | NONE => done := true
        )
        val _ = TextIO.closeIn archi
    in 
        !lista
    end


fun guardarDatos () =
    let
        val archi = TextIO.openIn "Catalogo.csv"
        val done = ref false
        val lista : string list list ref = ref []
        val _ = while (not (!done)) do (
            case TextIO.inputLine archi of
                SOME s => 
                    let 
                        val e = String.tokens (fn c => c = #",") s
                    in
                        lista := e :: !lista
                    end

                | NONE => done := true
        )
        val _ = TextIO.closeIn archi
    in 
        rev(!lista)
    end

fun comparar lista referencia : int = 
    let
        val inicial = hd lista
        val (contador, elemento) = foldl (fn (e, (cont, anterior)) => 
                                    if e <> anterior then (cont+1, e)
                                    else (cont, e) 
                                    ) (0, inicial) lista
    in 
        if contador > 3 then referencia else 0
    end
    

fun frutasDiferentes referencia =
    let 
        val contador = 0
        val lista = guardarDatos()
        val coincidencias = List.filter (fn sublista => List.nth (sublista, 2) = referencia) lista;
        val frutas = List.map ( fn e => List.nth (e, 1) ) coincidencias
    in
        comparar frutas referencia
    end

fun contarCant() = 
    let 
        val lista = guardarDatos()
        (*val contador = 0;*)
        val datos =  List.map ( fn c => frutasDiferentes(List.nth(c, 2))) lista
    in 
        datos
    end

fun ventas lista = 
    let
        val venta = case lista of 
                       (_::x::_::_::_) => x
                       | _ => "0"
    in 
        venta
    end


 
fun guardarNombre (archivo : string) : string = archivo

val archivoCSV = guardarNombre ("Catalogo")


fun mostrarVentas () = 
    let
        val _ = print("Ingrese el codigo o nombre del producto: \n")
        val dato = TextIO.inputLine TextIO.stdIn
        val pasarString = case dato of
                            SOME s => String.substring(s, 0, String.size s - 1)
                            | NONE => ""
        val lista = if validCode pasarString then identificarCodigo pasarString
                    else
                        identificarNombre pasarString
        val _ = 
        if lista <> [] then print("Ventas de " ^ pasarString ^ ": " ^ (ventas lista) ^ "\n")
        else 
            let 
                val _ = print("Producto no encontrado\n") 
            in  
                mostrarVentas()
            end
    in 
        mainA()

    end


fun opcionesAnalizador n =
    case n of
        "3" => let val _ = mostrarVentas () in () end
        | _ => ()

fun mainA() = 
    let 
        val _ = print("Ingrese alguna de las siguientes opciones: \n")
        val _ = print("1) Mostrar las frutas más populares \n")
        val _ = print("2) Familias con más de 4 frutas diferentes registradas \n")
        val _ = print("3) Buscar frutas por código o nombre \n")
        val _ = print("4) Cantidad de frutas por familia \n")
        val _ = print("5) Resumen general de la verdulería \n")
        val _ = print("6) Volver atras\n")
        val opc = TextIO.inputLine TextIO.stdIn;
        val pasarString = (
                case opc of
                    SOME s => String.substring(s, 0, String.size s - 1)
                    | NONE => ""   
        )    
                
    in
        if pasarString = "6" then main()
        else 
            opcionesAnalizador pasarString 
        
    end

fun main() =
    let 
        val _ = print("Ingrese alguna de las siguientes opciones: \n")
        val _ = print("1) Menu de creador\n")
        val _ = print("2) Menu de Analizador \n")
        val opc = TextIO.inputLine TextIO.stdIn;
        val pasarString = case opc of
                            SOME s => String.substring(s, 0, String.size s - 1)
                            | NONE => ""
    in
        case pasarString of
            "1" => mainC()
            |"2" => mainA()
            | _ => main()
    end

val ini = main();






