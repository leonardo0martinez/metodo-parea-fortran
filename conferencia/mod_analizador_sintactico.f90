module mod_analizador_sintactico
    use mod_tokens
    implicit none
    private
    public :: parsear

    ! Lista de Tokens que el parser recibe del analizador lexico
    type(token), dimension(:), allocatable :: listaTokens
    ! Variable que se usa para recorrer la lista de tokens
    integer :: numPreanalisis
    ! Variable que representa el caracter de anticipación que posee el parser para realizar el analisis
    type(token) :: preanalisis

    ! Lo primero que se tiene que hacer es generar una gramatica que no sea ambigua
    ! Vamos a utilizar la gramatica de expresiones aritmeticas que respeta la precedencia de operadores
    ! La gramatica es la siguiente:
    ! E -> E + T
    ! E -> E - T
    ! E -> T
    ! T -> T * F
    ! T -> T / F
    ! T -> F
    ! F -> ( E )
    ! F -> NUMERO

    ! Esta gramatica es recursiva por la izquierda, pero para implementarla necesitamos
    ! que la gramatica no tenga recursividad por la izquierda, entonces la transformamos
    ! con algunas reglas:

    ! Gramatica sin recursividad por la izquierda
    ! NOTA: $ representa EPSILON o VACIO

    ! E -> T EP
    ! EP -> + T EP
    ! EP -> - T EP
    ! EP -> $
    ! T -> F TP
    ! TP -> * F TP
    ! TP -> / F TP
    ! TP -> $
    ! F -> ( E )
    ! F -> NUMERO

contains

    ! Para cada no terminal del lado izquierdo de las producciones, se crea una subrutina
    ! Para cada no terminal del lado derecho de las producciones, se hace una llamada
    ! a la subrutina correspondiente, y para cada terminal del lado derecho se hace una
    ! llamada a la subrutina match enviando como parametro el terminal correspondiente.
    subroutine parsear(tokens)
        type(token), dimension(:), intent(in) :: tokens
        listaTokens = tokens
        numPreanalisis = 1
        preanalisis = listaTokens(numPreanalisis)
        call E()
    end subroutine parsear

    subroutine E()
        call T()
        call EP()
    end subroutine E

    recursive subroutine EP()
        if (preanalisis%tipo == SIGNO_MAS) then
            call match(SIGNO_MAS)
            call T()
            call EP()
        else if (preanalisis%tipo == SIGNO_MEN) then
            call match(SIGNO_MEN)
            call T()
            call EP()
        end if
    end subroutine EP

    subroutine T()
        call F()
        call TP()
    end subroutine T

    recursive subroutine TP()
        if (preanalisis%tipo == SIGNO_POR) then
            call match(SIGNO_POR)
            call F()
            call TP()
        else if (preanalisis%tipo == SIGNO_DIV) then
            call match(SIGNO_DIV)
            call F()
            call TP()
        end if
    end subroutine TP

    subroutine F()
        if (preanalisis%tipo == PARENTESIS_IZQ) then
            call match(PARENTESIS_IZQ)
            call E()
            call match(PARENTESIS_DER)
        else
            call match(NUMERO)
        end if
    end subroutine F

    subroutine match(p)
        integer, intent(in) :: p
        if (preanalisis%tipo /= p) then
            print *, "Error de sintaxis: Se esperaba ", getTipoTokenError(p)
            stop
        end if
        if (numPreanalisis < size(listaTokens)) then
            numPreanalisis = numPreanalisis + 1
            preanalisis = listaTokens(numPreanalisis)
        end if
    end subroutine match

    function getTipoTokenError(p) result(res)
        integer, intent(in) :: p
        character(len=11) :: res

        res = ""
        
        select case (p)
        case (SIGNO_MAS)
            res = "+"
        case (SIGNO_MEN)
            res = "-"
        case (SIGNO_POR)
            res = "*"
        case (SIGNO_DIV)
            res = "/"
        case (PARENTESIS_IZQ)
            res = "("
        case (PARENTESIS_DER)
            res = ")"
        case (NUMERO)
            res = "NUMERO"
        case default
            res = "Desconocido"
        end select
    end function getTipoTokenError

end module mod_analizador_sintactico
