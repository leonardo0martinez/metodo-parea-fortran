module mod_analizador_lexico
    use mod_tokens
    implicit none
    private
    public :: escanear

contains
    subroutine escanear(entrada, listaTokens)
        character(len=*), intent(inout) :: entrada
        ! Variable que representa la lista de tokens
        type(token), dimension(:), allocatable, intent(out) :: listaTokens
        ! Variable que representa el estado actual
        integer :: i, estado, length
        character(len=1) :: c
        ! Variable que representa el lexema que actualmente se esta acumulando
        character(len=100) :: auxLex

        ! Inicialización
        length = len_trim(entrada)
        estado = 0
        auxLex = ""
        i = 0
        allocate(listaTokens(0)) ! Inicializar el array de tokens vacío

        ! Le agrego caracter de fin de cadena porque hay lexemas que aceptan con 
        ! el primer caracter del siguiente lexema y si este caracter no existe entonces
        ! perdemos el lexema
        entrada(length + 1: length + 1) = '#'
        length = length + 1

        ! Ciclo que recorre de izquierda a derecha caracter por caracter la cadena de entrada
        do while (i <= length)
            i = i + 1
            c = entrada(i:i)

            ! Select en el que cada caso representa cada uno de los estados del conjunto de estados
            select case (estado)
            case (0)
                if (c >= '0' .and. c <= '9') then
                    estado = 1
                    auxLex = trim(auxLex) // c
                else if (c == '+') then
                    auxLex = c
                    call addToken(SIGNO_MAS, auxLex, listaTokens)
                else if (c == '-') then
                    auxLex = c
                    call addToken(SIGNO_MEN, auxLex, listaTokens)
                else if (c == '*') then
                    auxLex = c
                    call addToken(SIGNO_POR, auxLex, listaTokens)
                else if (c == '/') then
                    auxLex = c
                    call addToken(SIGNO_DIV, auxLex, listaTokens)
                else if (c == '(') then
                    auxLex = c
                    call addToken(PARENTESIS_IZQ, auxLex, listaTokens)
                else if (c == ')') then
                    auxLex = c
                    call addToken(PARENTESIS_DER, auxLex, listaTokens)
                else if (c == '#' .and. i == length) then
                    print *, "Hemos concluido el analisis lexico satisfactoriamente"
                    exit
                else
                    print *, "Error lexico con: ", trim(c)
                    estado = 0
                end if
            case (1)
                if (c >= '0' .and. c <= '9') then
                    estado = 1
                    auxLex = trim(auxLex) // c
                else
                    call addToken(NUMERO, auxLex, listaTokens)
                    i = i - 1
                    estado = 0
                end if
            end select
        end do
    end subroutine escanear

    subroutine addToken(tipo, valor, listaTokens)
        integer, intent(in) :: tipo
        character(len=*), intent(inout) :: valor
        type(token), dimension(:), allocatable, intent(inout) :: listaTokens
        type(token) :: nuevoToken
        type(token), dimension(:), allocatable :: tempTokens
        integer :: tamanoActual

        nuevoToken%tipo = tipo
        nuevoToken%valor = valor

        tamanoActual = size(listaTokens)

        ! Redimensionar la lista de tokens para agregar uno nuevo
        allocate(tempTokens(tamanoActual + 1))

        ! Copiar los tokens actuales a la nueva lista
        if ( tamanoActual > 0 ) then
            tempTokens(1:tamanoActual) = listaTokens
        end if

        ! Añadir el token a la lista de tokens
        tempTokens(tamanoActual + 1) = nuevoToken

        ! Transferir la nueva lista a la lista de tokens
        call move_alloc(tempTokens, listaTokens)

        ! Limpieza
        valor = ""
    end subroutine addToken

end module mod_analizador_lexico
