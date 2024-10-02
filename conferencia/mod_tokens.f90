module mod_tokens
    implicit none
    private
    public :: token, NUMERO, SIGNO_MAS, SIGNO_MEN, SIGNO_POR, SIGNO_DIV, PARENTESIS_IZQ, PARENTESIS_DER

    type :: token
        character(len=100) :: valor
        integer :: tipo
    end type token

    integer, parameter :: NUMERO = 1
    integer, parameter :: SIGNO_MAS = 2
    integer, parameter :: SIGNO_MEN = 3
    integer, parameter :: SIGNO_POR = 4
    integer, parameter :: SIGNO_DIV = 5
    integer, parameter :: PARENTESIS_IZQ = 6
    integer, parameter :: PARENTESIS_DER = 7

end module mod_tokens
