PROGRAM EulerSimple_v2

! Programa para resolver una EDO-PVI
! por el Método de Euler Simple
! Versión 2
! Autor: Francisco Lizarralde - 2020
! Todo en el mismo programa
! Resultados sólo por pantalla
! Graba los resultados en un archivo
! Grafica los resultados

INTEGER, PARAMETER :: cant_ec=1

REAL(8), DIMENSION(0: cant_ec) :: v, vi
INTEGER iter, max_iter
REAL(8) h

! Valores iniciales
h = 0.05
max_iter = 80

vi(0) = 1.0
vi(1) = 1.0

! Abro un archivo para guardar los resultados
  OPEN(2, FILE='euler.txt')

  WRITE(*, '(2F10.6)') vi
  WRITE(2, '(2F10.6)') vi
  
  v = vi
  
  DO iter = 1, max_iter
  
    ! Método de Euler Simple
    v = v + h*v_prima( v )
    
    WRITE(*, '(2F10.6)') v
    WRITE(2, '(2F10.6)') v
    
  END DO
  
! Cierro el archivo
  CLOSE(2)
  
! Grafico los resultados
  CALL SYSTEM ("gnuplot -persist 'script_edo_pvi'")
  
  CONTAINS
  
FUNCTION v_prima( v )
! Definición de la EDO
REAL(8), DIMENSION(0: cant_ec) :: v, v_prima

  v_prima(0) = 1.0
  v_prima(1) = v(0)*v(1)**(1/3.0)
  
END FUNCTION

END PROGRAM
