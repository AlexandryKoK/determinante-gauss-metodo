program determinant

!Problema - Determinante de uma matriz n x n utilizando metodo de eliminação de Gauss

integer*8 :: n,h,g,p
double precision :: e, r, f
double precision, dimension(100,100) :: matrix
double precision, dimension(100,100) :: reduced
real*8 :: det = 1

write(*,*) "Bem vindo ao programa de determinante usando metodo de eliminacao de Gauss."
write(*,*) 
write(*,*) "Digite a ordem da sua matriz (numero inteiro):"
read (*,*) n

do i = 1, n
	do j = 1, n
		write(*,*) "Digite o",i, "elemento da",j, "coluna"
		read(*,*) matrix(i,j)
	end do
end do


!Clonamos a matriz
do h = 1, n
do i = 1, n
	do g = 1, n
	do j = 1, n
		if (h == i .and. g == j ) then
            reduced(h,g) = matrix(i,j) 
		end if
	end do
end do
end do
end do


!p indica a qual linha e coluna devemos comecar nosso calculo
do p =1,n

!a cada linha e coluna ele faz o algoritmo que zera a coluna abaixo do termo diagonal i=j 
!troca o p e ele zera todos abaixo da diagonal

do h = p+1, n
do i = p+1, n
	do g = p, n
	do j = p, n
		if (h == i .and. g == j .and. matrix(i,j) /= 0 ) then 
            reduced(h,g) = matrix(i,j) - matrix(i,p) * matrix(p,j) / matrix(p,p)
        end if
	end do
end do
end do
end do

!igualamos os termos da reduced com a matrix para que no proximo p ele faça o calculo com os novos valores

do h = 1, n
do i = 1, n
	do g = 1, n
	do j = 1, n
		if (h == i .and. g == j ) then
             matrix(i,j) = reduced(h,g)
		end if
	end do
end do
end do
end do

end do


write(*,*)
write(*,*)

!calculamos o determinante a seguir

do h = 1, n
do i = 1, n
	do g = 1, n
	do j = 1, n
		if (h == i .and. g == j .and. h == g) then
             det = det * reduced(i,j)
		end if
	end do
end do
end do
end do



write(*,*) "Determinante =", det


end program Determinant