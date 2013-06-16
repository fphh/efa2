
#set terminal pdf
#set output "interp.pdf"


load "tri.txt"
splot "./kennfeld.txt" with lines # , "./interpolation.txt" with lines