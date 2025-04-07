##problem1##
A=matrix(1:12,4,3)
indices=A[,2]>5
mean(A[indices,3])
##11

##problem2##
##b##total points
##a##half points

##problem3##
##sol1##
x=1:4
#y=x^2/2
ind_2=which(x>2)
y=sum(x[ind_2]^2)
sum(y)
##sol2##
i=0;y=0;
while(i<4){i=i+1;if(i>2){y=y+i^2}}
print(y)
##25
##sol3##
y=0;
for(i in 1:4){if(i>2){y=y+i^2}}
##25
