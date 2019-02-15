using namespace std;
#include <iostream>
#include <conio.h>
#include <math.h>
int main()
{
	int n,d,i,j;
	int itera = 0;
	i= 0; j=0;
	n = 73;
	while( n > 0)
	{

		d = n % 2;
		i++;
		n = n/2;
		j++;
		cout<<"n ="<<n<<endl;
		cout<<"d ="<<d<<endl;
		itera++;
	}

	cout<<"Numero de iteraciones: "<< itera <<endl;
}
