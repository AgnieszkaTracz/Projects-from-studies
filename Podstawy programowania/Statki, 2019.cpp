#include <stdio.h>



int main(){
	
	int wielkosc_planszy=5;
	int max_wielk_stat=4;
	int plansza[10][10][2]; //2-dla graczy na pol

	int i; 
	int ilosc_pol_statkow[2]={0,0};
	int statek[4];
	for(i=0;i<max_wielk_stat;i++)
	{
		printf("Wybierz ilosc %d -ek: ",i+1);
		scanf("%d",&statek[i]);
		
		ilosc_pol_statkow[0]=ilosc_pol_statkow[0]+(statek[i]*(i+1));
		ilosc_pol_statkow[1]=ilosc_pol_statkow[1]+(statek[i]*(i+1));
	}	

	//dla 1 gracza
	for(i=0;i<2;i++)
	{
		int j;
		for(j=0;j<wielkosc_planszy;j++)
		{
			int k;
			for(k=0;k<wielkosc_planszy;k++)
			{
				plansza[j][k][i]=0;
			}
		}

		int m;
		for(m=(max_wielk_stat-1);m>=0;m--) //m-wielkosc statku  statek[m] -ilosc statkow
		{
			int n;
			for(n=0;n<statek[m];n++){  //n-statek z kolei
				//-----------------------------------------------------------------------------
				int aa, bb, kk;
				for(; ;){

					int czybreak=1;
					printf("Podaj polozenie %d z kolei %d -ki:\n (x-lewa krawedz kwadratu, y-gorna krawedz kwadratu, k-wybierz:",n +1,m+1);
					printf("\n 1-Dol\n 2-Gora\n 3-Lewo\n 4-Prawo\n");
					
					scanf("%d%d%d",&aa ,&bb ,&kk);
					aa=aa-1;		//ulatwienie dla uzytkownika
					bb=bb-1;
					
				if((aa< wielkosc_planszy) && (bb< wielkosc_planszy) && (aa>=0) && (bb>=0)){
					int t;
					for(t=0;t<=m;t++){
						printf("\nObrot %d petli III wartosc czybreak %d\n", t, czybreak);
						if(kk==1){
							if(plansza[aa+t][bb][i]!=0){
								czybreak=0;
							}
							if((aa+t)>= wielkosc_planszy){
								czybreak=0;							
							} 
							
							
						}
						else if(kk==2){
								if(plansza[aa-t][bb][i]!=0){
								czybreak=0;
							}
							if((aa-t)<0){
								czybreak=0;							
							} 
						}
						else if(kk==3){
							
							if(plansza[aa][bb-t][i]!=0){
								czybreak=0;
								printf("\nWyjscie z powody statku\n");
							}
							if((bb-t)<0){
								czybreak=0;	
								printf("\nWyjscie z powodu przekroczenia tablicy\n");						
							} 
						}
						else if(kk==4){
								if(plansza[aa][bb+t][i]!=0){
								czybreak=0;
							}
							if((bb+t)>= wielkosc_planszy){
								czybreak=0;							
							} 
						}
						else{ 
						czybreak=0;
						
						}
					}
				}
				else{ 
					czybreak=0;
				}
				
				if(czybreak==1)
				{
					break;
				}
				printf("\nBledne dane\n");
				
				} // wyjscie z petli sprawdzajacej
				//petla rysujaca
					int t;
					for(t=0;t<=m;t++){
						if(kk==1){
							plansza[aa+t][bb][i]=1;
						}
							
						else if(kk==2){
								plansza[aa-t][bb][i]=1;													
						} 
					
						else if(kk==3){
							plansza[aa][bb-t][i]=1;
								
						}
						else if(kk==4){
								plansza[aa][bb+t][i]=1;
								
						}
					}
			int dl,sr;
			printf("  ");
			for(dl=0;dl<wielkosc_planszy;dl=dl+1){
				printf("%d ",dl+1);
			}
			printf("(y)");
			for(dl=0;dl<wielkosc_planszy;dl++){
				printf("\n%d ",dl+1);
				for(sr=0;sr<wielkosc_planszy;sr++){
					printf("%d ", plansza[dl][sr][i]);
				}
			}
			printf("\n(x)");
			/*for(dl=0;dl<wielkosc_planszy;dl++){
				printf("\n");
				for(sr=0;sr<wielkosc_planszy;sr++){
					if(plansza[dl][sr][i]==1){
						printf("X ");}
					if(plansza[dl][sr][i]==0){
						printf("X ");}
					}
				}*/
			printf("\n");
		//-----------------------------------------------------------------------------------
			}
		}
		printf("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n 	kolejny gracz: \n");		
	}
		int g=1;
		
		for(;ilosc_pol_statkow[1-g]!=0;){
		printf("\nTura gracz nr. %d: \n",2-g);
		int s_1,s_2;
		printf("Podaj kordy strzalu: (x-lewa krawedz kwadratu, y-gorna krawedz kwadratu) ");
		scanf("%d%d",&s_1,&s_2);
		s_1=s_1-1;
		s_2=s_2-1;
		if((s_1>=0) && (s_1<wielkosc_planszy) && (s_2>=0) && (s_2<wielkosc_planszy)){
			if(plansza[s_1][s_2][g]==1){
				printf("Trafiony\n");
				plansza[s_1][s_2][g]=3;
				ilosc_pol_statkow[1-g]=ilosc_pol_statkow[1-g]-1;
			}
			else if(plansza[s_1][s_2][g]==0){
				printf("Pudlo\n");
				plansza[s_1][s_2][g]=2;
			}
			else {
				printf("Juz strzelales/as");
			}
			
		}
		else {
			printf("Zle");
		}
		//_______________________________________________________________________________
		
		
		
		int dl,sr;
		printf("  ");
		for(dl=0;dl<wielkosc_planszy;dl=dl+1){
				printf("%d ",dl+1);
			}
			printf("(y)");
		for(dl=0;dl<wielkosc_planszy;dl++){
				printf("\n%d ",dl+1);
				for(sr=0;sr<wielkosc_planszy;sr++){
					if(plansza[dl][sr][g]==1){
						printf("X ");}
					if(plansza[dl][sr][g]==0){
						printf("X ");}
					if(plansza[dl][sr][g]==2){
						printf("O ");}
					if(plansza[dl][sr][g]==3){
						printf("I ");}		
					}
				}
				printf("\n(x)");	
		
		if(	plansza[s_1][s_2][g]==2){
			//printf("Przejscie do nowego gracza: %d\n", g);
			if(g==1){
				g=0;
			}
			else if(g==0){
				g=1;
			}
			//printf("Wypisanie g: %d\n", g);
		}
		else if(plansza[s_1][s_2][g]==3){
			
		}
		else{
			printf("Powtorz strzal: \n");
		}		
			
			
		}
	printf("\nWygral gracz nr. %d",2-g);	//2-g bo na koncu petli zawsze zmienia gracza
	
	
	
	
return 0;
}
