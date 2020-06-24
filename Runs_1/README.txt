countDUtot attivo
maxciclenumber = 3 * Length[wanted];
maxciclenumberTOT = 4 * maxciclenumber;

(* Stack e table di prova *)
listastack = {{l,a,u,n,e},{},{u,n,v,e},{s,i,r,l,a,u,n,e,v,e},{v,r,l},{u,n,i,v,e,r}};
listatable = {{s,i,r,v,e},{u,n,i,v,e,s,r,a,l,e},{i,r,s,a,l,e},{},{u,n,i,e,s,a,e},{s,a,l,e}};
Do[ stackRand;  AppendTo[listastack, stack]; AppendTo[listatable, table], {14} ];

Fitness:
- Depth max = 8
- no limite comandi
- voto individui troppo lunghi      =   10.
- voto+ patrimonio                  =   50. * Length [Union[listheads] ] * Length[listastack]
- voto+ correttezza singolo stack   =   100. * (indiceTCB-1);
- voto+ correttezza parallela       =   200. * Total[corrstacks] / (Max[corrstacks] - Min[corrstacks] + 1);		

Limite sulla lunghezza delle run
Npop = 200;
Ngen = 200;
pc = 0.8;
pm = 0.15;


// ----------------------------- RISULTATI ----------------------------- //

Numero runs:                    100

