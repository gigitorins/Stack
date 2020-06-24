maxciclenumber = 3 * Length[wanted];
maxciclenumberTOT = 4 * maxciclenumber; (countDUtot attivo)

(* Stack e table di prova *)
listastack = {{l,a,u,n,e},{},{u,n,v,e},{s,i,r,l,a,u,n,e,v,e},{v,r,l},{u,n,i,v,e,r}};
listatable = {{s,i,r,v,e},{u,n,i,v,e,s,r,a,l,e},{i,r,s,a,l,e},{},{u,n,i,e,s,a,e},{s,a,l,e}};
Do[ stackRand;  AppendTo[listastack, stack]; AppendTo[listatable, table], {4} ];

Fitness:
- Depth max = 8, no limite comandi
- voto individui troppo lunghi      =   5. * Length [Union[listheads] ] * Length[listastack]
- voto+ patrimonio                  =   5. * Length [Union[listheads] ] * Length[listastack]
- voto+ correttezza singolo stack   =   10. * (indiceTCB-1);
- voto+ correttezza parallela       =   20. * Total[corrstacks] / (Max[corrstacks] - Min[corrstacks] + 1);		

Npop = 150      --> Ngen = 200;
T iniziale = 1. --> T = T*0.5 ogni 4 generazioni
pc = 0.8;
pm = 0.15;