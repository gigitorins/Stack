maxciclenumber = 3 * Length[wanted];
maxciclenumberTOT = 4 * maxciclenumber; (countDUtot attivo)

(* Stack e table di prova *)
listastack = {{l,a,u,n,e},{},{u,n,v,e},{s,i,r,l,a,u,n,e,v,e},{v,r,l},{u,n,i,v,e,r},{e,r,n},{a,u,i,r},{a,i,v},{i,n,r,u,v,a,l,e},{l,v,i},{e,r,l,u,n,s,i,a},{s,a},{s,r,u,v,e,a,l,i,n},{}};
listatable = {{s,i,r,v,e},{u,n,i,v,e,s,r,a,l,e},{i,r,s,a,l,e},{},{u,n,i,e,s,a,e},{s,a,l,e},{e,a,u,v,i,s,l},{e,e,n,l,s,v},{r,u,s,e,n,e,l},{e,s},{e,n,a,s,r,e,u},{e,v},{n,r,e,l,v,e,i,u},{e},{s,e,l,u,r,a,v,n,e,i}};

Fitness:
- Depth max = 8, no limite comandi
- voto individui troppo lunghi      =   5. * Length [Union[listheads] ] * Length[listastack]
- voto+ patrimonio                  =   5. * Length [Union[listheads] ] * Length[listastack]
- voto+ correttezza singolo stack   =   10. * (indiceTCB-1);
- voto+ correttezza parallela       =   20. * Total[corrstacks] / (Max[corrstacks] - Min[corrstacks] + 1);		

Npop = 100      --> Ngen = 200;
T iniziale = 1. --> T = T*0.5 ogni 4 generazioni
pc = 0.8;
pm = 0.15;