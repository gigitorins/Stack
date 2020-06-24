(*************************************************************************** SENSORI ***************************************************************************)

CS := Module[ {temp},

    temp = If[ Length[stack] > 0, Last[stack], Null ];
    temp
];


TB := Module[ {temp, indiceTB=1},
                
    indiceTB = IndiceBlock[stack, wanted];
    Which[ indiceTB === 1, temp = Null, True, temp = stack[[indiceTB-1]] ]; 
	temp
];


NN := Module[ {temp, indiceNN=1},
                
    indiceNN = IndiceBlock[stack, wanted];
    Which[  indiceNN > Length[wanted], temp = Null, True, temp = wanted[[indiceNN]] ];
    temp                
];



(*************************************************************************** COMANDI ***************************************************************************)

MS[value_] := Module[ {temp},
                
            If[ MemberQ[table,value],
                AppendTo[stack,value];
                table = Delete[ table, Position[table, value ][[1]] ];
                temp = value,
                temp = Null
            ];
    temp
];


MT[value_] := Module[ {temp},
                
            Which[  value === Null, temp = Null,
                    Length[stack] === 0, temp = Null,
                    True,
                    If[ MemberQ[stack,value],
                        AppendTo[table,stack[[-1]]];
                        stack = Delete[stack,-1];
                        temp = value,
                        temp = Null
                    ];                      
                ];
    temp
];


EQ[expr1_, expr2_] := Module[ {temp},
                       
    temp = If[ expr1 === expr2 , True, Null]; 
    temp
];


NOT[expr_] := Module[ {temp},
                    
    temp = If[ expr === Null , True, Null];
    temp
];


SetAttributes[DU, HoldAll];
maxciclenumber = 3 * Length[wanted];
maxciclenumberTOT = 4 * maxciclenumber;

DU[expr1_,expr2_] := Module[ {temp, counterDU = 0},

    While[  ReleaseHold[expr2] === Null && counterDU <= maxciclenumber && counterDUtot <= maxciclenumberTOT,
            ReleaseHold[expr1];
            counterDU++;
            counterDUtot++;
        ];

    temp = If[ counterDU > maxciclenumber || counterDUtot > maxciclenumberTOT, Null, True ];
    temp
];



(********************************************************************* COSTRUZIONE INDIVIDUO *********************************************************************)

sensori = {xCS, xTB, xNN};                              (* Sensori *)
movimenti = {xMS[arg], xMT[arg]};                       (* Movimenti *)
comandiTF = {xEQ[arg, arg], xNOT[arg], xDU[arg, arg]};  (* Comandi True/False per 2° argomento di DU *)
comandiDUarg1 = Join[movimenti, comandiTF];             (* Comandi per 1° argomento di DU *)
comandi = Join[sensori,comandiDUarg1];                  (* Comandi totali *)

(* Genero individui corretti sintatticamente contenenti solo comandi con la x davanti *)

generaIndividuo := Module[{temp, indinit, initdepth, inddepth = 4 (* Depth aggiuntiva degli individui da generare *)},
		
        indinit = comandi[[ Random[Integer, {1, Length[comandi]} ] ]]; (* Comando di partenza *)
        temp = Hold[Evaluate[indinit]];     (* Individuo iniziale, Hold davanti per bloccare la valutazione *)
        initdepth = Depth[indinit];         (* Depth iniziale intrinseca dell'individuo *)                    
		
        (* Genero il resto dell'individuo *)
        If[ MemberQ[temp, arg, Infinity],   temp = CompletamentoInd[temp, inddepth, initdepth] ];

		(* Individui contenenti arg sono scartati e generati nuovamente *)
		If[ MemberQ[temp, arg, Infinity],   temp = generaIndividuo ];

		temp
];

(* Funzione che sostituisce gli "arg" dentro un individuo con comandi sintatticamente corretti, serve in generaIndividuo e MutaIndividuo *)
CompletamentoInd[ind_, inddpt_, initialdepth_] := Module[{temp, Pos, pos, com, sommadepth},

    temp = ind;
    sommadepth = inddpt + initialdepth;

    While[  MemberQ[temp, arg, Infinity] && Depth[temp] <= sommadepth,
            
            Pos = Position[temp, arg];              (* Trovo posizioni degli arg *)
            pos = Pos[[1]] /. {x___,y_} -> {x,0};   (* Posizione head del primo arg *)

            (* Sostituisco gli arg rispettando la correttezza sintattica dei comandi*)
            Which[  MatchQ[ Part[temp, Sequence @@ pos], xEQ ],			com = comandi,
                    MemberQ[ {xMS, xMT}, Part[temp, Sequence @@ pos] ],	com = sensori,
                    MatchQ[ Part[temp, Sequence @@ pos], xNOT],			com = comandi,
                    Pos[[1]][[Length[Pos[[1]]]]] === 1,					com = comandiDUarg1, (* Assegna argomento 1 o 2 del DU *)
                    True,												com = comandiTF
                ];
					
            temp[[Sequence @@ Pos[[1]]]] = com[[ Random[Integer, {1, Length[com]} ]]];
        ];

    temp
];



(************************************************************************ ALTRE FUNZIONI ************************************************************************)

IndiceBlock[x_, y_] := Module[ {temp=1, list1, list2},

    list1 = x;
    list2 = y;
    While[ temp <= Length[x], If[ x[[temp]] === y[[temp]] , temp++, Break[] ] ];
	temp
];


regolesost = {xCS->Hold[CS], xTB->Hold[TB], xNN->Hold[NN], xNOT->NOT, xMT->MT, xMS->MS, xEQ->EQ, xDU->DU};

SostituisciComandi[x_] := Module[ {temp},   (* Sostituisce i comandi con la x con i comandi normali *)

    temp = x /. regolesost ;
    temp
];


TogliHold[x_] := Module[ {temp},    (* Rimuove gli Hold dall'individuo a tutti i livelli, l'individuo adesso agisce su stack a table *)

    temp = x;
    temp = Map[ReleaseHold, temp, {0, Infinity}];
    temp
];


Esegui[x_] := Module[ {temp},

    temp = x;
    temp = SostituisciComandi[temp];
    TogliHold[ temp ];
];



(*************************************************************************** FITNESS ***************************************************************************)

counterDUtot = 0;
nstacktest = 9;
corrstacks = {};

(* Stack e table di prova *)
listastack = {{l,a,u,n,e},{},{u,n,v,e},{s,i,r,l,a,u,n,e,v,e},{v,r,l},{u,n,i,v,e,r}};
listatable = {{s,i,r,v,e},{u,n,i,v,e,s,r,a,l,e},{i,r,s,a,l,e},{},{u,n,i,e,s,a,e},{s,a,l,e}};
Do[ stackRand;  AppendTo[listastack, stack]; AppendTo[listatable, table], {nstacktest} ];

Fitness[individuo_] := Module[ {temp, voto=1, listheads},

    temp = individuo;
    listheads = Map[Head, Level[temp, Infinity]];

    If[ Depth[temp] > 8 ||  Length[listasoluzioni] > 0,

        voto = 5. * Length [Union[listheads] ] * Length[listastack];, (* Penalizzo individui troppo lunghi, in questo modo sto già ottimizzando la soluzione e sto evitando loop *)

        (************************ Fitness ************************)
        corrstacks = {};
        voto += 5. * Length [Union[listheads] ] * Length[listastack]; (* Premio un patrimonio genetico vario *)
        voto += Total[ ( (provaFitness[temp,#])& /@ listastack ) ];		(* Calcolo il voto sugli stack *)
        
        (* Premio gli individui che costruiscono più stack contemporanee alla stessa velocità *)		
        voto += 20. * Total[corrstacks] / (Max[corrstacks] - Min[corrstacks] + 1);
		
        (* Se trovo la soluzione la aggiungo alla lista delle soluzioni, inoltre evito di valutare ulteriormente la fitness per la generazione corrente *)
        If[ Total[corrstacks] === Length[listastack] * Length[wanted],  temp = individuo;   AppendTo[listasoluzioni, temp] ];

    ];  (*********************************************************)

	voto
];


provaFitness[individuo_, stackprova_] := Module[ {temp, stackpos, votoprovv=0, indiceTCB=1},

    temp = individuo;       (* Ripristino temp *)
    counterDUtot = 0;       (* Ripristino counter DU totale *)
    stack = stackprova;
    stackpos = Position[listastack, stackprova][[1]];
    table = listatable[[ Sequence @@ stackpos ]];
						
    Esegui[temp];	                            (* Azione su stack e table *)
    indiceTCB = IndiceBlock[stack, wanted];    (* Controllo correttezza dello stack dopo l'azione dell'individuo *)
    AppendTo[corrstacks, indiceTCB-1];

    votoprovv += 10. * (indiceTCB-1);
    votoprovv
];



(******************************************************************** STACK E TABLE CASUALI ********************************************************************)

stackRand := Module[{lenstack},

    (* Genero la condizione iniziale casuale, estraggo lettere da wanted *)
    lenstack = Random[Integer, {0, Length[wanted]}];
    stack = RandomSampleZ[wanted, lenstack];
    table = DeleteCasesOnce[wanted, stack];
];

DeleteCasesOnce[list_List, cases_List] := Module[{countq},

    countq[_] := 0;
    Scan[(countq[#] = countq[#] + 1;) &, cases];
    #[[ Ordering[Random[] & /@ #] ]] & @ Reap[If[countq[#] === 0, Sow[#], countq[#] = countq[#] - 1] & /@ list][[2, 1]]
];

(* In alternativa a partire da Mathematica 6.0 si può usare RandomSample *)
RandomSampleZ[lis_List, num_] := Module[{len, selectfunc, ll, nn, aa},

    len = Length[lis];
    selectfunc[{ll_, nn_}] := {Drop[ll, {aa = Random[Integer, {1, Length[ll]}], aa}], nn - 1};
    #[[ Ordering[Random[] & /@ #] ]] & @ Complement[lis, First[Nest[selectfunc[#] &, {lis, num}, num]]]
];



(************************************************************************** CROSSOVER **************************************************************************)

(* Comandi senza arg per crossover *)
sensoriNA = {xCS, xTB, xNN};                        (* per MS, MT*)
movimentiNA = {xMS, xMT};                           (* Movimenti *)
comandiTFNA = {xEQ, xNOT, xDU};                     (* per 2° argomento di DU *)
comandiDUarg1NA = Join[movimentiNA, comandiTFNA];   (* per 1° argomento di DU *)
comandiNA = Join[sensoriNA, comandiDUarg1NA];       (* Comandi totali *)


Crossover[coppia_] := Module[ { i1, i2, risultato, listaparti1, listaparti2, swap, rndcomm1, rndcomm2,
                                 cmdpos1, cmdpos2, headcomm1, headcomm2, newi1, newi2 },
                                 
    i1 = coppia[[1]];
    i2 = coppia[[2]];

    If[ Random[] < pc,  (* Effettuo il crossover se il numero casuale è minore della prob. di crossover *)
 
        listaparti1 = Union[ Level[i1, {1, Infinity}] ];
        listaparti2 = Union[ Level[i2, {1, Infinity}] ];
        swap = False;

        While[ swap === False,

            (* Prendo una parte a caso 1 *)
            rndcomm1 = listaparti1[[ Random[Integer, {1, Length[listaparti1]}] ]];
            cmdpos1 = Position[i1, rndcomm1, {1, Infinity}, Heads->True];
            cmdpos1 = cmdpos1[[ Random[Integer, {1, Length[cmdpos1]}] ]];
            (* Vedo dentro che comando è contenuta la parte *)
            headcomm1 = i1[[Sequence @@ (cmdpos1 /. {x___,y_} -> {x,0}) ]];

            (* Prendo una parte a caso 2 *)
            rndcomm2 = listaparti2[[ Random[Integer, {1, Length[listaparti2]}] ]];
            cmdpos2 = Position[i2, rndcomm2, {1, Infinity}, Heads->True];
            cmdpos2 = cmdpos2[[ Random[Integer, {1, Length[cmdpos2]}] ]];
            (* Vedo dentro che comando è contenuta la parte *)
            headcomm2 = i2[[Sequence @@ (cmdpos2 /. {x___,y_} -> {x,0}) ]];

            (* Faccio swap solo se sintatticamente corretto *)
            Which[	headcomm1 === headcomm2 && Last[cmdpos1] === Last[cmdpos2], (* I comandi hanno stesse head e posizioni dentro le head *)
                    swap = True,
                    MemberQ[ sensoriNA, rndcomm1[[0]] ] && MemberQ[ sensoriNA, rndcomm2[[0]] ], (* Entrambi i comandi sono sensori *)
                    swap = True,
                    MemberQ[ movimentiNA, rndcomm1[[0]] ] && MemberQ[ movimentiNA, rndcomm2[[0]] ], (* Entrambi i comandi sono movimenti *)
                    swap = True,
                    MemberQ[ comandiTFNA, rndcomm1[[0]] ] && MemberQ[ comandiTFNA, rndcomm2[[0]] ], (* Entrambi i comandi sono comandiTF *)
                    swap = True,
                    (headcomm1 === xEQ || headcomm1 === xNOT || headcomm1 === Hold) && (headcomm2 === xEQ || headcomm2 === xNOT || headcomm2 === Hold), 
                    swap = True,        (* Le head sono Hold/xEQ/xNOT *)
                    headcomm1 === xDU && Last[cmdpos1] === 1 && MemberQ[ Join[movimentiNA, comandiTFNA], rndcomm2[[0]] ] && headcomm2 =!= xDU, 
                    swap = True,        (* Head1 è xDU, Head2 no, il comando1 è in pos 1 e il comando2 è compatibile *)
                    headcomm2 === xDU && Last[cmdpos2] === 1 && MemberQ[ Join[movimentiNA, comandiTFNA], rndcomm1[[0]] ] && headcomm1 =!= xDU,
                    swap = True,        (* Head2 è xDU, Head1 no, il comando2 è in pos 1 e il comando1 è compatibile *)
                    True, swap = False
				];
        ];

        (* Genero individui ibridi *)
        If[ Depth[i1] === 2,
            newi1 = Hold[ Evaluate[ Part[i2, Sequence @@ cmdpos2] ] ],
            newi1 = i1;
            newi1[[ Sequence @@ cmdpos1 ]] = Part[i2, Sequence @@ cmdpos2]
        ];

        If[ Depth[i2] === 2,
            newi2 = Hold[ Evaluate[ Part[i1, Sequence @@ cmdpos1] ] ],
            newi2 = i2;
            newi2[[ Sequence @@ cmdpos2 ]] = Part[i1, Sequence @@ cmdpos1]
        ];

        (* Se c'è crossover restituisco gli individui ibridi *)
        risultato = {newi1, newi2},

        (* Se non c'è crossover restituisco gli individui iniziali *)
        risultato = {i1, i2}
    ];  

    risultato
];



(************************************************************************** MUTAZIONE **************************************************************************)

Mutazione[individuo_]:= Module[ {temp},  (* Effettuo la mutazione se Random[] < probabilità di mutazione *)
		
    temp = individuo;
    temp = If[ Random[] < pm, MutaIndividuo[temp], temp ];
    temp
];


MutaIndividuo[individuo_]:= Module[ {temp, Pos, pos, rndcomm, cmdpos, cmdcont, cmdhead, commands, com, i, listapartitemp},
		
    temp = individuo;
    listapartitemp = Union[ Level[temp, {1, Infinity}] ];

    (* Scelgo un comando tra le parti, vedo la sua posizione e parte iniziale *)
    rndcomm = listapartitemp[[ Random[Integer, {1, Length[listapartitemp]}] ]];
    cmdpos = Position[temp, rndcomm, {1, Infinity}, Heads->True];
    cmdpos = cmdpos[[ Random[Integer, {1, Length[cmdpos]}] ]];
    cmdhead = rndcomm[[0]];
    cmdcont = cmdpos /. {x___,y_} -> {x,0};

    (* Vedo di che tipo è la parte iniziale *)
    Which[  temp[[ Sequence @@ cmdcont ]] === Hold, commands = comandi,
            MatchQ[Symbol,cmdhead], 				commands = sensori,
            MemberQ[movimentiNA,cmdhead],			commands = movimenti,
            MemberQ[comandiTFNA,cmdhead],			commands = comandiTF
        ];

    (***************** Mutazione *****************)
    temp[[ Sequence @@ cmdpos ]] = commands[[ Random[Integer, {1, Length[commands]} ]]];
    initdepth = Depth[temp];
    temp = CompletamentoInd[temp, 3, initdepth];

    (* Individui contenenti arg sono scartati e generati nuovamente *)
    If[ MemberQ[temp, arg, Infinity],   temp = MutaIndividuo[individuo] ];

	temp
];


(***************************************************************** GENERAZIONE DI POPOLAZIONI *****************************************************************)

PopIniziale := Table[ generaIndividuo , {Npop} ];


Voti[popolazione_] := Map[ Fitness, popolazione ];


Suddivisione[ voti_, criterio_] := Module[ {temp, voitnew, totalevoti, frazioni, suddivisione},

    Which[  criterio === FitnessProportionate,
            temp = voti;
            voitnew = Exp[temp/T];
            totalevoti = Total[voitnew];
            frazioni = voitnew/totalevoti;
            suddivisione = Table[ Sum[ frazioni[[j]], {j,1,i}] , {i,1,Npop} ],
            True, Print["Criterio non definito"]; Abort[]
        ];

    suddivisione
];


Ricombina[popolazione_] := Module[ {temp,figli},

    temp = Partition[popolazione, 2];
    figli = Map[Crossover, temp];
    figli = Flatten[figli,1];
    figli = Map[Mutazione, figli];
    figli
];


Generazione[popolazione_List] := Module[ {temp, votipop, intervallo, genitori, figli, rr, indice, meanvoti},

    counterT++;    
    If[ Mod[counterT,4] == 0,  T = T*0.5];
    votipop = Voti[popolazione];			
    meanvoti = Mean[votipop];
    intervallo = Suddivisione[votipop, FitnessProportionate];
    genitori = Table[   rr = Random[];
                        indice = Count[ intervallo, x_ /; x < rr ] + 1;
                        popolazione[[indice]],
                        {i,1,Npop}  ];
    figli = Ricombina[genitori];
    AppendTo[data, {N[meanvoti],Max[votipop]}];
    figli
];



(************************************************************************** RUN **************************************************************************)

(**** Parametri per le run ****)
Npop = 100;
Ngen = 200;
Tin = 1.;
counterTin = 0;
pc = 0.8;
pm = 0.15;
wanted = {u,n,i,v,e,r,s,a,l,e};    (* Parola desiderata *)
soluzionitotali = {};
data = {};


run := Module[ {pop, countgen=0},

    listasoluzioni = {};
    T = Tin; counterT = counterTin;
    pop = PopIniziale;

    (* Creo generazioni successive finchè non trovo la soluzione *)
    NestWhile[ (Generazione[#])&, pop, (Print["Generazione ",countgen += 1]; countgen < Ngen && Length[listasoluzioni] === 0) &];

    If[ Length[listasoluzioni] > 0, soluzionitotali = Join[soluzionitotali, {listasoluzioni[[1]]}] ];
    countgen += -1;
    countgen
];


printSoluzione[gennum_] := Module[ {temp, bestind},

    temp = gennum;
    bestind = listasoluzioni[[1]];
    Print["\n"];
    Print["Programma corretto trovato alla generazione ",temp," --- numero soluzioni = ", Length[listasoluzioni],"\n"];
    Print[bestind];
];



(*********************************************************************** STATISTICA ***********************************************************************)

(* Prova un individuo su una stack random, resituisce 1 se genera una stack corretta e 0 altrimenti *)
tryS[sol_] := Module[ {temp, ccc},

    temp = sol;
    counterDUtot = 0;		(* Ripristino counter DU totale *)
    stackRand;
    Esegui[temp];
    ccc = If[ stack === wanted, 1, 0];
    ccc
];


(* Prova un individuo sulla stack corrente, resituisce 1 se genera una stack corretta e 0 altrimenti *)
tryAS[sol_, coppiaL_] := Module[ {temp, ccc},

    temp = sol;
    stack = coppiaL[[1]];
    table = coppiaL[[2]];
    counterDUtot = 0;		(* Ripristino counter DU totale *)
    Esegui[temp];
    ccc = If[ stack === wanted, 1, 0];
    ccc
];


(* Prova tutte le soluzioni su stack random, resituisce la percentuale di stack corrette rispetto al numero di test *)
testcorrettezza := Module[ {ctest, numtest = 100},

    ctest = Plus @@ Table[ Plus @@ Map[ tryS, soluzionitotali ] ,{numtest}];
    N[ctest*100./(Length[soluzionitotali] * numtest)]
];


(* Prova tutte le soluzioni su stack random assegnate all'inizio, esporta su file la correttezza percentuale delle singole soluzioni *)
testcorrettezzaNEW := Module[ {ctest, numtest = 200},

    stacklist = {};
    Do[ stackRand;  AppendTo[stacklist, {stack,table}], {numtest} ];
    ctest = Table[ Plus @@ Map[ (tryAS[soluzionitotali[[ll]],#])&, stacklist], {ll,1,Length[soluzionitotali]} ];
    ctest = Map[ (#*100./numtest)& ,ctest];
    Export[ "a_correttezza.dat", ctest , "Table",  "FieldSeparators" -> " ", OverwriteTarget -> True];                      
];


(* Lancia #numrun run, calcola generazione media a cui la soluzione viene trovata, tempo medio delle run e correttezza media *)
runs := Module[ {numrun=100, listrun, timesrun={}, generations={}},

    testrun = Table[    listasoluzioni = {};    data = {};
                        Print["//------ RUN ",ll," ------//"];
                        listrun = AbsoluteTiming[ run ];
                        filename = "run"<>ToString[ll]<>".dat";
                        Export[ filename, data , "Table",  "FieldSeparators" -> " ", OverwriteTarget -> True];
                        Export[ "a_Soluzioni.dat", soluzionitotali , "Table",  "FieldSeparators" -> " ", OverwriteTarget -> True];
                        AppendTo[timesrun, Part[listrun,1] ];
                        AppendTo[generations, Part[listrun,2] ];
                        Export[ "a_times.dat", timesrun , "Table",  "FieldSeparators" -> " ", OverwriteTarget -> True];
                        Export[ "a_generations.dat", generations , "Table",  "FieldSeparators" -> " ", OverwriteTarget -> True];
                        listrun,
                        {ll,1,numrun} ];

    Print["Tempo medio run:\t\t", N[Mean[timesrun] ], " sec +- ", N[StandardDeviation[timesrun] ] ];
    Print["Generazione media:\t\t", N[Mean[generations] ], " +- ", N[StandardDeviation[generations] ] ];
    Print["Numero totale di soluzioni:\t", Length[soluzionitotali] ];
];


runs;
Print[ AbsoluteTiming[ testcorrettezzaNEW ][[1]] ];