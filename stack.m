(*************************************************************************** SENSORI ***************************************************************************)


CS := Module[ {temp},
                
		    	If[	Length[stack] > 0,
                    temp = Last[stack],
				    temp = Null
			    ];  temp
        ];


TB := Module[ {temp, indiceTB=1},
                
                While[indiceTB <= Length[stack],
                        If[ stack[[indiceTB]] === wanted[[indiceTB]] , indiceTB++, Break[] ]
                    ];

                Which[ indiceTB === 1, temp = Null,
                        True, temp = stack[[indiceTB-1]]                        
                    ]; 
            temp
	    ];


NN := Module[ {temp, indiceNN=1},
                
                While[indiceNN <= Length[stack],
                        If[ stack[[indiceNN]] === wanted[[indiceNN]] , indiceNN++, Break[] ]
                    ];

                Which[  indiceNN > Length[wanted], temp = Null,
                        True, temp = wanted[[indiceNN]]                        
                     ];
                temp                
    	];


(*************************************************************************** COMANDI ***************************************************************************)

MS[value_] := Module[ {temp},
                
                If[ MemberQ[table,value],
                    counter += 1;
                    AppendTo[stack,value];
                    table = Delete[table, Position[table, value ][[1]] ];
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
                            counter += 1;
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
counterDUtot = 0;

DU[expr1_,expr2_] := Module[ {temp, counterDU, maxciclenumber, maxciclenumberTOT},
                        
                        counterDU = 0;
                        maxciclenumber = 3 * Length[wanted];
                        maxciclenumberTOT = 10 * maxciclenumber;
                        While[ ReleaseHold[expr2] === Null && counterDU <= maxciclenumber && counterDUtot <= maxciclenumberTOT,
                                ReleaseHold[expr1];
                                counterDU++;
                                counterDUtot++;
                            ];

                        If[ counterDU > maxciclenumber || counterDUtot > maxciclenumberTOT,
                            temp = Null,
                            temp = True
                        ];
            temp
        ];



(********************************************************************* COSTRUZIONE INDIVIDUO *********************************************************************)

sensori = {xCS, xTB, xNN};                              (* per MS, MT*)
movimenti = {xMS[arg], xMT[arg]};                       (* Movimenti *)
comandiTF = {xEQ[arg, arg], xNOT[arg], xDU[arg, arg]};  (* per 2° argomento di DU *)
comandiDUarg1 = Join[movimenti, comandiTF];             (* per 1° argomento di DU *)
comandi = Join[sensori,comandiDUarg1];                  (* Comandi totali *)

(* Genero individui corretti sintatticamente contenenti solo comandi con la x davanti *)

inddepth = 4;       (* Depth iniziale degli individui generati *)

generaIndividuo := Module[{temp, Pos, pos, com, i, indinit, initdepth},
		
        indinit = comandi[[ Random[Integer, {1, Length[comandi]} ] ]]; (* Individuo iniziale, Hold davanti per bloccare la valutazione *)
        initdepth = Depth[indinit];
		temp = Hold[Evaluate[indinit]];
		
		While[MemberQ[temp, arg, Infinity] && Depth[temp] <= (inddepth+initdepth),

            (* Trovo posizioni degli arg *)
			Pos = Position[temp, arg];

            (* Sostituisco gli arg *)
			For[i=1, i <= Length[Pos], i++,
				pos = Pos[[i]];
				pos[[-1]] = 0;

				(* Correttezza sintattica dei comandi *)
                Which[  MatchQ[ Part[temp, Sequence @@ pos], xEQ ],
					    com = comandi,
                        MemberQ[ {xMS, xMT}, Part[temp, Sequence @@ pos] ],
						com = sensori,
                        MatchQ[ Part[temp, Sequence @@ pos], xNOT],
						com = comandi,
                        (* Assegna argomento 1 e 2 del DU secondo la correttezza sintattica *)
						Pos[[i]][[Length[Pos[[i]]]]] === 1,
						com = comandiDUarg1,
						True,
                        com = comandiTF
                    ];

				temp[[Sequence @@ Pos[[i]]]] = com[[ Random[Integer, {1, Length[com]} ]]];
			];

		];

		(* Individui contenenti arg sono scartati e generati nuovamente *)
		If[ MemberQ[temp, arg, Infinity],   temp = generaIndividuo  ];

		temp

	];



(************************************************************************ ALTRE FUNZIONI ************************************************************************)


SostituisciComandi[x_] := Module[ {temp, regolesost},   (* Sostituisce i comandi con la x con i comandi normali *)

                    regolesost = {xCS->Hold[CS], xTB->Hold[TB], xNN->Hold[NN], xNOT->NOT, xMT->MT, xMS->MS, xEQ->EQ, xDU->DU};
                    temp = x /. regolesost ;
                    temp
        ];


TogliHold[x_] := Module[ {temp},    (* Rimuove gli Hold dall'individuo a tutti i livelli, l'individuo adesso agisce su stack a table *)
                    temp = x;
                    temp = Map[ReleaseHold, temp, {0, Infinity}];
                    temp
        ];


(*************************************************************************** FITNESS ***************************************************************************)

counter = 0;
Fitness[individuo_] :=  Module[ {temp, voto=1, nnn=1, counterpatr=0, indiceTCB=1, contoDU, contoEQ, contoNOT},

            temp = individuo;
            contoDU = Position[ temp, xDU, {0, Infinity} ];
            contoEQ = Position[ temp, xEQ, {0, Infinity} ];
            contoNOT = Position[ temp, xNOT, {0, Infinity} ];

            If[ Depth[temp] > 10 || Length[contoDU] > 6 || Length[contoEQ] > 8 || Length[contoNOT] > 8 ,

                (* Penalizzo individui troppo lunghi, con troppi DU o EQ o NOT *)
                voto = 100,

                (* Premio un patrimonio genetico vario *)
                While[  nnn <= Length[comandiNA],
                        If[ MemberQ[temp, comandiNA[[ nnn ]], {0,Infinity}], counterpatr += 1; ];
                        nnn += 1;   ];
                voto += counterpatr;       

                (* Valuto l'azione dell'individuo su stack e table e premio gli individui che fanno più mosse *)
                temp = SostituisciComandi[temp];
                TogliHold[ temp ];
                voto += counter;

                (* Salvo individui che generano uno stack corretto *)
                While[  indiceTCB <= Length[stack],
                        If[ stack[[indiceTCB]] === wanted[[indiceTCB]] , indiceTCB++, Break[] ]
                    ];

                If[ (indiceTCB - 1) === Length[wanted],
                    temp = individuo;
                    AppendTo[listasoluzioni, temp];
                ];

                (* Boost alla fitness per ogni lettera corretta nello stack *)
                voto += (indiceTCB - 1)*1000;

                (* Operazioni per la prossima valutazione della fitness *)
                indiceTCB = 1;          (* Ripristino indice *)
                counter = 0;            (* Ripristino counter *)
                counterpatr = 0;        (* Ripristino counter patrimonio genetico *)
                counterDUtot = 0;       (* Ripristino counter DU totale *)
                stack = stackiniziale;    (* Ripristino stack *)
                table = tableiniziale;    (* Ripristino table *)

            ];
            
        voto
    ];


(***************************************************************** GENERAZIONE DI POPOLAZIONI *****************************************************************)

PopIniziale := Table[ generaIndividuo , {Npop} ];


Voti[popolazione_] := Map[ Fitness, popolazione ];


Suddivisione[ voti_, criterio_, votimean_, votistd_] := Module[ {temp, totalevoti, votiscaled, frazioni, suddivisione},

					Which[  criterio === FitnessProportionate,
					        totalevoti = Plus @@ voti;
					        frazioni = voti/totalevoti;
					        suddivisione = Table[ Sum[ frazioni[[j]], {j,1,i}] , {i,1,Npop} ],
                            criterio === SigmaScaling && votistd > 0,
                            votiscaled = Map[ (N[1 + (# - votimean)/(2*votistd)])&, voti];
                            totalevoti = Plus @@ votiscaled;
                            frazioni = votiscaled/totalevoti;
					        suddivisione = Table[ Sum[ frazioni[[j]], {j,1,i}] , {i,1,Npop} ],
                            criterio === SigmaScaling,
					        totalevoti = Plus @@ voti;
					        frazioni = voti/totalevoti;
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


Generazione[popolazione_List] := Module[{temp, votipop, intervallo, genitori, figli, rr, indice, meanvoti, stdevivoti},

				    votipop = Voti[popolazione];
                    meanvoti = Mean[votipop];
                    stdevivoti = StandardDeviation[votipop];
                    intervallo = Suddivisione[votipop, FitnessProportionate, meanvoti, stdevivoti];
				    genitori = Table[   rr = Random[];
						                indice = Count[ intervallo, x_ /; x< rr ] + 1;
						                popolazione[[indice]],
                                        {i,1,Npop}  ];
				    figli = Ricombina[genitori];
	                figli
	];


(************************************************************************** CROSSOVER **************************************************************************)

(* Comandi senza arg per crossover *)
sensoriNA = {xCS, xTB, xNN};                        (* per MS, MT*)
movimentiNA = {xMS, xMT};                           (* Movimenti *)
comandiTFNA = {xEQ, xNOT, xDU};                     (* per 2° argomento di DU *)
comandiDUarg1NA = Join[movimentiNA, comandiTFNA];   (* per 1° argomento di DU *)
comandiNA = Join[sensoriNA, comandiDUarg1NA];       (* Comandi totali *)

Crossover[ coppia_] := Module[ { i1, i2, risultato, listaparti1, listaparti2, swap, rndcomm1, rndcomm2,
                                 cmdpos1, cmdpos2, cmdheadpos1, cmdheadpos2, headcomm1, headcomm2, newi1, newi2 },

        i1 = coppia[[1]];
        i2 = coppia[[2]];

        (* Effettuo il crossover se il numero casuale è minore della prob. di crossover *)
        If[ Random[] < pc,  (********************************************************* INIZIO IF *********************************************************)
 
            listaparti1 = Union[ Level[i1, {1, Infinity}] ];
            listaparti2 = Union[ Level[i2, {1, Infinity}] ];
            swap = False;

            While[ swap === False,

                (* Prendo una parte a caso 1 *)
                rndcomm1 = listaparti1[[ Random[Integer, {1, Length[listaparti1]}] ]];
                cmdpos1 = Position[i1, rndcomm1, {1, Infinity}, Heads->True];
                cmdpos1 = cmdpos1[[ Random[Integer, {1, Length[cmdpos1]}] ]];
                (* Vedo dentro che comando è contenuta la parte *)
                cmdheadpos1 = cmdpos1;
                cmdheadpos1[[-1]] = 0;
                headcomm1 = i1[[Sequence @@ cmdheadpos1]];

                (* Prendo una parte a caso 2 *)
                rndcomm2 = listaparti2[[ Random[Integer, {1, Length[listaparti2]}] ]];
                cmdpos2 = Position[i2, rndcomm2, {1, Infinity}, Heads->True];
                cmdpos2 = cmdpos2[[ Random[Integer, {1, Length[cmdpos2]}] ]];
                (* Vedo dentro che comando è contenuta la parte *)
                cmdheadpos2 = cmdpos2;
                cmdheadpos2[[-1]] = 0;
                headcomm2 = i2[[Sequence @@ cmdheadpos2]];

                (* Faccio swap solo se sintatticamente corretto *)
                Which[  
                    headcomm1 === headcomm2 && Last[cmdpos1] === Last[cmdpos2], (* I comandi hanno stesse head e posizioni dentro le head *)
                    swap = True,
                    MemberQ[ sensoriNA, rndcomm1[[0]] ] && MemberQ[ sensoriNA, rndcomm2[[0]] ], (* Entrambi i comandi sono sensori *)
                    swap = True,
                    MemberQ[ movimentiNA, rndcomm1[[0]] ] && MemberQ[ movimentiNA, rndcomm2[[0]] ], (* Entrambi i comandi sono movimenti *)
                    swap = True,
                    MemberQ[ comandiTFNA, rndcomm1[[0]] ] && MemberQ[ comandiTFNA, rndcomm2[[0]] ], (* Entrambi i comandi sono comandiTF *)
                    swap = True,
                    headcomm1 === xEQ && headcomm2 === xEQ, (* Entrambe le head sono xEQ *)
                    swap = True,
                    headcomm1 === xNOT && headcomm2 === xNOT, (* Entrambe le head sono xNOT *)
                    swap = True,
                    headcomm1 === xEQ && headcomm2 === xNOT, (* Una head è xEQ e l'altra è xNOT *)
                    swap = True, 
                    headcomm1 === xNOT && headcomm2 === xEQ, (* Una head è xEQ e l'altra è xNOT *)
                    swap = True, 
                    headcomm1 === xDU && Last[cmdpos1] === 2 && MemberQ[ comandiTFNA, rndcomm2[[0]] ], (* Una head è xDU, un comando è in pos 2 e il comando opposto è un comando TF *)
                    swap = True,
                    headcomm2 === xDU && Last[cmdpos2] === 2 && MemberQ[ comandiTFNA, rndcomm1[[0]] ], (* Una head è xDU, un comando è in pos 2 e il comando opposto è un comando TF *)
                    swap = True,
                    headcomm1 === xDU && Last[cmdpos1] === 1 && MemberQ[ Join[movimentiNA, comandiTFNA], rndcomm2[[0]] ]
                        && headcomm2 =!= xDU, (* Una head è xDU e l'altra no, un comando è in pos 1 e il comando opposto è compatibile *)
                    swap = True,
                    headcomm2 === xDU && Last[cmdpos2] === 1 && MemberQ[ Join[movimentiNA, comandiTFNA], rndcomm1[[0]] ]
                        && headcomm1 =!= xDU, (* Una head è xDU, un comando è in pos 1 e il comando opposto è compatibile *)
                    swap = True,        
                    headcomm1 === xDU && headcomm2 === xDU && MemberQ[ comandiTFNA, rndcomm1[[0]] ] && MemberQ[ comandiTFNA, rndcomm2[[0]] ], (* Le head sono xDU e i comandi sono comandi TF *)
                    swap = True,
                    headcomm1 === Hold && (headcomm2 === xEQ || headcomm2 === xNOT) , (* Una delle head è Hold e l'altra è xEQ o xNOT *)
                    swap = True,
                    headcomm2 === Hold && (headcomm1 === xEQ || headcomm1 === xNOT) , (* Una delle head è Hold e l'altra è xEQ o xNOT *)
                    swap = True,
                    True, 
                    swap = False
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
        ];  (********************************************************* FINE IF *********************************************************)

    risultato
];

(************************************************************************** MUTAZIONE **************************************************************************)

Mutazione[individuo_]:= Module[{temp},
		
		        temp = individuo;
                (* Effettuo la mutazione se il numero casuale è minore della prob. di mutazione *)
                If[ Random[] < pm,  temp = MutaIndividuo[temp],  temp   ];
		temp
	];


MutaIndividuo[individuo_]:= Module[{temp, Pos, pos, rndcomm, cmdpos, cmdcont, cmdhead, commands, com, i, listapartitemp},
		
		temp = individuo;
        listapartitemp = Union[ Level[temp, {1, Infinity}] ];

        (* Scelgo un comando tra le parti, vedo la sua posizione e parte iniziale *)
        rndcomm = listapartitemp[[ Random[Integer, {1, Length[listapartitemp]}] ]];
        cmdpos = Position[temp, rndcomm, {1, Infinity}, Heads->True];
        cmdpos = cmdpos[[ Random[Integer, {1, Length[cmdpos]}] ]];
        cmdhead = rndcomm[[0]];
        cmdcont = cmdpos;
        cmdcont[[-1]] = 0;

        (* Vedo di che tipo è la parte iniziale *)
        Which[  temp[[ Sequence @@ cmdcont ]] === Hold,
                commands = comandi,
                MatchQ[Symbol,cmdhead],
                commands = sensori,
                MemberQ[movimentiNA,cmdhead],
                commands = movimenti,
                MemberQ[comandiTFNA,cmdhead],
                commands = comandiTF
            ];

        (***************** Mutazione *****************)
        temp[[ Sequence @@ cmdpos ]] = commands[[ Random[Integer, {1, Length[commands]} ]]];
        initdepth = Depth[temp];

		While[MemberQ[temp, arg, Infinity] && Depth[temp] <= (3+initdepth),
                (* Trovo posizioni degli arg *)
			    Pos = Position[temp, arg];
                (* Sostituisco gli arg *)
			    For[    i=1, i <= Length[Pos], i++,
				        pos = Pos[[i]];
				        pos[[-1]] = 0;

				        (* Correttezza sintattica dei comandi *)
                        Which[  MatchQ[ Part[temp, Sequence @@ pos], xEQ ],
					            com = comandi,
                                MemberQ[ {xMS, xMT}, Part[temp, Sequence @@ pos] ],
						        com = sensori,
                                MatchQ[ Part[temp, Sequence @@ pos], xNOT],
						        com = comandi,
                                (* Assegna argomento 1 e 2 del DU secondo la correttezza sintattica *)
						        Pos[[i]][[Length[Pos[[i]]]]] === 1,
						        com = comandiDUarg1,
						        True,
                                com = comandiTF
                            ];

				        temp[[Sequence @@ Pos[[i]]]] = com[[ Random[Integer, {1, Length[com]} ]]];
			    ];

		];

        If[ MemberQ[temp, arg, Infinity],	
			temp = MutaIndividuo[individuo];
		];

		temp

	];



(*********************************************** RUN ***********************************************)

Npop = 150;
pc = 0.7;
pm = 0.05;
wanted = {u,n,i,v,e,r,s,a,l,e};    (* Parola desiderata *)
stackiniziale = {l,a,u,n,e};
tableiniziale = {s,i,r,v,e};
stackiniziale = {u,n,v};
tableiniziale = {l,a,e,s,i,r,e};
stack = stackiniziale;                    (* Stack *)
table = tableiniziale;                    (* Table *)
listasoluzioni = {};

run := Module[ {pop, voti0, countgen, posbestind, bestind, votoTop},

        Print["Stack iniziale:  ",stack,"   Table iniziale:  ",table];
        countgen = 1;
        pop = PopIniziale;  (* Popolazione iniziale di individui generati casualmente *)
        voti0 = Voti[pop];

        (* Creo generazioni successive finchè non trovo la soluzione *)
        While[  Length[ listasoluzioni ] === 0,
                pop = Generazione[pop];
                voti0 = Voti[pop];
                If[ countgen === 1 || Mod[countgen,5] === 0,
                    Print["Generazione: ", countgen,"    Fitness più alta: ", Max[voti0]];          
                ];
                countgen += 1;                
		];

        stack = stackiniziale;      (* Ripristino stack *)
        table = tableiniziale;      (* Ripristino table *)
        counter = 0;                (* Ripristino counter *)
        counterDUtot = 0;           (* Ripristino counter DU totale *)

        (* Mostro la soluzione *)
        bestind = listasoluzioni[[1]];
        Print["\n"];
        Print["Programma corretto trovato alla generazione ",countgen," :  ",bestind];
        votoTop = Fitness[ bestind ];   
        bestind = SostituisciComandi[bestind];
        TogliHold[ bestind ];
        Print["Fitness: ", votoTop,"    Stack: ",stack];
];

run;