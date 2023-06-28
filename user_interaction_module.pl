:- module(user_interaction_module, [askif/1, askifnot/1, start/0, askfor/1]).
:- dynamic(asked/2).
:- dynamic user_answers/1.

accepted_answer(_,A) :- affirmative(A).
accepted_answer(Qcode,A) :- \+(negative(A)), \+(affirmative(A)), \+(interrogative(A)),
                            write('   invalid answer: reply with y, n or w.'), 
                            read(A2), retract(asked(Qcode,A)), assertz(asked(Qcode,A2)), affirmative(A2).

interrogative_answer(Qcode,A) :- interrogative(A),
                            questioncode(Qcode, _, E),
                            write(E), read(A2), retract(asked(Qcode,A)), assertz(asked(Qcode,A2)), affirmative(A2).

ask(Qcode,A) :- asked(Qcode,A).
ask(Qcode,A) :- \+(asked(Qcode,A)), 
                questioncode(Qcode,Q,_), write(Q), write('? '), read(A2), ask2(Q,Qcode,A2,A).

ask2(_,Qcode,A,A) :-  assertz(asked(Qcode,A)).

askif(Q) :- ask(Q,A), accepted_answer(Q,A).
askif(Q) :- ask(Q,A), interrogative_answer(Q,A).
askifnot(Q) :- not(askif(Q)).

askinfo(Question, Answer) :- write(Question), read(Answer).

affirmative(yes).
affirmative(y).
affirmative(yup).
affirmative(ok).

negative(no).
negative(n).
negative(not).
negative(never).
negative(nope).

interrogative(w).
interrogative(why).

printfacts :- nl, write('   This is how I got the diagnosis:'), nl, forall(asked(Q, A), print_fact(Q, A)).
print_fact(Q, A) :- format('        To the question: ~w,     you replied: ~w~n', [Q, A]).

reset :-
    retractall(asked(_, _)),
    retractall(diagnosis_module:power_supply(_)),
    retractall(diagnosis_module:cpu(_,_)),
    retractall(diagnosis_module:gpu(_)),
    retractall(diagnosis_module:ram(_)),
    retractall(diagnosis_module:mobo(_,_,_)),
    retractall(diagnosis_module:diagnosis(_)).

start :-
    consult(diagnosis_module),
    nl,
    write('Welcome to FAULT-FINDER!'), nl,
    write('Let me ask some questions about your pc-related problem:'), nl, nl,
    diagnosis_module:diagnosis(Diagnosis),
    nl, nl,
    format('DIAGNOSIS: ~w~n', [Diagnosis]),
    printfacts,
    reset.

askfor(power_supply) :-
    askinfo('   What is the wattage of the power supply (watts)? ', Wattage),
    assertz(diagnosis_module:power_supply(Wattage)),
    assertz(asked('psu_wattage',Wattage)).

askfor(cpu) :-
    askinfo('   What is the manifacturer of the CPU (intel or amd)? ', Manifacturer),
    askinfo('   What is the TDP of the CPU (watts)? ', Tdp),
    assertz(diagnosis_module:cpu(Manifacturer, Tdp)),
    assertz(asked('cpu_manifacturer', Manifacturer)),
    assertz(asked('cpu_tpd',Tdp)).

askfor(gpu) :-
    askinfo('   What is the TDP of the graphics card (watts)? ', Tdp),
    assertz(diagnosis_module:gpu(Tdp)),
    assertz(asked('gpu_tdp',Tdp)).

askfor(ram) :-
    %askinfo('What is the capacity of RAM (in GB)? ', Capacity),
    % askinfo('What is the speed of RAM? ', Speed),
    askinfo('   What is the type of RAM? (ddr?) ', Type),
    assertz(diagnosis_module:ram(Type)), %ram(Type, Capacity, Speed)
    assertz(asked('ram_type',Type)).

askfor(hdd) :-
    askinfo('   How old is the hard drive? (years)', Age),
    assertz(diagnosis_module:hdd(Age)),
    assertz(asked('hdd_age',Age)).

% askfor(ssd) :-
%     askinfo('How old is the solid state drive?', Age),
%     assertz(diagnosis_module:ssd(Age)).

askfor(mobo) :-
    askinfo('   What cpus can the motherboard run (intel or amd)', Cpu_comp),
    askinfo('   What type of RAM can the motherboard read (ddr?)', Ram_comp),
    askinfo('   What is the TDP of the motherboard (watts)?', Tdp),
    assertz(diagnosis_module:mobo(Cpu_comp, Ram_comp, Tdp)),
    assertz(asked('mobo_cpu_comp',Cpu_comp)),
    assertz(asked('mobo_ram_comp',Ram_comp)),
    assertz(asked('mobo_tdp',Tdp)).
