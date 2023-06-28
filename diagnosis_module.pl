:- module(diagnosis_module, [diagnosis/1, questioncode/3]).
:- use_module(user_interaction_module).
:- dynamic(diagnosis/1).

/* Top-level diagnosis rules */
diagnosis('PSU Turned OFF.') :- power_problem_current, askifnot(psu_on).
diagnosis('PSU not plugged in.') :- power_problem_current, askif(psu_on), askifnot(just_plugged).
diagnosis('Insufficient PSU wattage.') :- askif(pc_dead), psu_problem, askif(crashes), askif(more_info), askfor(power_supply), askfor(cpu), askfor(gpu), askfor(mobo),
                                            cpu(_,Cpu_wattage), gpu(Gpu_wattage), power_supply(Psu_wattage), mobo(_,_,Tdp), 
                                            Required_wattage is Cpu_wattage + Gpu_wattage + Tdp + 50,
                                            Psu_wattage < Required_wattage.
diagnosis('Incompatible RAM.') :- askifnot(pc_dead), askif(lights_turn_on), askif(more_info), askfor(mobo), askfor(ram), 
                                    ram(Type), mobo(_,Ram_comp,_), dif(Type, Ram_comp).  %The dif/2 predicate is a constraint that is true if and only if A and B are different terms. 
                                                                                         %If A and B can never unify, dif/2 succeeds deterministically. If A and B are identical, it fails immediately.
diagnosis('Incompatible CPU.') :- askifnot(pc_dead), askifnot(lights_turn_on), askif(more_info), askfor(mobo), askfor(cpu), 
                                    cpu(Manifacturer,_), mobo(Cpu_comp,_,_), dif(Manifacturer, Cpu_comp).
diagnosis('CPU thermal throttle, insufficient cooling.') :- askif(pc_dead), slow_performance, cpu_problem.
diagnosis('GPU thermal throttle, insufficient cooling.') :- askif(pc_dead), slow_performance, gpu_problem.
diagnosis('Old HDD, You need to replace it.') :- askif(pc_dead), slow_performance, mechanical_problem_hdd, askif(more_info), askfor(hdd), hdd(Hdd_age), Hdd_age>4.
diagnosis('Damaged HDD, may require replacement.') :- askif(pc_dead), mechanical_problem_hdd.
diagnosis('Debris or cables between fan blades.') :- askif(pc_dead), mechanical_problem_fans.
diagnosis('Some cables are unplugged, check every internal cable.') :- power_problem_current, askif(psu_on), askif(just_plugged).
diagnosis('Out-of-date BIOS.') :- askif(new_component).
diagnosis('I am sorry. I cannot find any problem with the information you provided.').

/* Definitions of intermediate predicates */                            
power_problem_current :- askifnot(pc_dead).                             
mechanical_problem_hdd :- askif(has('an HDD')), askif(hear('whiny noise from the HDD')).
mechanical_problem_fans :- askif(hear('weird noise from the fans')).
slow_performance :- askif(slow_pc).
cpu_problem :- askif(cpu_thermal).
gpu_problem :- askif(gpu_thermal).
psu_problem :- askif(crashes).

/* Question decoding */
questioncode(more_info, 'Are you able to provide some more specific info about components','  your problem could be related to some specific features of your components').
questioncode(pc_dead, 'Does the pc turn on','   this is the most basic question in order to discriminate between problems').
questioncode(cpu_thermal, 'Does the CPU run near or more than 85C','  sustained cpu temps near or more than 85C are not good, best if under 79C').
questioncode(gpu_thermal, 'Does the GPU run near or more than 85C','  sustained gpu temps near or more than 85C are not good, best if under 79C').
questioncode(slow_pc, 'Does the PC run slow','  if it runs slow there likely is a problem somewhere').
questioncode(psu_on, 'Is the ON/OFF switch of the PSU set to ON','  maybe you just forgot to turn the PSU on').
questioncode(just_plugged, 'Did you plug the PC in','   maybe you just forgot to plug it in').
questioncode(has(X), X,'I need to know if you have this component to include it in the diagnosis') :- write('Does the PC have ').
questioncode(hear(X), X,'I need to knwo if you hear this kind of noise to discriminate between problems') :- write('Do you hear a ').
questioncode(lights_turn_on, 'Do the lights turn on, even for a second','    to understand if current flows in your sys').
questioncode(crashes, 'Does the PC crashes often while under load','    to understand if the PC creshes when it draws more power').
questioncode(new_component, 'Do you have recently installed a new component','  new components generally need some software update').