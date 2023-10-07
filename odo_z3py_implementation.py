################################################################################
# SUPPLEMENTARY MATERIAL
#
# PAPER: Unlocking Organizational Potential: Explainable Cooperation and Coordination
# Risks through Logic-Based Dependency Analysis
#
# @ 2024
################################################################################

from z3 import *

s = Solver()


################################################################################
# ------------------------------------------------------------------------------
################################################################################
# ENUMERATED (INTERPRETED) SORTS: Agents, Activities, Evaluations, Goals,  Tasks, 
################################################################################
# ------------------------------------------------------------------------------
################################################################################

################################################################################
# Agents
################################################################################
# C1: City
# C2: Riverine Team
# C3: Water Infrastructure Management
# C4: parks and Recreation

Agent, (C1, C2, C3, C4) = EnumSort('Agent', ('C1', 'C2', 'C3', 'C4'))

################################################################################
# Collective Agents
# Collective(C1)
# Collective(C2)
# Collective(C3)
# Collective(C4)
################################################################################

Collective  = Function('Collective', Agent, BoolSort())

s.add(Collective(C1))
s.add(Collective(C2))
s.add(Collective(C3))
s.add(Collective(C4))


Individual = Function('Individual', Agent, BoolSort())

################################################################################
# Goals
################################################################################
# G1: Riverine Risk Minimized
# G2: Ubran Risk Minimized
# G3: Number of Flood Incident Reports Reduced

Goal, (G0, G1, G2, G3) = EnumSort('Goal', ['G0', 'G1', 'G2', 'G3'])

################################################################################
# States
################################################################################
# S1: Riverine Risk at 350-year Storm Level
# S2: Urban Risk at 100-year Storm Level
# S3: Number of Incident Reports Low

State, (S0, S1, S2, S3) = EnumSort('State', ['S0','S1', 'S2', 'S3'])

################################################################################
# Activity
################################################################################
# A1: Develop Concrete Channel
# A2: Improve Sewer Infrastructure
# A3: Review and Accommodate Sewer Improvements

Activity, (A0, A1, A2, A3) = EnumSort('Activity', ['A0','A1', 'A2', 'A3'])

################################################################################
# Tasks
################################################################################
# T1: Deliver Riverine Risk Mitigation Infrastructure
# T2: Execute Basement Flooding Protection Program

Task, (T0, T1, T2, T3) = EnumSort('Task', ['T0', 'T1', 'T2', 'T3'])

################################################################################
# Evaluations
################################################################################
# E1: Riverine Project Evaluation
# E2: Sewer Project Evaluation

Evaluation, (E1, E2) = EnumSort('Evaluation', ['E1', 'E2'])

################################################################################
# ------------------------------------------------------------------------------
################################################################################
# UNINTERPRETED SORTS
################################################################################
# ------------------------------------------------------------------------------
################################################################################

ActivityCharacteristic = DeclareSort('ActivityCharacteristic')
Incentive = DeclareSort('Incentive')
Operator = DeclareSort('Operator')
Outcome = DeclareSort('Outcome')
PerformanceSpecification = DeclareSort('PerformanceSpecification')
Resource = DeclareSort('Resource')
StateCharacteristic = DeclareSort('StateCharacteristic')
Value = DeclareSort('Value')


################################################################################
# ------------------------------------------------------------------------------
################################################################################
# Sub-SORTS 
################################################################################
# ------------------------------------------------------------------------------
################################################################################
Reward =  Function('Reward', Incentive, BoolSort())
Sanction =  Function('Sanction', Incentive, BoolSort())

ComplexSpecification =  Function('ComplexSpecification', PerformanceSpecification, BoolSort())
AtomicSpecification =  Function('AtomicSpecification', PerformanceSpecification, BoolSort())

ComplexState = Function('ComplexState', State, BoolSort())
AtomicState = Function('AtomicState', State, BoolSort())
ConjunctiveState = Function('ConjunctiveState', State, BoolSort())
DisjunctiveState = Function('DisjunctiveState', State, BoolSort())

outcome  = Function('outcome', State, BoolSort())

################################################################################
# ------------------------------------------------------------------------------
################################################################################
# VARIABLE DECLARATIONS
################################################################################
# ------------------------------------------------------------------------------
################################################################################

activity1 = Const('activity1', Activity)
activity2 = Const('activity2', Activity)
activity3 = Const('activity3', Activity)
activity4 = Const('activity4', Activity)

agent1  = Const('agent1' , Agent)
agent2  = Const('agent2' , Agent)
agent3  = Const('agent3' , Agent)
agent4  = Const('agent4' , Agent)
agent5  = Const('agent4' , Agent)
agent6  = Const('agent4' , Agent)


evaluation1 = Const('evaluation1', Evaluation)
evaluation2 = Const('evaluation2', Evaluation)
evaluation3 = Const('evaluation3', Evaluation)

goal1 = Const('goal1', Goal)
goal2 = Const('goal2', Goal)
goal3 = Const('goal3', Goal)

state1 = Const('state1', State)
state2 = Const('state2', State)
state3 = Const('state3', State)

task1 = Const('task1', Task)
task2 = Const('task2', Task)
task3 = Const('task2', Task)
task4 = Const('task2', Task)

activitycharacteristic1 = Const('activityC=characteristic1', ActivityCharacteristic)
activitycharacteristic2 = Const('activityC=characteristic2', ActivityCharacteristic)

incentive1 = Const('incentive1', Incentive)
incentive2 = Const('incentive2', Incentive)

operator1 = Const('operator1', Operator)
operator2 = Const('operator2', Operator)

outcome1 = Const('outcome1', Outcome)
outcome2 = Const('outcome2', Outcome)

resource1 = Const('resource1', Resource)
resource2 = Const('resource2', Resource)

statecharacteristic1 = Const('statecharacteristic1', StateCharacteristic)

value1 = Const('value1', Value)
value2 = Const('value2', Value)

performancespecification1 = Const('performancespecification1', PerformanceSpecification)
performancespecification2 = Const('performancespecification2', PerformanceSpecification)
performancespecification3 = Const('performancespecification3', PerformanceSpecification)



# ###############################################################################
# ------------------------------------------------------------------------------
# ###############################################################################
# AXIOMS
# ###############################################################################
# ------------------------------------------------------------------------------
# ###############################################################################

# ###############################################################################
# An agent is either an organization or individual
# ###############################################################################
s.add( ForAll (agent1, Or( Collective(agent1 ), Individual(agent1 ) ))) 

# ###############################################################################
# A state is either a complex or an atomic state:
# forall s (State(s) -> ComplexState(s) \/ AtomicState(s)) 
# (16)
# ###############################################################################

s.add( ForAll(state1, Or ( ComplexState(state1), AtomicState(state1)) ))


# ###############################################################################
# Complex states and atomic states are disjoint subclasses of state:
# forall s1, s2(ComplexState(s1) /\ AtomicState(s2) -> ~(s1 = s2) )
# ###############################################################################

s.add( ForAll ( [state1, state2], 
                Implies( And( ComplexState(state1), 
                              AtomicState(state2) ), 
                         Not(state1 == state2)
                         )))

# ###############################################################################
# A complex state can be either conjunctive or disjunctive, which are mutually exclusive:
# forall s ( ComplexState(s) -> ConjunctiveState(s) \/ DisjunctiveState(s) ) (19)
# ###############################################################################
s.add( ForAll (state1, Implies ( ComplexState(state1), 
                                 Or( ConjunctiveState(state1), 
                                     DisjunctiveState(state1) ))))

# ###############################################################################
# conjunctive and disjunctive states are disjoint
# forall s1, s2(ConjunctiveState(s1) /\ DisjunctiveState(s2) -> ~(s1 = s2)) (20)
# ###############################################################################
s.add( ForAll ([state1, state2], 
               Implies ( And( ConjunctiveState(state1), 
                              DisjunctiveState(state2)), Not(state1 == state2))) )

# ###############################################################################
# hasSubState
# subStateOf
# Domain and range constraints
# Irreflexivity constraints
# ###############################################################################
hasSubState = Function('hasSubState', State, State, BoolSort())

subStateOf = Function('subStateOf', State, State, BoolSort())

s.add( ForAll( [state1, state2], 
               Implies(hasSubState(state1, state2), 
                       Not(state1 == state2)
                       )))


s.add( ForAll( [state1, state2], 
               Implies(subStateOf(state1, state2), 
                       Not(state1 == state2)
                       )))

# ###############################################################################
# hasSubState and subStateOf are inverse relations
# ###############################################################################
s.add(ForAll ([state1, state2], Implies(hasSubState(state1, state2), subStateOf(state2, state1))))

s.add(ForAll ([state1, state2], Implies(subStateOf(state2, state1), hasSubState(state1, state2))))
 
# ###############################################################################
# A complex state is a state that has atleast one substate
# ###############################################################################
s.add( ForAll ( [state1, state2], 
                Implies( ComplexState(state1), 
                         Exists( state2, 
                                 And ( hasSubState(state1, state2), 
                                       Not(state1 == state2)))))) 


# ###############################################################################
# An atomic state cannot have a substate:
# ###############################################################################
s.add(ForAll (state1, Implies( AtomicState(state1), Not(Exists(state2, hasSubState(state1, state2))))))

# ###############################################################################
# If a state has a substate, then it cannot be a substate of its substate:
# ###############################################################################
s.add(ForAll ([state1, state2], Implies(hasSubState(state1, state2), Not(subStateOf(state1, state2)) )))


# ###############################################################################
# hasStateCharacteristic 
# stateCharacteristicOf
# hasStateOperator 
# stateOperatorOf 
# hasActivityOperator 
# activityOperatorOf 
# hasStateValue 
# stateValueOf 
# hasPerformanceSpecificationValue 
# performanceSpecificationValueOf 
# ###############################################################################
hasStateCharacteristic = Function('hasStateCharacteristic', State, StateCharacteristic, BoolSort())
stateCharacteristicOf = Function('stateCharacteristicOf ', StateCharacteristic, State, BoolSort())

hasStateOperator = Function('hasStateOperator', State, Operator, BoolSort())
stateOperatorOf = Function('stateOperatorOf', Operator, State, BoolSort())

hasActivityOperator = Function('hasActivityOperator', Activity, Operator, BoolSort())
activityOperatorOf = Function('activityOperatorOf', Operator, Activity, BoolSort())

hasStateValue = Function('hasStateValue', State, Value, BoolSort())
stateValueOf = Function('stateValueOf', Value, State, BoolSort())

hasPerformanceSpecificationValue = Function('hasPerformanceSpecificationValue', PerformanceSpecification, Value, BoolSort())
performanceSpecificationValueOf = Function('performanceSpecificationValueOf', Value, PerformanceSpecification, BoolSort())


# ###############################################################################
# hasStateCharacteristic and stateCharacteristicOf are inverse relations
# ###############################################################################
s.add(ForAll([state1, statecharacteristic1], Implies(hasStateCharacteristic(state1, statecharacteristic1), stateCharacteristicOf (statecharacteristic1, state1))))

s.add(ForAll([state1, statecharacteristic1], Implies(stateCharacteristicOf (statecharacteristic1, state1), hasStateCharacteristic(state1, statecharacteristic1), )))


# ###############################################################################
# hasOperator and operatorOf are inverse relations
# ###############################################################################
s.add( ForAll( [state1, operator1], Implies( hasStateOperator(state1, operator1), stateOperatorOf (operator1, state1))))

s.add( ForAll( [state1, operator1], Implies( stateOperatorOf (operator1, state1), hasStateOperator(state1, operator1))))

s.add( ForAll( [activity1, operator1], Implies( hasActivityOperator(activity1, operator1), activityOperatorOf (operator1, activity1))))

s.add( ForAll( [activity1, operator1], Implies( activityOperatorOf (operator1, activity1), hasActivityOperator(activity1, operator1))))


# ###############################################################################
# hasValue and valueOf are inverse relations
# ###############################################################################
s.add(ForAll([state1, value1], Implies(hasStateValue(state1, value1), stateValueOf (value1, state1))))

s.add(ForAll([state1, value1], Implies(stateValueOf (value1, state1), hasStateValue(state1, value1), )))


# ###############################################################################
# An atomic state is defined in terms of a characteristic, operator, and value:
# ###############################################################################
s.add( ForAll ( state1, 
                Implies ( AtomicState(state1), 
                          Exists( [statecharacteristic1, operator1, value1], 
                                  And( hasStateCharacteristic(state1, statecharacteristic1),  
                                       And( hasStateOperator(state1, operator1), 
                                            hasStateValue(state1, value1) ) ) ) ) ) ) 


# ###############################################################################
# hasMember
# Domain and range constraints
# irreflexivity
# ###############################################################################

hasMember = Function('hasMember', Agent, Agent, BoolSort())

s.add( ForAll ([agent1, agent2], Implies(hasMember(agent1, agent2), Collective(agent1))))

s.add( ForAll( [agent1, agent2], 
               Implies(hasMember(agent1, agent2), 
                       Not(agent1 == agent2)
                       )))

# ###############################################################################
# memberOf
# Domain and range constraints
# Irreflexivity constraint
# ###############################################################################
memberOf = Function('memberOf', Agent, Agent, BoolSort())

#s.add( ForAll ([agent1, agent2], Implies(memberOf(agent1, agent2), Collective(agent2))))


s.add( ForAll( [agent3, agent4], 
               Implies(memberOf(agent3, agent4), 
                       Not(agent3 == agent4)
                       )))

# ###############################################################################
# hasMember and memberOf are inverse relations :
# forall a1, a2 ( memberOf (a1, a2) <--> hasMember(a2, a1))
# ###############################################################################
s.add( ForAll ( [agent1, agent2], Implies( memberOf(agent1, agent2), hasMember(agent2, agent1) ))) 

s.add( ForAll ( [agent1, agent2], Implies( hasMember(agent1,agent2), memberOf(agent2, agent1) ))) 

# ###############################################################################
# hasTask 
# Domain and range constraints
#
# taskOf
# Domain and range constraints
#
# hasTask and taskOf are inverse relations
#
# NOTE: to speed up model building hasTask is implemented as two different predicates hasTaskAg and taskOfAc
# ###############################################################################

hasTaskAg   = Function('hasTaskAg', Agent, Task, BoolSort())
taskOfAg    = Function('taskOfAg ', Task, Agent, BoolSort())

hasTaskAc   = Function('hasTaskAc', Activity, Task, BoolSort())
taskOfAc   = Function('taskOfAc ', Task, Activity, BoolSort())

s.add( ForAll ( [agent5, task1], Implies( hasTaskAg(agent5, task1), taskOfAg(task1, agent5) ))) 

s.add( ForAll ( [agent6, task1], Implies( taskOfAg(task2, agent6), hasTaskAg(agent6, task2) ))) 

s.add( ForAll ( [activity3, task1], Implies( hasTaskAc(activity3, task1), taskOfAc(task1, activity3) ))) 

s.add( ForAll ( [activity4, task1], Implies( taskOfAc(task2, activity4), hasTaskAc(activity4, task1) ))) 


# ###############################################################################
# hasGoal 
# Domain and range constraints 
#
# goalOf 
# Domain and range constraints 
# ###############################################################################
hasGoal   = Function('hasGoal', Agent, Goal, BoolSort())

goalOf    = Function('goalOf', Goal, Agent, BoolSort())

# ###############################################################################
# hasGoal and goalOf are inverse relations
# ###############################################################################
s.add( ForAll ( [task1, goal1], Implies( hasGoal(agent1, goal1), goalOf(goal1, agent1) ))) 

s.add( ForAll ( [task1, goal1], Implies( goalOf(goal1, agent1), hasGoal(agent1, goal1) ))) 

# ###############################################################################
# Domain and range constraint for towards:
# ###############################################################################
towards    = Function('towards', Task, Goal, BoolSort())

# ###############################################################################
# hasActivity
# Domain and range constraint
#
# activityOf
# Domain and range constraint
# ###############################################################################
hasActivity   = Function('hasActivity', Task, Activity, BoolSort())

activityOf    = Function('activityOf', Activity, Task, BoolSort())

# ###############################################################################
# hasActivity and activityOf are inverse relationships
# ###############################################################################
s.add( ForAll( [activity1, task1], Implies( hasActivity(task1, activity1), activityOf(activity1, task1))))

s.add( ForAll( [activity1, task1], Implies( activityOf(activity1, task1), hasActivity(task1, activity1))))

# ###############################################################################
# hasDesiredState
# Domain and range constraint
# 
# desiredStateOf
# Domain and range constraint
#
# causes
# Domain and range constraint
#
# causedBy
#
# hasEvaluatee 
# Domain and range constraint
#
# hasTarget
# Domain and range constraint
#
# hasSubject
# Domain and range constraint
#
# hasEvaluator
# Domain and range constraint
#
# dependsOn 
# Domain and range constraint
# Irreflexivity constraint
#
# strategicComplements
# Domain and range constraint
# Irreflexivity constraint
#
# performedBy
# Domain and range constraint
#
# activityOf 
# Domain and range constraint
#
# enables
# Domain and range constraint
#
# enabledBy
# Domain and range constraint
# ###############################################################################
hasDesiredState = Function('hasDesiredState', Goal, State, BoolSort())
desiredStateOf  = Function('desiredStateOf', State, Goal, BoolSort())   
performs        = Function('performs', Agent, Activity, BoolSort())  
causes          = Function('causes', Activity, State, BoolSort())
causedBy        = Function('causedBy', State, Activity,  BoolSort()) 
hasEvaluatee    = Function('hasEvaluatee', Evaluation, Agent, BoolSort())
hasEvaluator = Function('hasEvaluator', Evaluation, Agent, BoolSort())
performedBy  = Function('performedBy', Activity, Agent, BoolSort())
activityOf   = Function('activityOf', Activity, Task, BoolSort())
enables      = Function('enables', State, Activity, BoolSort())
enabledBy    = Function('enabledBy', Activity, State, BoolSort()) 
produces     = Function('produces', Activity, Resource, BoolSort())
producedBy   = Function('producedBy', Resource, Activity, BoolSort()) 
requires     = Function('requires', Activity, Resource, BoolSort())
requiredBy   = Function('requiredBy', Resource, Activity,  BoolSort()) 


# ###############################################################################
# performedBy and  perform are inverse relationships
# ###############################################################################

s.add(ForAll([activity1,  agent1], Implies( performedBy(activity1,  agent1), performs(agent1, activity1))))

s.add(ForAll([activity1,  agent1], Implies( performs(agent1, activity1), performedBy(activity1,  agent1))))


# ###############################################################################
# enables and enabledBy are inverse relationship
# ###############################################################################

s.add(ForAll([state1, activity1], Implies(enables(state1, activity1), enabledBy(activity1, state1))))

s.add(ForAll([state1, activity1], Implies(enabledBy(activity1, state1), enables(state1, activity1))))

# ###############################################################################
# hasDesiredState and desiredStateOf are inverse relations 
# ###############################################################################
s.add( ForAll ( [goal1, state1], Implies( hasDesiredState(goal1, state1), desiredStateOf(state1, goal1) )))

s.add( ForAll ( [goal1, state1], Implies(desiredStateOf(state1, goal1), hasDesiredState(goal1, state1) )))

# ###############################################################################
# causedBy and causes are inverse relationships
# ###############################################################################
s.add(ForAll([activity1, state1], Implies(causes(activity1, state1), causedBy(state1, activity1))))

s.add(ForAll([activity1, state1], Implies(causedBy(state1, activity1), causes(activity1, state1))))

# ###############################################################################
# An agent has at least one task and one goal:
# ###############################################################################

s.add( ForAll ( agent1, Exists([task1], hasTaskAg(agent1, task1))))

s.add( ForAll ( agent1, Exists([goal1], hasGoal(agent1, goal1) )))

# ###############################################################################
# Every task is associated with a goal it is oriented towards:
# ###############################################################################
s.add( ForAll(task1, Exists(goal1, towards(task1, goal1))))

# ###############################################################################
# A goal is defined in terms of an agent which has the goal and a desired state:
# ###############################################################################
s.add( ForAll (goal1, Exists ( [agent1, state1, goal1], And ( goalOf(goal1, agent1), hasDesiredState(goal1, state1) ))))

# ###############################################################################
# An activity is a function, or well-defined pattern of operation that causes a state:
# ###############################################################################
s.add( ForAll (activity1, Exists( state1, causes(activity1, state1) )))

# ###############################################################################
# An outcome is a state that is caused by an activity:
# ###############################################################################
s.add(ForAll([state1, activity1], Implies(causedBy(state1, activity1), outcome(state1))))


# ###############################################################################
# produces and producedBy are inverse relationships
# ###############################################################################
s.add( ForAll([activity1, resource1], Implies(produces(activity1, resource1), producedBy(resource1, activity1))))
s.add( ForAll([activity1, resource1], Implies(producedBy(resource1, activity1), produces(activity1, resource1))))

# ###############################################################################
# requires and requiredBy are inverse relationships
# ###############################################################################
s.add(ForAll([activity1, resource1], Implies(requires(activity1, resource1), requiredBy(resource1, activity1))))
s.add(ForAll([activity1, resource1], Implies(requiredBy(resource1, activity1), requires(activity1, resource1))))


# ###############################################################################
# every resource is produced or required by an activity
# ###############################################################################
s.add( ForAll( resource1, Exists( activity1, Or( requires(activity1, resource1), produces(activity1, resource1)))))

# ###############################################################################
# hasActivityCharacteristic:
# Domain and range constraint
# ###############################################################################
hasActivityCharacteristic = Function('hasActivityCharacteristic', Activity, ActivityCharacteristic, BoolSort())
activityCharacteristicOf = Function('activityCharacteristicOf', ActivityCharacteristic, Activity,  BoolSort())

# ###############################################################################
# hasActivityCharacteristic and activityCharacteristicOf are inverse relations
# ###############################################################################
s.add(ForAll([activity1, activitycharacteristic1], 
             Implies( hasActivityCharacteristic(activity1, activitycharacteristic1), 
                      activityCharacteristicOf(activitycharacteristic1, activity1))))

s.add(ForAll([activity1, activitycharacteristic1], 
             Implies( activityCharacteristicOf(activitycharacteristic1, activity1), 
                      hasActivityCharacteristic(activity1, activitycharacteristic1))))

# ###############################################################################
# Each activity characteristic must be defined as a characteristic of a specific activity:
# ###############################################################################
s.add(ForAll(activitycharacteristic1, Exists(activity1, activityCharacteristicOf (activitycharacteristic1, activity1))))


# ###############################################################################
# Each activity characteristic belongs to a single activity:
# ###############################################################################
s.add(ForAll([activity1, activity2,  activitycharacteristic1], 
             Implies( And(hasActivityCharacteristic(activity1, activitycharacteristic1), 
                          hasActivityCharacteristic(activity2, activitycharacteristic1)), 
                      (activity1 == activity2))))


# ###############################################################################
# hasPerformanceSpecification
# Domain and range constraint
#
# performanceSpecificationOf
# Domain and range constraint
# ###############################################################################
hasPerformanceSpecification = Function('hasPerformanceSpecification', Activity, PerformanceSpecification, BoolSort())

performanceSpecificationOf = Function('performanceSpecificationOf', PerformanceSpecification, Activity, BoolSort()) #TODO


# ###############################################################################
# A performance specification can be complex or atomic:
# ###############################################################################
s.add( ForAll( performancespecification1, 
               Or( ComplexSpecification(performancespecification1), 
                   AtomicSpecification(performancespecification1))))

# ###############################################################################
# Complex and atomic performance specifications are disjoint classes
# ###############################################################################
s.add( ForAll( performancespecification1, 
               Implies( ComplexSpecification(performancespecification1), 
                        Not(AtomicSpecification(performancespecification1)))))

s.add( ForAll( performancespecification1, 
               Implies( AtomicSpecification(performancespecification1), 
                        Not(ComplexSpecification(performancespecification1)))))


# ###############################################################################
# A complex specification has atleast two sub specifications
# Domain and range of hasSubSpec
# Inverse relationships
# ###############################################################################
hasSubSpecification = Function('hasSubSpecification', PerformanceSpecification, PerformanceSpecification, BoolSort())
subSpecificationOf = Function('subSpecificationOf', PerformanceSpecification, PerformanceSpecification, BoolSort())

s.add( ForAll( [performancespecification1, performancespecification2], 
               Implies( hasSubSpecification(performancespecification1, performancespecification2), 
                        Not(performancespecification1 == performancespecification2))))

s.add( ForAll( [performancespecification1, performancespecification2], 
               Implies( subSpecificationOf(performancespecification1, performancespecification2), 
                        Not(performancespecification1 == performancespecification2))))

s.add( ForAll( performancespecification1, 
               Implies( ComplexSpecification(performancespecification1), 
                        Exists( [performancespecification2, performancespecification3], 
                                And( hasSubSpecification(performancespecification1, performancespecification2), 
                                     And( hasSubSpecification(performancespecification1, performancespecification3), 
                                          Not( performancespecification2 == performancespecification3)))))))

s.add(ForAll([performancespecification1, performancespecification2], 
             Implies( hasSubSpecification(performancespecification1, performancespecification2), 
                      subSpecificationOf(performancespecification2, performancespecification1))))

s.add(ForAll([performancespecification1, performancespecification2], 
             Implies( subSpecificationOf(performancespecification2, performancespecification1), 
                      hasSubSpecification(performancespecification1, performancespecification2))))


# ###############################################################################
# An atomic specification is defined in terms of an activity characteristic, operator, and value
# ###############################################################################
s.add( ForAll( performancespecification1, 
             Implies( AtomicSpecification(performancespecification1), 
                      Exists( [activity1, activitycharacteristic1, value1, operator1], 
                              And( hasActivityCharacteristic(activity1, activitycharacteristic1), 
                                   And( hasActivityOperator(activity1, operator1), 
                                        hasPerformanceSpecificationValue(performancespecification1, value1)))))))


# ###############################################################################
# dependsOn 
# domain and range constraint
# ###############################################################################
dependsOnA = Function('dependsOnA', Activity, Activity, BoolSort())

s.add( ForAll( [activity1, activity2], 
               Implies( dependsOnA(activity1, activity2),
                        Not(activity1 == activity2)
                       )))

dependsOnT = Function('dependsOnT', Task, Task, BoolSort())

s.add( ForAll( [task1, task2], 
               Implies( dependsOnT(task1, task2),
                        Not(task1 == task2)
                       )))

# ###############################################################################
# task dependence if activity dependence inference
# ###############################################################################
s.add( ForAll( [activity1, activity2, task1, task2], 
               Implies( And( dependsOnA(activity1, activity2), 
                             And( hasTaskAc(activity1, task1), 
                                  And( hasTaskAc(activity2, task2),
                                       Not(task1 == task2)))), 
                        dependsOnT(task1, task2))))


# ###############################################################################
# contributorTo
# NOTE: to speed up model building contributorTo is implemented as two predicates
# ###############################################################################
contributorToAc = Function('contributorToAc', Agent, Activity, BoolSort())

soleContributorToAc = Function('soleContributorToAc', Agent, Activity, BoolSort())

s.add( ForAll( [agent1, activity1], Implies( performs(agent1, activity1), contributorToAc(agent1, activity1))))

# ###############################################################################
# forall a1, act1, s(performs(a1, act1) /\ causes(act1, s) -> contributorTo((a1, s)))  
# ###############################################################################
contributorToSt = Function('contributorToSt', Agent, State, BoolSort())

soleContributorToSt = Function('soleContributorToSt', Agent, State, BoolSort())

s.add( ForAll( [agent1, activity1, state1], 
               Implies( And( performs(agent1, activity1), 
                             causes(activity1, state1)), 
                        contributorToSt((agent1, state1)))))


# ###############################################################################
# contributorTo
# soleContributorTo
# ###############################################################################

s.add( ForAll( [agent1, activity1], 
               Implies( And( contributorToAc(agent1, activity1), 
                             Not ( Exists( agent2, And( (contributorToAc(agent2, activity1), 
                                                         Not( (agent1 == agent2))
                                                         ))))), 
                        soleContributorToAc(agent1, activity1))
               ))

s.add( ForAll( [agent1, state1], 
               Implies( And( contributorToSt(agent1, state1), 
                             Not ( Exists( agent2, 
                                           And( (contributorToSt(agent2, state1), 
                                                 Not( (agent1 == agent2))
                                                 ))))), 
                        soleContributorToSt(agent1, state1))
               ))



# ###############################################################################
# strategic complements.
# ###############################################################################

strategicComplementsAc = Function('strategicComplementsAc', Activity, Activity, State, BoolSort())

strategicComplementsTsk = Function('strategicComplementsTsk', Task, Task, State, BoolSort())

s.add(ForAll ([activity1, activity2, state1], 
              Implies( strategicComplementsAc(activity1, activity2, state1), 
                       Not(activity1 == activity2))))

s.add(ForAll ([task1, task2, state1], 
              Implies( strategicComplementsTsk(task1, task2, state1), 
                       Not(task1 == task2))))


# ###############################################################################
# strategic substitutes
# ###############################################################################

strategicSubstitutesAc = Function('strategicSubstitutesAc', Activity, Activity, State, BoolSort())

strategicSubstitutesTsk = Function('strategicSubstitutesTsk', Task, Task, State, BoolSort())

s.add( ForAll( [activity1, activity2, state1], 
               Implies( strategicSubstitutesAc(activity1, activity2, state1), 
                        Not(activity1 == activity2))
               ))

s.add( ForAll( [task1, task2, state1], 
               Implies( strategicSubstitutesTsk(task1, task2, state1), 
                        Not(task1 == task2))
               ))



# ###############################################################################
# An evaluation is defined in terms of its evaluatee(s), evaluator(s), and target of evaluation.
# hasEvaluatee
# Domain and range 
#
# hasEvaluator
# Domain and range
#
# hasTarget
#
# hasStateTarget is a subtype of hasTarget where the target must be a state 
#
# hasPerformanceTarget is a subtype of of hasTarget where the target must be a performance specification:
# ###############################################################################

hasEvaluatee = Function('hasEvaluatee', Evaluation, Agent, BoolSort())

hasEvaluator = Function('hasEvaluator', Evaluation, Agent, BoolSort())

hasStateTarget = Function('hasStateTarget', Evaluation, State, BoolSort())

hasPerformanceTarget = Function('hasPerformanceTarget', Evaluation, PerformanceSpecification, BoolSort())

s.add( ForAll( evaluation1, 
               Or( Exists( [state1, agent1, agent2], 
                           And( hasEvaluatee(evaluation1, agent1), 
                                And( hasEvaluator(evaluation1, agent2), 
                                     hasStateTarget(evaluation1, state1)))), 
                   Exists( [performancespecification1, agent1, agent2], 
                           And( hasEvaluatee(evaluation1, agent1), 
                                And( hasEvaluator(evaluation1, agent2), 
                                     hasPerformanceTarget(evaluation1, performancespecification1)))))))


# ###############################################################################
# An evaluation has exactly one target:
# ###############################################################################
s.add( ForAll([evaluation1, state1, state2], 
              Implies( And( hasStateTarget(evaluation1, state1), 
                            hasStateTarget(evaluation1, state2)), 
                       (state1 == state2))))


s.add( ForAll([evaluation1, performancespecification1, performancespecification2], 
              Implies( And( hasPerformanceTarget(evaluation1, performancespecification1), 
                            hasPerformanceTarget(evaluation1, performancespecification2)), 
                       (performancespecification1 == performancespecification2))))
# ###############################################################################
# hasSubject
# Domain and range constraaint
# NOTE: to speed up model building hasSubject is implememented as two distinct predicates
# ###############################################################################
hasStateSubject = Function('hasStateSubject', Evaluation, State, BoolSort())

hasActivitySubject = Function('hasActivitySubject', Evaluation, Activity, BoolSort())

# ###############################################################################
# An incentive is defined in terms of the evaluation that forms the basis of its provision
# ###############################################################################

hasIncentive = Function('hasIncentive', Evaluation, Incentive, BoolSort())

s.add(ForAll(incentive1, Exists(evaluation1, hasIncentive(evaluation1, incentive1))))


# ###############################################################################
# An incentive can either be a reward or sanction
#
# hasIncentive
# Domain and range constraint
# ###############################################################################

s.add(ForAll(incentive1, Or(Reward(incentive1), Sanction(incentive1))))

# ###############################################################################
# Rewards and sanctions are disjoint classes
# ###############################################################################

s.add(ForAll(incentive1, Implies(Reward(incentive1), Not(Sanction(incentive1)))))

s.add(ForAll(incentive1, Implies(Sanction(incentive1), Not(Reward(incentive1)))))


# ###############################################################################
# hasRecipient
# ###############################################################################

hasRecipient = Function('hasRecipient', Incentive, Agent, BoolSort())

recipientOf = Function('recipientOf', Agent, Incentive,  BoolSort())

# ###############################################################################
# hasAdministrator
# ###############################################################################

hasAdministrator = Function('hasAdministrator', Incentive, Agent, BoolSort())

# ###############################################################################
# An evaluation can have at most a single incentive
# ###############################################################################

s.add( ForAll( [evaluation1, incentive1, incentive2], 
               Implies( And( hasIncentive(evaluation1, incentive1), 
                             hasIncentive(evaluation1, incentive2)), 
                        (incentive1 == incentive2))))


# ###############################################################################
# outcomeDependentOn
# Two agents are outcome dependent if they are individual evaluatees of the same evaluation
#
# outcomeDependentOn is symmetric and irreflexive
# ###############################################################################

outcomeDependentOn = Function('outcomeDependentOn', Agent, Agent, Evaluation, BoolSort())

s.add( ForAll( [agent1, agent2, evaluation1], 
               Implies( outcomeDependentOn(agent1, agent2, evaluation1), 
                        Not(agent1 == agent2))))
                 
s.add(ForAll([agent1, agent2, evaluation1], 
             Implies( outcomeDependentOn(agent1, agent2, evaluation1), 
                      outcomeDependentOn(agent2, agent1, evaluation1))))

s.add(ForAll([evaluation1, agent1, agent2], 
             Implies( And( hasEvaluatee(evaluation1, agent1),  
                           And( hasEvaluatee(evaluation1, agent2),
                                Not(agent1 == agent2))), 
                      outcomeDependentOn(agent1, agent2, evaluation1))))


################################################################################
# Two agents are outcome dependent if they are collectively evaluated as an organization (case 2)
################################################################################

s.add( ForAll([evaluation1, agent1, agent2, agent3], 
              Implies( And( hasEvaluatee(evaluation1, agent1), 
                            And( memberOf(agent2, agent1), memberOf(agent3, agent1)
                                 )), 
                       outcomeDependentOn(agent2, agent3, evaluation1) )))


################################################################################
# Agent 1 has a predictive need of Agent 2 if an activity of the former depends on an activity of the latter
################################################################################

predictiveNeed = Function('predictiveNeed', Agent, Agent, Activity, Activity, BoolSort())

s.add( ForAll( [agent1, agent2, activity1, activity2], Implies( predictiveNeed(agent1, agent2, activity1, activity2),
                                          Not(agent1 == agent2))))

s.add( ForAll( [agent1, agent2, activity1, activity2], 
               Implies( And(performs(agent1, activity1), 
                            And( performs(agent2, activity2), 
                                 And( dependsOnA(activity1, activity2), 
                                      Not(agent1 == agent2)))), 
                        predictiveNeed(agent1, agent2, activity1, activity2))))


################################################################################
# Agent 1 is epistemically dependent on Agent 2 if they are reward dependent on them through an evaluation which has a subject that
# generates a predictive need of Agent 2
################################################################################

epistemicallyDependentOn = Function('epistemicallyDependentOn', Agent, Agent, Evaluation, BoolSort())

s.add( ForAll( [agent1, agent2, evaluation1], Implies( epistemicallyDependentOn(agent1, agent2, evaluation1),
                                          Not(agent1 == agent2)
                                          )))

s.add( ForAll([agent1, agent2, activity1, activity2, evaluation1, incentive1], 
              Implies(  And( Not(agent1 == agent2),
                             And( predictiveNeed(agent1, agent2, activity1, activity2),
                                  And( hasEvaluatee(evaluation1, agent1), 
                                       And( hasIncentive(evaluation1, incentive1),
                                            And( hasRecipient(incentive1, agent1),
                                                 Or( hasActivitySubject(evaluation1, activity1), 
                                                     Exists( state1, And( hasStateSubject(evaluation1, state1),
                                                                          causes(activity1, state1))))))))),
                        epistemicallyDependentOn(agent1, agent2, evaluation1))))


# ###############################################################################
# Two agents are reward dependent if they both receive the same incentive that is tied to an evaluation they share as individuals (i.e.
# they are outcome dependent through an evaluation which has an incentive).
# ###############################################################################

rewardDependentOn = Function('rewardDependentOn', Agent, Agent, Evaluation, BoolSort())

s.add( ForAll( [agent1, agent2, evaluation1], Implies( rewardDependentOn(agent1, agent2, evaluation1),
                                          Not(agent1 == agent2)
                                          )))

s.add( ForAll( [evaluation1, incentive1, agent1, agent2], 
               Implies( And( Not(agent1 == agent2),
                             And( outcomeDependentOn(agent1, agent2, evaluation1), 
                                  And( hasIncentive(evaluation1, incentive1), 
                                       And( recipientOf(agent1, incentive1), 
                                            recipientOf(agent2, incentive1)
                                            )))),  
                        And(rewardDependentOn(agent1, agent2, evaluation1), 
                            rewardDependentOn(agent2, agent1, evaluation1) 
                            ))))

# ###############################################################################
# An agent is reward dependent on another agent if they are epistemically dependent on them:
# ###############################################################################

s.add( ForAll( [agent1, agent2, evaluation1], 
               Implies( epistemicallyDependentOn(agent1, agent2, evaluation1), 
                        rewardDependentOn(agent1, agent2, evaluation1))))

# ###############################################################################
# Coordination Needs and Cooperation Risks
# A coordination need between two agents exists when atleast one of them epistemically depends on the other
# ###############################################################################

CoordinationRequirement = Function('CoordinationRequirement', Agent, Agent, BoolSort())

s.add( ForAll([agent1, agent2], Implies( CoordinationRequirement(agent1, agent2), 
                                         Not(agent1 == agent2)
                                         )))

s.add( ForAll( [agent1, agent2], 
               Implies( CoordinationRequirement(agent1, agent2), 
                        Exists( evaluation1, 
                                Or( epistemicallyDependentOn(agent1, agent2, evaluation1), 
                                    epistemicallyDependentOn(agent2, agent1, evaluation1)
                                    )))))

s.add( ForAll( [agent1, agent2], 
               Implies( Exists( evaluation1, 
                                Or( epistemicallyDependentOn(agent1, agent2, evaluation1), 
                                    epistemicallyDependentOn(agent2, agent1, evaluation1)
                                    )),
                        CoordinationRequirement(agent1, agent2))
               ))


# ###############################################################################
# A free-riding risk exists when an agent is an evaluatee of an evaluation that includes other 
# evaluatees yet there is no subject of evaluation that the agent solely contributes to 
# (i.e. sole performer of an activity.
# ###############################################################################

freeRideRisk = Function('freeRideRisk', Agent, Evaluation, BoolSort())

s.add( ForAll( [agent1, agent2, evaluation1], 
               Implies( And( outcomeDependentOn(agent1, agent2, evaluation1),  
                             And( Not( Exists( state1, (
                                 And(hasStateSubject(evaluation1, state1), 
                                     soleContributorToSt(agent1, state1) 
                                     )))), 
                                  Not ( Exists (activity1, 
                                                And (hasActivitySubject(evaluation1, activity1), 
                                                     soleContributorToAc(agent1, activity1)) ))) ), 
               freeRideRisk(agent1, evaluation1))))


# ###############################################################################
# A free-riding risk exists when an agent is an evaluatee of an evaluation that includes 
# another evaluatee where they both have activities that are subjects of evaluation and the 
# activities are strategic substitutes:
# ###############################################################################

s.add( ForAll( [agent1, agent2, activity1, activity2, state1, evaluation1], 
               Implies( And( Not(agent1 == agent2), 
                             And( performs(agent1, activity1), 
                                  And ( performs(agent2, activity2), 
                                        And( hasEvaluatee(evaluation1, agent1), 
                                             And (hasEvaluatee(evaluation1, agent2), 
                                                  And (hasActivitySubject(evaluation1, activity1), 
                                                       And (hasActivitySubject(evaluation1, activity2), 
                                                            strategicSubstitutesAc(activity1, activity2, state1)))))))), 
                        freeRideRisk(agent1, evaluation1))))

# ###############################################################################
# A shirking risk exists when an agent has a task or activity for which there is no evaluation
# ###############################################################################

taskShirkRisk = Function('taskShirkRisk', Agent, Task, BoolSort())

activityShirkRisk = Function('activityShirkRisk', Agent, Activity, BoolSort())

s.add( ForAll( [agent1, activity1], 
               Implies( 
                   And( performs(agent1, activity1), 
                        Not( Exists( evaluation1, And(hasEvaluatee(evaluation1, agent1), 
                                                      And( hasPerformanceTarget(evaluation1, performancespecification1),
                                                           performanceSpecificationOf(performancespecification1, activity1)
                                                           ))))),
                   activityShirkRisk(agent1, activity1)
               )))


s.add( ForAll( [agent1, task1, goal1], 
               Implies( And( hasTaskAg(agent1, task1), 
                             And( towards(task1, goal1), 
                                  And( hasDesiredState(goal1, state1), 
                                       Not( Exists (evaluation1,  
                                           And( hasEvaluatee(evaluation1, agent1), 
                                                hasStateTarget(evaluation1, state1)
                                                )))))), 
                        taskShirkRisk(agent1, task1)
                        )))



# ###############################################################################
# A sub-goal optimization risk exists when... 
# ###############################################################################

subGoalOptRisk = Function('subGoalOptRisk', Agent, Agent, State, BoolSort())

s.add( ForAll([agent1, agent2, state1], 
              Implies( subGoalOptRisk(agent1, agent2, state1), 
                       Not(agent1 == agent2))))


s.add(ForAll([agent1, agent2, activity1, activity2, goal1, evaluation1, evaluation2, state1, state2, state3], 
             Implies( And( Not(agent1 == agent2), 
                           And( hasDesiredState(goal1, state3), 
                                And( performs(agent1, activity1), 
                                     And( performs(agent2, activity2),
                                          And( causes(activity1, state1), 
                                               And( causes(activity2, state2),
                                                    And( strategicComplementsAc(activity1, activity2, state3),
                                                         And( hasEvaluatee(evaluation1, agent1),
                                                              And( hasEvaluatee(evaluation2, agent2), 
                                                                   And( hasStateTarget(evaluation1, state1),
                                                                        And( hasStateTarget(evaluation2, state2),
                                                                             Not( Exists( evaluation3, (hasStateTarget(evaluation3, state3))))))))))))))),
                      subGoalOptRisk(agent1, agent2, state3))))



# ###############################################################################
# CooperationRisk 
# ###############################################################################
CooperationRisk = Function('CooperationRisk', Agent, Agent, BoolSort())

s.add( ForAll( [agent1, agent2], 
               Implies( CooperationRisk (agent1, agent2), 
                        Not(agent1 == agent2)))) 

# ###############################################################################
# A cooperation risk can occur between the evaluatees of an evaluation when at least one of the evaluatees has a free-riding risk
# ###############################################################################

s.add( ForAll( [ agent1, agent2, evaluation1],
               Implies( And( hasEvaluatee(evaluation1, agent1), 
                             And( hasEvaluatee(evaluation1, agent2),
                                  Or( freeRideRisk(agent1, evaluation1), freeRideRisk(agent2, evaluation1)))),
                CooperationRisk(agent1, agent2))))

s.add( ForAll( [agent1, agent2, agent3, evaluation1],
               Implies( And( hasEvaluatee(evaluation1, agent3),
                             And( memberOf(agent1, agent3),
                                  And( memberOf(agent2, agent3),
                                       Or( freeRideRisk(agent1, evaluation1), freeRideRisk(agent2, evaluation1))))), 
                        CooperationRisk(agent1, agent2))))

# ###############################################################################
# A cooperation risk can exist when one agent epistemically depends on another, 
# yet the latter can shirk on their efforts that the former’s efforts depends on.
# ###############################################################################

s.add( ForAll( [agent1, agent2, evaluation1, state1, activity1], 
               Implies( And( epistemicallyDependentOn(agent1, agent2, evaluation1), 
                             And( hasStateSubject(evaluation1, state1),
                                  activityShirkRisk(agent2, activity1)
                                  )), 
                        CooperationRisk(agent1, agent2))
               ))


# ###############################################################################
# A cooperation risk can exist between two agents when there is a risk of sub-goal optimization between theme
# ###############################################################################

subGoalOptRisk = Function('subGoalOptRisk', Agent, Agent, State, BoolSort())

s.add( ForAll([agent1, agent2, state1], 
              Implies( subGoalOptRisk(agent1, agent2, state1), 
                       Not(agent1 == agent2)))) 

s.add( ForAll([agent1, agent2, state1], 
              Implies( And( Not(agent1 == agent2), 
                            subGoalOptRisk(agent1, agent2, state1)), 
                       CooperationRisk(agent1, agent2))))

# ###############################################################################
# A cooperation risk is symmetric
# ###############################################################################

s.add(ForAll([agent1, agent2], 
             Implies( CooperationRisk(agent1, agent2), 
                      CooperationRisk(agent2, agent1))))


# ###############################################################################
# A cooperation risk is non-reflexive
# ###############################################################################

s.add( ForAll( [agent1, agent2], Implies( CooperationRisk( agent1, agent2), 
                                          Not (agent1 == agent2)
                                          )))


################################################################################
# ASSERTIONS
################################################################################
# C1: City
# C2: Riverine Team
# C3: Water Infrastructure Management
# C4: Parks and Recreation
# G1: Riverine Risk Minimized
# G2: Ubran Risk Minimized
# G3: Number of Flood Incident Reports Reduced
# G0: Disruption to Park Minimized
# S1: Riverine Risk at 350-year Storm Level
# S2: Urban Risk at 100-year Storm Level
# S3: Number of Incident Reports Low
# S0: Park Always at least Partially Open
# A1: Develop Concrete Channel
# A2: Improve Sewer Infrastructure
# A3: Review and Accommodate Sewer Improvements
# T3: Execute Infrastructure Review Process
# T1: Deliver Riverine Risk Mitigation Infrastructure
# T2: Deliver Basement Flooding Protection Program
# T0: Oversee Flood Risk Mitigation Efforts
# E1: Riverine Project Evaluation
# E2: Sewer Project Evaluation

s.add(hasMember(C1,C2))
s.add(hasMember(C1,C3))
s.add(hasMember(C1,C4))

s.add(hasTaskAg(C1,T0))
s.add(hasTaskAg(C2,T1))
s.add(hasTaskAg(C3,T2))
s.add(hasTaskAg(C4,T3))

s.add( hasGoal(C2, G1) )
s.add( hasGoal(C3, G2) )
s.add( hasGoal(C1, G3) )
s.add( hasGoal(C4, G0) )

s.add(towards(T0,G3))
s.add(towards(T1,G1))
s.add(towards(T2,G2))
s.add(towards(T3,G0))

s.add(hasActivity(T1, A1))
s.add(hasActivity(T2, A2))
s.add(hasActivity(T3, A3))

s.add(hasDesiredState(G1,S1))
s.add(hasDesiredState(G2,S2))
s.add(hasDesiredState(G3,S3))
s.add(hasDesiredState(G0,S0))

s.add(performs(C2,A1))
s.add(performs(C3,A2))
s.add(performs(C4,A2))

s.add(causes(A1,S1))
s.add(causes(A2,S2))

s.add(dependsOnA(A1,A2))
s.add(dependsOnA(A2,A1))
s.add(dependsOnA(A2,A3))


s.add(hasEvaluatee(E1,C2))
s.add(hasEvaluatee(E2,C3))

s.add(hasStateTarget(E1,S1))
s.add(hasStateTarget(E2,S2))
s.add(hasActivitySubject(E1,A1))
s.add(hasActivitySubject(E2,A2))
s.add(hasEvaluator(E2,C1))
s.add(hasEvaluator(E1,C1))

s.add(strategicComplementsAc(A1,A2,S3))


s.add(ForAll([agent1, agent2], 
             Implies( memberOf(agent1, agent2), 
                      And( (agent2 == C1), 
                           Or( (agent1 == C2), 
                               Or( (agent1 == C3), 
                                   (agent1 == C4)))))))

s.add(ForAll([agent1, goal1], 
             Implies( hasGoal(agent1, goal1), 
                      Or( And( (agent1 == C1), (goal1 == G3)),
                          Or( And( (agent1 == C2), (goal1 == G1)),
                              Or ( And( (agent1 == C3), (goal1 == G2)),
                                   And( (agent1 == C4), (goal1 == G0))))))))

s.add(ForAll([task1, goal1], 
             Implies( towards(task1, goal1), 
                      Or( And( (task1 == T1), (goal1 == G1)),
                          Or ( And( (task1 == T2), (goal1 == G2)),
                               Or( And( (task1 == T3), (goal1 == G0)),
                                   And( (task1 == T0), (goal1 == G3))))))))

s.add(ForAll([task1, activity1], 
             Implies( hasActivity(task1, activity1), 
                      Or( And( (task1 == T1), (activity1 == A1)),
                          Or( And( (task1 == T2), (activity1 == A2)),
                              And( (task1 == T3), (activity1 == A3)))))))

s.add(ForAll([evaluation1, state1], 
             Implies( hasStateTarget(evaluation1, state1), 
                      Or( And( (evaluation1 == E1), (state1 == S1)),
                          And( (evaluation1 == E2), (state1 == S2))))))


s.add(ForAll([activity1, activity2, state1], 
             Implies( strategicComplementsAc(activity1, activity2, state1), 
                      And( (activity1 == A1), 
                           And( (activity2 == A2), 
                                (state1 == S3))))))

s.add(ForAll([agent1, task1], 
             Implies( hasTaskAg(agent1, task1), 
                      Or( And( (agent1 == C1), (task1 == T0)),
                          Or( And( (agent1 == C2), (task1 == T1)),
                              Or( And( (agent1 == C3), (task1 == T2)),
                                  And( (agent1 == C4), (task1 == T3))))))))

s.add(ForAll([agent1, agent2], 
             Implies( hasMember(agent1, agent2), 
                      And( (agent1 == C1), 
                           Or( (agent2 == C2), 
                               Or( (agent2 == C3), 
                                   (agent2 == C4)))))))

s.add(ForAll([evaluation1, agent1], 
             Implies( hasEvaluatee(evaluation1, agent1), 
                      Or( And( (evaluation1 == E1), (agent1 == C2)),
                          And( (evaluation1 == E2), (agent1 == C3))))))


s.add(ForAll([evaluation1, agent1], 
             Implies( hasEvaluator(evaluation1, agent1), 
                      Or( And( (evaluation1 == E1), (agent1 == C1)),
                          And( (evaluation1 == E2), (agent1 == C1))))))


s.add(ForAll([task1, goal1], 
             Implies( hasDesiredState(goal1, state1), 
                      Or( And( (goal1 == G1), (state1 == S1)),
                          Or ( And( (goal1 == G2), (state1 == S2))),
                               Or( And( (goal1 == G0), (state1 == S0))),
                                   And( (goal1 == G3), (state1 == S3))))))


    
if bool(s.check()):
    print('solver check: ', s.check())
    model = s.model()
    print(model)
    

